;;;========================
;;; MSCX HELPERS (HACK V1)
;;;========================

;;
;;
;; using parts from OMs export-mxml.lisp
;;

(defpackage "MSCX" 
  (:use "COMMON-LISP")
  (:use "MusicXML")
  (:use :om)
  (:nicknames :mscx))

(in-package :mscx)

(pushnew :mscx *features*)

(defvar *xml-version* "XML 1.0")

(defun mscx-header ()
  (list "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"))

(defparameter *mscx-tpc-table*
  '(("Cbb" . 0) ("Gbb" . 1) ("Dbb" . 2) ("Abb" . 3) ("Ebb" . 4) ("Bbb" . 5)
    ("Fb" . 6) ("Cb" . 7) ("Gb" . 8) ("Db" . 9) ("Ab" . 10) ("Eb" . 11)
    ("Bb" . 12) ("F" . 13) ("C" . 14) ("G" . 15) ("D" . 16) ("A" . 17)
    ("E" . 18) ("B" . 19) ("F#" . 20) ("C#" . 21) ("G#" . 22) ("D#" . 23)
    ("A#" . 24) ("E#" . 25) ("B#" . 26) ("F##" . 27) ("C##" . 28) ("G##" . 29)
    ("D##" . 30) ("A##" . 31) ("E##" . 32) ("B##" . 33)))

(defun alteration->acc-string (alteration)
  (case alteration
    ((nil 0) "")
    (-2 "bb")
    (-1 "b")
    (1 "#")
    (2 "##")
    (t "")))

(defun step+alter-to-tpc (step alteration)
  (or (cdr (assoc (format nil "~A~A" step (alteration->acc-string alteration))
                  *mscx-tpc-table*
                  :test #'string=))
      14))

(defun om-midic-to-midi (midic)
  "e.g. 6000 -> midi 60."
  (round (/ midic 100)))

(defun xml-head-to-mscx-duration-type (note-head)
  "Current mxml::*note-types* gives MusicXML type names. For this hack we reuse them."
  note-head)

(defun clef-sign->mscx-clef (sign)
  (string-upcase (string sign)))

(defun ratio-base-note-name (dur)
  "Map a duration unit to a MuseScore baseNote string.
Hack v1: reuse the same duration naming logic as for Chord/Rest."
  (let* ((head-and-pts (mxml::get-head-and-points dur))
         (note-head (cadr (find (car head-and-pts) mxml::*note-types* :key 'car))))
    note-head))

(defun ratio->mscx-fraction-string (r)
  (cond ((integerp r) (format nil "~D" r))
        ((rationalp r) (format nil "~D/~D" (numerator r) (denominator r)))
        (t (format nil "~A" r))))

(defun mscx-actual-fraction-dur (self free)
  "Real timeline duration for tie/spanner location.
FREE is the written duration used for durationType."
  (let* ((written (if (listp free) (car free) free))
         (ratio (mxml::time-mod-val self)))
    (if (and ratio (not (= (car ratio) (cadr ratio))))
        (* written (/ (cadr ratio) (car ratio)))   ;; normal / actual
      written)))

(defun current-mscx-dur (free)
  (if (listp free) (car free) free))


;;
;; TUPLETS
;;

(defun make-mscx-tuplet-start (actual normal base-note)
  (list "<Tuplet>"
        (format nil "<normalNotes>~D</normalNotes>" normal)
        (format nil "<actualNotes>~D</actualNotes>" actual)
        (format nil "<baseNote>~A</baseNote>" base-note)
        "<Number>"
        "<style>tuplet</style>"
        (format nil "<text>~D</text>" actual)
        "</Number>"
        "</Tuplet>"))

(defun make-mscx-tuplet-end ()
  (list "<endTuplet/>"))


;;
;; TIES
;; 

(defun tied-kind (self)
  (cond ((and (not (om::cont-chord-p self))
              (om::cont-chord-p (om::next-container self '(om::chord))))
         :start)
        ((and (om::cont-chord-p self)
              (not (om::cont-chord-p (om::next-container self '(om::chord)))))
         :stop)
        ((and (om::cont-chord-p self)
              (om::cont-chord-p (om::next-container self '(om::chord))))
         :continue)
        (t nil)))

(defun same-measure-p (a b)
  (and a b
       (eq (mxml::get-parent-measure a)
           (mxml::get-parent-measure b))))

(defun next-chord (self)
  (om::next-container self '(om::chord)))

(defun prev-chord (self)
  (om::previous-container self '(om::chord)))

(defun obj-written-dur (self)
  "Return written note value as a whole-note fraction."
  (let* ((mesure (mxml::get-parent-measure self))
         (tree (om::tree mesure))
         (real-beat-val (/ 1 (om::fdenominator (first tree))))
         (symb-beat-val (/ 1 (om::find-beat-symbol (om::fdenominator (first tree)))))
         (dur-obj-noire (/ (om::extent self) (om::qvalue self)))
         (factor (/ (* 1/4 dur-obj-noire) real-beat-val)))
    (* symb-beat-val factor)))

(defun obj-actual-dur (self)
  (let ((written (obj-written-dur self))
        (ratio (ignore-errors (time-mod-val self))))
    (if (and ratio
             (listp ratio)
             (= (length ratio) 2)
             (numberp (car ratio))
             (numberp (cadr ratio))
             (not (= (car ratio) (cadr ratio))))
        (* written (/ (cadr ratio) (car ratio)))
      written)))

(defun mscx-next-location (self free)
  (declare (ignore free))
  (let ((nxt (next-chord self)))
    (if (same-measure-p self nxt)
        (list "<location>"
              (format nil "<fractions>~A</fractions>"
                      (ratio->mscx-fraction-string (obj-actual-dur self)))
              "</location>")
      (list "<location>"
            "<measures>1</measures>"
            "</location>"))))

(defun mscx-prev-location (self free)
  (declare (ignore free))
  (let ((prv (prev-chord self)))
    (if (same-measure-p self prv)
        (list "<location>"
              (format nil "<fractions>-~A</fractions>"
                      (ratio->mscx-fraction-string (obj-actual-dur prv)))
              "</location>")
      (list "<location>"
            "<measures>-1</measures>"
            "</location>"))))

(defun mscx-tie-spanner (self free)
  (let ((kind (tied-kind self)))
    (case kind
      (:start
       (list "<Spanner type=\"Tie\">"
             "<Tie/>"
             "<next>"
             (mscx-next-location self free)
             "</next>"
             "</Spanner>"))

      (:stop
       (list "<Spanner type=\"Tie\">"
             "<prev>"
             (mscx-prev-location self free)
             "</prev>"
             "</Spanner>"))
      (:continue
       (append
        (list "<Spanner type=\"Tie\">"
              "<prev>"
              (mscx-prev-location self free)
              "</prev>"
              "</Spanner>")
        (list "<Spanner type=\"Tie\">"
              "<Tie/>"
              "<next>"
              (mscx-next-location self free)
              "</next>"
              "</Spanner>")))
      (otherwise nil))))

;;
;; main methods for #'cons-mscx-expr for relevant OM classes
;;

(defgeneric cons-mscx-expr (self &key free key approx part))

(defmethod cons-mscx-expr ((self om::chord) &key free key (approx 2) part)
  (let* ((dur (if (listp free) (car free) free))
         (head-and-pts (mxml::get-head-and-points dur))
         (note-head (cadr (find (car head-and-pts) mxml::*note-types* :key 'car)))
         (nbpoints (cadr head-and-pts))
         (inside (om::inside self)))
    (append
     ;; take care to emit correct list order, which decides semantics in output
     (list "<Chord>")
     (loop for i from 1 to nbpoints
           collect "<dots>1</dots>")
     (list (format nil "<durationType>~A</durationType>" (xml-head-to-mscx-duration-type note-head)))
     (loop for note in inside
           append
           (let* ((note-values (mxml::mc->xmlvalues (om::midic note) approx))
                  (step (nth 1 note-values))
                  (alteration (nth 2 note-values))
                  (midi (om-midic-to-midi (om::midic note)))
                  (tpc (step+alter-to-tpc step alteration))
                  (vel (om::vel note)))
             
	     ;; (list "<Note>"
             ;;       (format nil "<pitch>~D</pitch>" midi)
             ;;       (format nil "<tpc>~D</tpc>" tpc)
             ;;       (format nil "<velocity>~D</velocity>" vel)
             ;;       "</Note>")

	     (append
	      (list "<Note>")
	      (when (mscx-tie-spanner self free)
		(mscx-tie-spanner self free))
	      (list (format nil "<pitch>~D</pitch>" midi)
		    (format nil "<tpc>~D</tpc>" tpc)
		    (format nil "<velocity>~D</velocity>" vel)
		    "</Note>"))))
     (list "</Chord>"))))

(defmethod cons-mscx-expr ((self om::rest) &key free key (approx 2) part)
  (let* ((dur (if (listp free) (car free) free))
         (head-and-pts (mxml::get-head-and-points dur))
         (note-head (cadr (find (car head-and-pts) mxml::*note-types* :key 'car)))
         (nbpoints (cadr head-and-pts)))
    (append
     (list "<Rest>")
     (loop for i from 1 to nbpoints
           collect "<dots>1</dots>")
     (list (format nil "<durationType>~A</durationType>"
                   (xml-head-to-mscx-duration-type note-head)))
     (list "</Rest>"))))

(defmethod cons-mscx-expr ((self om::group) &key free key (approx 2) part)
  (let* ((durtot (if (listp free) (car free) free))
         (cpt (if (listp free) (cadr free) 0))
         (num (or (om::get-group-ratio self) (om::extent self)))
         (denom (om::find-denom num durtot))
         (num (if (listp denom) (car denom) num))
         (denom (if (listp denom) (cadr denom) denom))
         (unite (/ durtot denom)))

    (format t "~&GROUP ratio -> num=~A denom=~A unite=~A~%" num denom unite)    
    
    (cond

      ;; not a tuplet-like group: recurse normally
      ((not (om::get-group-ratio self))
       (loop for obj in (om::inside self) append
             (let* ((dur-obj (/ (/ (om::extent obj) (om::qvalue obj))
                                (/ (om::extent self) (om::qvalue self)))))
               (cons-mscx-expr obj :free (* dur-obj durtot) :approx approx :part part))))

      ;; ratio simplifies away: recurse normally
      ((= (/ num denom) 1)
       (loop for obj in (om::inside self)
             append
             (let* ((operation (/ (/ (om::extent obj) (om::qvalue obj))
                                  (/ (om::extent self) (om::qvalue self))))
                    (dur-obj (* num operation)))
               (cons-mscx-expr obj :free (* dur-obj unite) :approx approx :part part))))

      ;; real tuplet
      (t
       (let ((depth 0)
             (rep nil)
             ;; base note should be the written value of one unit inside the tuplet.
             ;; For 3 eighths in the time of 2 eighths, this should become "eighth".
             (base-note (ratio-base-note-name unite)))
         
         (setf rep (append rep
                           (make-mscx-tuplet-start num denom base-note)))

         (loop for obj in (om::inside self) do
               (setf rep
                     (append rep
                             (let* ((operation (/ (/ (om::extent obj) (om::qvalue obj))
                                                  (/ (om::extent self) (om::qvalue self))))
                                    (dur-obj (* num operation))
                                    (tmp (multiple-value-list
                                          (cons-mscx-expr obj
                                                          :free (list (* dur-obj unite) cpt)
                                                          :approx approx
                                                          :part part)))
                                    (exp (car tmp)))
                               (when (and (cadr tmp) (> (cadr tmp) depth))
                                 (setf depth (cadr tmp)))
                               exp))))

         (setf rep (append rep (make-mscx-tuplet-end)))
         (values rep (+ depth 1)))))))


(defmethod cons-mscx-expr ((self om::measure) &key free (key '(G 2)) (approx 2) part)
  (let* ((mesnum free)
         (inside (om::inside self))
         (tree (om::tree self))
         (signature (car tree))
         (real-beat-val (/ 1 (om::fdenominator signature)))
         (symb-beat-val (/ 1 (om::find-beat-symbol (om::fdenominator signature)))))
    (list
     "<Measure>"
     "<voice>"

     (when (= mesnum 1)
       (remove nil
               (list
                (and key
                     (list "<Clef>"
                           (format nil "<concertClefType>~A</concertClefType>"
                                   (clef-sign->mscx-clef (car key)))
                           (format nil "<transposingClefType>~A</transposingClefType>"
                                   (clef-sign->mscx-clef (car key)))
                           "</Clef>"))
                (list "<TimeSig>"
                      (format nil "<sigN>~D</sigN>" (car signature))
                      (format nil "<sigD>~D</sigD>" (cadr signature))
                      "</TimeSig>"))))

     (loop for obj in inside
           append
           (let* ((dur-obj-noire (/ (om::extent obj) (om::qvalue obj)))
                  (factor (/ (* 1/4 dur-obj-noire) real-beat-val)))
             (cons-mscx-expr obj :free (* symb-beat-val factor)
				 :approx approx :part part)))

     "<BarLine>"
     "<subtype>normal</subtype>"
     "</BarLine>"

     "</voice>"
     "</Measure>")))

(defmethod cons-mscx-expr ((self om::voice) &key free (key '(G 2)) (approx 2) part)
  (let ((voicenum part)
        (measures (om::inside self)))
    (list
     (format nil "<Staff id=\"~D\">" voicenum)
     (loop for mes in measures
           for i = 1 then (+ i 1)
           collect (cons-mscx-expr mes :free i :key key :approx approx :part part))
     "</Staff>")))


(defmethod cons-mscx-expr ((self om::poly) &key free (key '((G 2))) (approx 2) part)
  (let ((voices (om::inside self)))
    (list
     "<museScore version=\"4.60\">"
     "<Score>"

     "<Division>480</Division>"

     ;; Minimal Part declarations
     (loop for v in voices
           for i = 1 then (+ i 1)
           append
           (list
            (format nil "<Part id=\"~D\">" i)
            "<Staff>"
            "<StaffType group=\"pitched\">"
            "<name>stdNormal</name>"
            "</StaffType>"
            "</Staff>"
            (format nil "<trackName>Part ~D</trackName>" i)
            "</Part>"))

     ;; Staff timelines
     (if (= 1 (length key))
         ;; same key for all voices
         (loop for v in voices
               for i = 1 then (+ i 1)
               append
               (cons-mscx-expr v :part i :key (car key) :approx approx))
	 ;; one key per voice
	 (loop for v in voices
               for i = 1 then (+ i 1)
               for k in key
               append
               (cons-mscx-expr v :part i :key k :approx approx)))

     "</Score>"
     "</museScore>")))


;;;===================================
;;; OM INTERFACE / API
;;;===================================


;; MAIN MSCX FILE OUTPUT

(in-package :om)

(defun write-mscx-file (list path)
  (with-open-file (out path :direction :output
			    :if-does-not-exist :create :if-exists :supersede)
    (loop for line in (mscx::mscx-header) do (format out "~A~%" line))
    (recursive-write-xml out list -1)))

(defmethod mscx-export ((self t) &key keys approx path name) nil)

(defmethod mscx-export ((self voice) &key keys approx path name)
  (mscx-export (make-instance 'poly :voices self)
	       :keys keys :approx approx :path path :name name))

(defmethod! export-mscx ((self t) &optional (keys nil) (approx 2) (path nil))
  :icon 351
  :indoc '("a VOICE or POLY object" "list of voice keys" "tone subdivision approximation" "a target pathname")
  :initvals '(nil '((G 2)) 2 nil)
  :doc "
Exports <self> to MuseScore MSCX format.

Hack v1:
- notes/chords/rests
- clef
- time signature
- velocity only
"
  (let* ((staff (get-edit-param (associated-box self) 'staff))
         (clefs (loop for i in staff collect (clefs->xml i))))
    (mscx-export self :keys (if clefs clefs '((G 2)))
                 :approx approx :path path)))

(defmethod! export-mscx ((self voice) &optional (keys nil) (approx 2) (path nil))
  :icon 351
  :indoc '("a VOICE or POLY object" "list of voice keys" "tone subdivision approximation" "a target pathname")
  :initvals '(nil ((G 2)) 2 nil)
  :doc "
Exports <self> to MuseScore MSCX format.

Hack v1:
- notes/chords/rests
- clef
- time signature
- velocity only
"
  (let* ((staff (get-edit-param (associated-box self) 'staff))
         (clefs (list (clefs->xml staff))))
    (mscx-export self :keys (if clefs clefs '((G 2)))
                      :approx approx :path path)))

(defmethod! export-mscx ((self poly) &optional (keys '((G 2))) (approx 2) (path nil))
  (call-next-method))
