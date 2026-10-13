;;;========================
;;; OpenMusic export MSCX 
;;;========================
;;;
;;; Anders Vinjar - 2026
;;; 
;;; Time-stamp: <2026-10-08 14:42:28 andersvi>
;;; 
;;; map OM_MSCORE instrument names and vocabularies to/from Muse Sounds.
;;; 
;;; General API.  Edit/add family definitions in separate files, check "muse-strings.lisp";
;;; 
;;; Load before mscx-expression.lisp.

(defpackage "MSCX"
  (:use "COMMON-LISP" "MusicXML" :om)
  (:nicknames :mscx)
  (:export #:muse-sounds-family #:instrument-mapping
           #:register-muse-sounds-family #:resolve-instrument-mappings
           #:find-instrument-mapping #:instrument-vocabulary #:instrument-term
           #:instrument-playing-technique #:instrument-ordinary-value
           #:muse-sounds-family-id #:muse-sounds-family-name
           #:muse-sounds-family-vocabulary #:muse-sounds-family-instruments
           #:muse-sounds-family-default-instrument #:muse-sounds-family-ordinary
           #:instrument-mapping-id #:instrument-mapping-name
           #:instrument-mapping-family #:instrument-mapping-om-names
           #:instrument-mapping-musescore-id #:instrument-mapping-musicxml-id
           #:instrument-mapping-muse-sound-name #:instrument-mapping-vocabulary
           #:instrument-mapping-ordinary #:instrument-mapping-evidence
           #:instrument-expression-values
	   #:extra-expressions #:expression-playback #:expression-context #:expand-semantic-expression 
           #:make-expression-context #:expression-extras-as-mscx
           #:expression-as-mscx #:expression-node-description
           #:mscx-expression-node-as-xml
           #:expression-context-family #:expression-context-mapping
           #:expression-context-instrument #:expression-context-current-technique
           #:expression-context-current-notation-technique))

(in-package :mscx)

(defclass instrument-mapping ()
  ((id :initarg :id :reader instrument-mapping-id)
   (name :initarg :name :initform nil :reader instrument-mapping-name)
   (family :initarg :family :initform nil :accessor instrument-mapping-family)
   (om-names :initarg :om-names :initform nil :reader instrument-mapping-om-names)
   (musescore-id :initarg :musescore-id :initform nil :reader instrument-mapping-musescore-id)
   (musicxml-id :initarg :musicxml-id :initform nil :reader instrument-mapping-musicxml-id)
   (muse-sound-name :initarg :muse-sound-name :initform nil :reader instrument-mapping-muse-sound-name)
   ;; Overrides merge by semantic term and then by plist field. In particular,
   ;; :playing-technique NIL explicitly removes an inherited playback mapping.
   (vocabulary :initarg :vocabulary :initform nil :accessor instrument-mapping-vocabulary)
   (ordinary :initarg :ordinary :initform :inherit :reader instrument-mapping-ordinary)
   (evidence :initarg :evidence :initform nil :reader instrument-mapping-evidence)))



;;;
;;; Muse Sounds family
;;; 
;;; A Muse Sounds family groups shared vocabulary and instrument mappings.  check muse-strings.lisp for muse-strings
;;; 
;;; Slots:
;;; id                 Registry key used by :instrument-mappings.
;;; name               Human-readable family name, e.g. "Muse Strings".
;;; vocabulary         Shared term mappings; instruments may override fields.
;;; ordinary           Semantic term used to reset playback, e.g. :arco.
;;; instruments        List of instrument-mapping objects in this family.
;;; default-instrument Mapping used when no instrument identity is supplied.
;;;                    Represents shared mappings, not a selected mixer sound.

(defclass muse-sounds-family ()
  ((id :initarg :id :reader muse-sounds-family-id)
   (name :initarg :name :reader muse-sounds-family-name)
   (vocabulary :initarg :vocabulary :initform nil :accessor muse-sounds-family-vocabulary)
   (ordinary :initarg :ordinary :initform nil :reader muse-sounds-family-ordinary)
   (instruments :initarg :instruments :initform nil :reader muse-sounds-family-instruments)
   ;; Retains the existing family-wide export when the caller omits INSTRUMENTS.
   ;; It identifies shared mappings, not a particular installed Muse sound.
   (default-instrument :initarg :default-instrument
                       :initform (make-instance 'instrument-mapping :id :unspecified)
                       :reader muse-sounds-family-default-instrument)))

(defvar *mscx-muse-sounds-families* (make-hash-table :test #'eq))

(defun instrument-name-key (name)
  (when (or (stringp name) (symbolp name))
    (string-downcase
     (string-trim '(#\Space #\Tab #\Newline)
                  (if (symbolp name) (symbol-name name) name)))))

(defun instrument-mapping-names (mapping)
  (remove nil
          (append (list (instrument-mapping-id mapping)
                        (instrument-mapping-name mapping)
                        (instrument-mapping-musescore-id mapping)
                        (instrument-mapping-musicxml-id mapping)
                        (instrument-mapping-muse-sound-name mapping))
                  (instrument-mapping-om-names mapping))))

(defun merge-instrument-vocabulary (common overrides)
  "Merge terms/fields without modifying either source table."
  (let ((result (copy-tree common)))
    (dolist (entry overrides result)
      (let ((existing (assoc (car entry) result)))
        (if existing
            (loop for (key value) on (cdr entry) by #'cddr do
              (setf (getf (cdr existing) key) (copy-tree value)))
          (setf result (append result (list (copy-tree entry)))))))))

(defun instrument-vocabulary (family &optional mapping)
  "Effective terms for one family/instrument. Availability is not inferred.
Each term is (KEY :text STRING :om-text (ALIASES...) :role ROLE :placement PLACE
:playing-technique ID
:mscx-notation RULE :extra-notation (DESCRIPTORS...) :evidence DATA).
Missing or NIL playback IDs retain notation/text. Only confirmed IDs belong
in :playing-technique; :ordinary refers to the known ordinary/reset term."
  (when family
    (merge-instrument-vocabulary (muse-sounds-family-vocabulary family)
                                 (and mapping (instrument-mapping-vocabulary mapping)))))

(defun instrument-term (family mapping value)
  (cdr (assoc value (instrument-vocabulary family mapping))))

(defun instrument-ordinary-value (family mapping)
  (when (and family mapping)
    (let ((value (instrument-mapping-ordinary mapping)))
      (if (eq value :inherit) (muse-sounds-family-ordinary family) value))))

(defun instrument-playing-technique (family mapping value)
  "Return an explicitly configured ID; unknown instruments have no IDs."
  (when (and family mapping)
    (let* ((term (instrument-term family mapping value))
           (id (getf term :playing-technique)))
      (when (eq id :ordinary)
        (setf id (getf (instrument-term family mapping
                                       (instrument-ordinary-value family mapping))
                      :playing-technique)))
      (and (stringp id) id))))

(defun register-muse-sounds-family (family)
  "Register/replace a family without changing exporter or extras adapters."
  (check-type family muse-sounds-family)
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (mapping (cons (muse-sounds-family-default-instrument family)
                           (muse-sounds-family-instruments family)))
      (when mapping
        (check-type mapping instrument-mapping)
        (dolist (entry (instrument-vocabulary family mapping))
          (let ((id (getf (cdr entry) :playing-technique)))
            (unless (or (null id) (stringp id) (eq id :ordinary))
              (error "MSCX: invalid playingTechnique ID ~S in term ~S." id (car entry)))))
        (unless (eq mapping (muse-sounds-family-default-instrument family))
          (dolist (name (instrument-mapping-names mapping))
            (let* ((key (instrument-name-key name)) (previous (gethash key seen)))
              (when (and previous (not (eq previous mapping)))
                (error "MSCX: ambiguous instrument name ~S in family ~S."
                       name (muse-sounds-family-id family)))
              (setf (gethash key seen) mapping))))))
    ;; Commit parent links and registry only after the whole definition validates.
    (dolist (mapping (cons (muse-sounds-family-default-instrument family)
                           (muse-sounds-family-instruments family)))
      (when mapping (setf (instrument-mapping-family mapping) family)))
    (setf (gethash (muse-sounds-family-id family) *mscx-muse-sounds-families*) family)))

(defun resolve-instrument-mappings (selection)
  "Select a Muse Sounds mapping family. NIL/:notation-only adds no playback.
This selects mappings; it does not select a sound in the MuseScore mixer."
  (cond ((or (null selection) (eq selection :notation-only)) nil)
        ((typep selection 'muse-sounds-family) selection)
        ((gethash selection *mscx-muse-sounds-families*))
        (t (error "Unknown MSCX instrument mappings ~S." selection))))

(defun find-instrument-mapping (family instrument)
  "Look up OM aliases, MSCX IDs or Muse sound names within FAMILY."
  (let ((family (resolve-instrument-mappings family)))
    (when family
      (cond ((null instrument) (muse-sounds-family-default-instrument family))
            ((typep instrument 'instrument-mapping)
             (if (eq (instrument-mapping-family instrument) family) instrument
               (error "MSCX: instrument mapping ~S belongs to another family."
                      (instrument-mapping-id instrument))))
            (t (let ((key (instrument-name-key instrument)))
                 (and key
                      (find-if (lambda (mapping)
                                 (member key (instrument-mapping-names mapping)
                                         :key #'instrument-name-key :test #'equal))
                               (muse-sounds-family-instruments family)))))))))

(defun instrument-expression-values (family instrument &optional role)
  "Return explicitly classified expression keys for FAMILY and INSTRUMENT.
ROLE optionally selects a semantic role, e.g. :TECHNIQUE or :ARTICULATION.
Terms without an explicit :ROLE are omitted; no availability is inferred.
An empty result means no matching palette has been declared."
  (let* ((family (resolve-instrument-mappings family))
         (mapping (find-instrument-mapping family instrument)))
    (when (and family instrument (null mapping))
      (error "MSCX: unknown instrument ~S in family ~S."
             instrument (muse-sounds-family-id family)))
    (loop for (value . properties) in
				   (instrument-vocabulary family mapping)
          for declared-role = (getf properties :role)
          when (and declared-role
                    (or (null role) (eq role declared-role)))
            collect value)))

(defun choose-instrument-mappings (selection selection-p legacy legacy-p)
  "Temporary keyword bridge; new code uses :instrument-mappings."
  (when (and selection-p legacy-p (not (equal selection legacy)))
    (error "MSCX: choose :instrument-mappings or :expression-profiles, not conflicting values."))
  (if selection-p selection (if legacy-p legacy :notation-only)))

