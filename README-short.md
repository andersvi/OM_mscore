# OM–MuseScore MSCX

A two-way bridge between OpenMusic and MuseScore notation.

It moves musical material between algorithmic composition in OpenMusic and clean, editable notation in MuseScore.

## Main files

- `export-mscx.lisp` — OpenMusic → MuseScore MSCX
- `import-mscx.lisp` — MuseScore MSCX → OpenMusic
- `om-mscore-workspace/` and `mscores/` — test patches and reference scores

## Loading

With OpenMusic's MusicXML support already loaded:

```lisp
(load "/path/to/export-mscx.lisp")
(load "/path/to/import-mscx.lisp")
```

## OM interface

```lisp
(export-mscx object clefs notation-scale path)
(import-mscx path)
```

`export-mscx` writes a MuseScore score from an OM voice or poly; `import-mscx`
returns an OM poly from a MuseScore score.
