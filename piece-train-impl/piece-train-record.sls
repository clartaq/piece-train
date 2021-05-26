;;; (piece-train-record) A record definition for the piece-train text buffer.

;; Copyright (C)  2021 David D. Clark <david@axaluna.com>

;;; Commentary:

;;; Code:

(library (piece-train-impl piece-train-record)
  (export

   ;; Procedures automatically created by the record definitions.
   make-piece-train
   piece-train-base
   piece-train-edits
   piece-train-edits-set!
   piece-train-pt
   piece-train-pt-set!

   make-piece
   piece-from-edits
   piece-start
   piece-start-set!
   piece-length
   piece-length-set!)

  (import (chezscheme))

  ;;------------------------------------------------------------------------------
  ;; The two key data types used to implement the text buffer.

  ;; A piece-train record contains the immutable original contents of the
  ;; document in the `base` vector. The `edits` field is an append-only vector
  ;; of characters containing text that is added to the document. The `pt`
  ;; field is a list of `piece`s (see below) containing information about the
  ;; edits to be applied to the `base` to generate  the edited document. Additions
  ;; reference added characters located in the `edits`field.
  (define-record-type piece-train (fields base
                                          (mutable edits)
                                          (mutable pt)))

  ;; An element in the piece table. `from-edits` is #t if the piece is contained
  ;; in the added field of the `piece-train`. Otherwise, the contents of the piece
  ;; are from the original document contents, `base`. `start` is the index of the
  ;; beginning of the piece in the `edits` vector or the `piece-train`. `length`
  ;; is the length of the piece.
  (define-record-type piece (fields from-edits (mutable start) (mutable length)))

  )

;; (piece-train-record) ends here.
