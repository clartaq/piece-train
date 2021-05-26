;;; (piece-train) A piece-table-based text buffer.

;; Copyright 2021 David D. Clark <david@axaluna.com>

;;; Commentary:

;;; Code:

(library (piece-train)
  (export ;; General utilities
          subvector
          vector-append
          subvector->list
          char-vector->string
          last
          take
          flatten
          split
          chop
          splice
          file->char-list

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
          piece-length-set!

          ;; These are internal text buffer utilities. They are only
          ;; exposed to enable testing.
          %vec-to-use
          %piece->char-vector
          %init-piece-table!
          %piece-train-debug-piece->content-string
          %piece-train-debug-piece->info-string
          %piece-train-debug-piece->info-and-text-string
          %piece-train-debug-piece-list
          %piece-train-debug-all-pieces->string
          %piece-train-canonicalize-additions
          %piece-train-append-edits!
          %piece-train-collect-to-length
          %piece-train-run-to-pos
          %piece-train-chop-all-after
          %piece-train-offset->piece-and-offset
          %piece-train-length-before-piece
          %piece-train-general-delete!

          ;; These are the procedures that constitute the API.
          piece-train-from-string
          piece-train-from-file
          piece-train-length
          piece-train-append!
          piece-train-insert!
          piece-train-delete-all!
          piece-train-delete!
          piece-train-char-at
          piece-train-copy!
          piece-train-move!
          piece-train-replace!
          piece-train-sequence-at
          piece-train-summary)

  (import (chezscheme)
          (piece-train-impl utils)
          (piece-train-impl piece-train-record)
          (piece-train-impl piece-train-internal-utils)
          (piece-train-impl piece-train-api)
          ))
