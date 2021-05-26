;;; (piece-table-api) The public API of the `piece-table` text buffer.

;;; Copyright (C) 2021 David D. Clark <david@axaluna.com>

;; Commentary:

;; Code:

(library (piece-train-impl piece-train-api)
  (export
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

  (import (piece-train-impl utils)
          (piece-train-impl piece-train-record)
          (piece-train-impl piece-train-internal-utils)
          (chezscheme))

  ;; Create a text buffer from a string. Useful for debugging.
  (define (piece-train-from-string str)
    (let ((tb (make-piece-train (list->vector (string->list str))
                                (make-vector 0)
                                '())))
      (%init-piece-table! tb)
      tb))

  ;; Create a piece-train from a file.
  (define (piece-train-from-file file-name)
    (let ((tb (make-piece-train (list->vector (file->char-list file-name))
                                (make-vector 0)
                                '())))
      (%init-piece-table! tb)
      tb))

  ;; Return the number of characters in the buffer.
  (define (piece-train-length buffer)
    (%piece-train-length buffer))

  ;; Append an item to the end of the piece-train. Will append correctly to
  ;; both the base vector and the edits vector, as needed.
  (define (piece-train-append! buffer item)
    (let* ((last-piece (last (piece-train-pt buffer)))
           (my-item (%piece-train-canonicalize-additions item)))
      (cond ((piece-from-edits last-piece)
             ;; If adding to edits, just extend the length of the last piece and
             ;; copy the new item to the end of the existing 'added' vector.
             (piece-length-set! last-piece (+ (piece-length last-piece)
                                              (length my-item))))
            ;; If adding and only the base vector is present, create a new piece
            ;; and add it to the piece table list.
            (else (let ((new-piece (make-piece #t 0 (length my-item))))
                    (piece-train-pt-set! buffer (append (piece-train-pt buffer)
                                                        (list new-piece))))))
      (%piece-train-append-edits! buffer my-item)))

  ;; Insert an item (a character, list of characters, or a string) into the
  ;; piece-train after the specified position, zero-based from the beginning
  ;; of the buffer.
  (define (piece-train-insert! buffer position item)
    ;; Do a (hopefully) quick check to see if the new item can just be
    ;; appended to the existing buffer.
    (if (= position (piece-train-length buffer))
        (piece-train-append! buffer item)
        (let* ((my-item (%piece-train-canonicalize-additions item))
               (my-res (%piece-train-offset->piece-and-offset buffer position))
               (piece (vector-ref my-res 0))
               (offset (vector-ref my-res 1))
               (piece-index (vector-ref my-res 2)))
          (cond ((and (piece-from-edits piece)
                      (= offset (+ (piece-start piece) (piece-length piece)))
                      (= (vector-length (piece-train-edits buffer))
                         (+ (piece-start piece) (piece-length piece))))
                 ;; Special case append to the end of the piece-train.
                 (piece-length-set! piece
                                    (+  (piece-length piece) (length my-item))))
                ;; General case. Make a new piece containing the new item and
                ;; adjust the pieces on both sides. Splice the non-empty pieces
                ;; into the piece list.
                (else
                 (let ((p1 (make-piece (piece-from-edits piece)
                                       (piece-start piece)
                                       (- offset (piece-start piece))))
                       (p2 (make-piece #t
                                       (vector-length (piece-train-edits buffer))
                                       (length my-item)))
                       (p3 (make-piece (piece-from-edits piece)
                                       offset
                                       (- (piece-length piece)
                                          (- offset (piece-start piece))))))
                   (piece-train-pt-set!
                    buffer
                    (splice (piece-train-pt buffer)
                            (filter (lambda (piece) (> (piece-length piece) 0))
                                    (list p1 p2 p3)) piece-index 1)))))
          (%piece-train-append-edits! buffer my-item))))

  ;; Delete the entire contents of the buffer. Note that this just replaces the
  ;; piece table with an empty piece so that undo history can be retained if
  ;; implemented somewhere else.
  (define (piece-train-delete-all! buffer)
    (let* ((edit-buffer-length (vector-length (piece-train-edits buffer)))
           (empty-piece (make-piece #t edit-buffer-length 0)))
      (piece-train-pt-set! buffer (list empty-piece))))

  ;; !!!NOTE: This is an abomination and requires some thought to restructure.

  ;; Delete a string from the buffer. Delete 'del-length' characters from the buffer,
  ;; beginning with the character at 'buffer-position'.
  (define (piece-train-delete! buffer buffer-position del-length)
    (cond
     ((= del-length 0) '())
     ((< del-length 0) (piece-train-delete!
                        buffer
                        (+ buffer-position del-length) (- del-length)))
     ((< buffer-position 0) (assertion-violation
                             piece-train-delete!
                             "buffer-position cannot be negative." buffer-position))
     ((and (zero? buffer-position)
           (= del-length (piece-train-length buffer)))
      (piece-train-delete-all! buffer))
     ((= (+ buffer-position del-length) (piece-train-length buffer))
      (%piece-train-chop-all-after buffer buffer-position del-length))
     (else (let* ((first-res (%piece-train-offset->piece-and-offset buffer buffer-position))
                  (initial-affected-piece (vector-ref first-res 0))
                  (initial-buffer-offset (vector-ref first-res 1))
                  (initial-buffer-index (vector-ref first-res 2))
                  (second-res (%piece-train-offset->piece-and-offset
                               buffer (+ buffer-position del-length)))
                  (final-affected-piece (vector-ref second-res 0))
                  (final-buffer-offset (vector-ref second-res 1))
                  (final-buffer-index (vector-ref second-res 2)))
             ;; If the desired sequence occurs within a single piece, only need
             ;; to return part of that single piece.at the end or beginning of a single
             ;; piece, simply adjust the window.
             (cond
              ((equal? initial-affected-piece final-affected-piece)
               (let ((piece initial-affected-piece))
                 ;; Is the delete at the beginning of the piece?
                 (if (= initial-buffer-offset (piece-start piece))
                     (begin
                       (piece-start-set! piece (+ (piece-start piece) del-length))
                       (piece-length-set! piece (- (piece-length piece) del-length)))
                     ;; ... or at the end of the piece?
                     (if (= final-buffer-offset
                            (+ (piece-length piece)  (piece-start piece)))
                         (piece-length-set! piece
                                            (- (piece-length piece)
                                               del-length))
                         (%piece-train-general-delete! buffer
                                                       initial-affected-piece
                                                       initial-buffer-offset
                                                       initial-buffer-index
                                                       final-affected-piece
                                                       final-buffer-offset
                                                       final-buffer-index)))))
              (else (%piece-train-general-delete! buffer
                                                  initial-affected-piece
                                                  initial-buffer-offset
                                                  initial-buffer-index
                                                  final-affected-piece
                                                  final-buffer-offset
                                                  final-buffer-index)))))))

  ;; Return the character at the position in the buffer.
  (define (piece-train-char-at buffer position)
    (let* ((my-res (%piece-train-offset->piece-and-offset buffer position))
           (piece (vector-ref my-res 0))
           (offset (vector-ref my-res 1)))
      (vector-ref (%piece->char-vector buffer piece) offset)))

  (define (piece-train-copy! buffer from-begin from-end to-position)
    '())

  (define (piece-train-move! buffer from-begin from-end to-position)
    '())

  (define (piece-train-replace! buffer from-begin from-end sequence-to-insert)
    '())

  ;; Return a string constructed from the buffer sequence starting at 'position'
  ;; and running for 'length' characters.
  (define (piece-train-sequence-at buffer position length)
    ;;(println "piece-train-sequence")
    (let* ((pt (piece-train-pt buffer))
           (start (%piece-train-run-to-pos pt position))
           (piece (car (vector-ref start 0)))
           (offset-from-piece-start (vector-ref start 1))
           (offset-in-vector (+ (piece-start piece) offset-from-piece-start))
           (vec-to-use (%vec-to-use buffer piece))
           (amt-can-get (min length (- (piece-length piece) offset-from-piece-start)))
           (char-vec (subvector vec-to-use offset-in-vector
                                (+ offset-in-vector amt-can-get))))
      (cond ((> (- length amt-can-get) 0)
             ;; Need more
             (%piece-train-collect-to-length buffer (cdr (vector-ref start 0))
                                             (- length amt-can-get)
                                             char-vec))
            (else
             ;; Got all we need
             (char-vector->string char-vec)))))

  ;; Combine the original text and the changes into a single string
  ;; representing the edited text and return it.
  (define (piece-train-summary buffer)
    (letrec ((helper (lambda (lst)
                       (cond ((null? lst) '#())
                             (else (vector-append (%piece->char-vector buffer (car lst))
                                                  (helper (cdr lst))))))))
      (char-vector->string (helper (piece-train-pt buffer)))))

  (define (piece-train-undo buffer)
    '())

  (define (piece-train-redo buffer)
    '())

  )


;; (piece-table-api) ends here.
