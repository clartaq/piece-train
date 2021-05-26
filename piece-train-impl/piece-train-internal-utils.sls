;;; (piece-train-internal-utils) Internal utilities for the `piece-train` project.

;;; Copyright (C) 2021 David D. Clark <david@axaluna.com>

;; Commentary:

;; Code:

(library (piece-train-impl piece-train-internal-utils)
  (export

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
   %piece-train-length
   %piece-train-chop-all-after
   %piece-train-offset->piece-and-offset
   %piece-train-length-before-piece
   %piece-train-general-delete!)

  (import (piece-train-impl utils)
          (piece-train-impl piece-train-record)
          (chezscheme))

  ;; Return the vector, base or added, to use when retrieving data for the
  ;; given piece.
  (define (%vec-to-use bufr piece)
    (cond ((piece-from-edits piece) (piece-train-edits bufr))
          (else (piece-train-base bufr))))

  ;; Return the text associated with a piece as a vector of characters.
  (define (%piece->char-vector bufr piece)
    (let* ((vec-to-use (%vec-to-use bufr piece)))
      (subvector vec-to-use (piece-start piece)
                 (+ (piece-start piece) (piece-length piece)))))

  ;; Initialize the piece table of a newly created `piece-train` such that it
  ;; contains a single piece, the entire original document.
  (define (%init-piece-table! tb)
    (let* ((base (piece-train-base tb))
           (last-index (- (vector-length (piece-train-base tb)) 1))
           (piece (make-piece #f 0 (+ 1 last-index))))
      (piece-train-pt-set! tb (list piece))))

  ;; Return the text associated with a piece as a string.
  (define (%piece-train-debug-piece->content-string buffer piece)
    (char-vector->string (%piece->char-vector buffer piece)))

  ;; Return a string containing info about the piece only (no buffer stuff).
  (define (%piece-train-debug-piece->info-string piece)
    (let ((source (cond ((piece-from-edits piece)
                         "from edits")
                        (else "from base"))))
      (string-append source ": start: " (number->string (piece-start piece))
                     ", length: " (number->string (piece-length piece)))))

  ;; Return a string containing info about the piece, including the piece of
  ;; text it describes.
  (define (%piece-train-debug-piece->info-and-text-string buffer piece)
    (string-append (%piece-train-debug-piece->info-string piece)
                   ", text: "
                   (%piece-train-debug-piece->content-string buffer piece)))

  ;; Return a string that includes all of the information about the piece table
  ;; including the text that each piece describes.
  (define (%piece-train-debug-piece-list buffer piece-lst)
    ;;(println "%piece-train-debug-piece-list: piece-lst: " piece-lst)
    (letrec ((helper (lambda (lst idx out-str)
                       (cond
                        ((null? lst) out-str)
                        (else ;;(println "(car lst): " (car lst))
                         (helper
                          (cdr lst) (+ 1 idx)
                          (string-append
                           out-str
                           "piece #"
                           (number->string idx) ": "
                           (%piece-train-debug-piece->info-and-text-string
                            buffer (car lst))
                           "\n")))))))
      (helper piece-lst 0 "")))

  ;; Return a string that includes all of the information about the piece table
  ;; including the text that each piece describes.
  (define (%piece-train-debug-all-pieces->string buffer)
    (%piece-train-debug-piece-list buffer (piece-train-pt buffer)));;)

  ;; Return the edits in a "canonical" form -- a list of `char`s.
  (define (%piece-train-canonicalize-additions edits)
    (cond
     ((string? edits) (string->list edits))
     ((char? edits) (list edits))
     ((list? edits) edits)
     (else (assertion-violation %piece-train-canonicalize-additions
                                "Unexpected data type" edits))))

  ;; Add new text to the `added` vector of the buffer. The 'edits' are
  ;; expected to be a list of characters.
  (define (%piece-train-append-edits! buffer edits)
    (let* ((v-len (vector-length (piece-train-edits buffer)))
           (new-len (+ (length edits) v-len))
           (new-vec (make-vector new-len))
           (tbe (piece-train-edits buffer)))
      ;; Copy the old vector.
      (let v-loop ((i 0))
        (when (< i v-len)
          (vector-set! new-vec i (vector-ref tbe i))
          (v-loop (+ i 1))))
      ;; Copy list of edit characters.
      (let l-loop ((i v-len)
                   (lst edits))
        (cond ((null? lst) '())
              (else (vector-set! new-vec i (car lst))
                    (l-loop (+ i 1) (cdr lst)))))
      (piece-train-edits-set! buffer new-vec)))

  ;; Collect an additional 'len' characters from 'buffer' using the portion of
  ;; the piece table starting at 'lst'. Append those characters to the 'acc'
  ;; vector. Convert the collected vector to a string and return it.
  (define (%piece-train-collect-to-length buffer lst len acc)
    ;;(println "%piece-train-collect-to-length: len: " len ", acc: " acc)
    (let* ((piece (car lst))
           (offset (piece-start piece))
           (amt-to-get (min len (piece-length piece)))
           (amt-left (- len amt-to-get))
           (vec-to-use (%vec-to-use buffer piece))
           (char-vec (subvector vec-to-use offset (+ offset amt-to-get)))
           (new-acc (vector-append acc char-vec)))
      (cond ((<= amt-left 0)
             (char-vector->string new-acc))
            (else
             (%piece-train-collect-to-length buffer (cdr lst) amt-left new-acc)))))

  ;; Return the number of characters in the buffer.
  (define (%piece-train-length buffer)
    (letrec ((helper (lambda (lst sum)
                       (cond ((null? lst) sum)
                             (else (helper (cdr lst)
                                           (+ sum (piece-length (car lst)))))))))
      (helper (piece-train-pt buffer) 0)))

  ;; Handle the special case of deleting to the end of the buffer.
  (define (%piece-train-chop-all-after buffer position del-len)
    (let* ((original-buf-length (%piece-train-length buffer))
           (my-res (%piece-train-offset->piece-and-offset buffer position))
           (piece (vector-ref my-res 0))
           (existing-length (piece-length piece))
           (index (vector-ref my-res 2))
           (length-in-front (%piece-train-length-before-piece buffer index))
           (shorter-piece-list (take (piece-train-pt buffer) (+ 1 index)))
           (last-piece (last shorter-piece-list))
           (len-reduction (- original-buf-length del-len length-in-front)))
      (piece-train-pt-set! buffer shorter-piece-list)
      (piece-length-set! last-piece len-reduction)))

  ;; !!!NOTE: These next two procedures do almost the same thing in almost
  ;; the same way. Any way to combine them?

  ;; Run to the list piece containing the character at position and
  ;; return the list starting with that piece and the position in that
  ;; piece (offset from piece-start).
  (define (%piece-train-run-to-pos lst position)
    ;;(println "%piece-train-run-to-pos: position: " position)
    (cond
     ((null? lst)
      (error "%piece-train-run-to-pos"
             "piece-list was null" lst))

     ;; This says the search is finished.
     ((< position (piece-length (car lst)))
      (let ((v (make-vector 2)))
        (vector-set! v 0 lst)
        (vector-set! v 1 position)
        v))

     ;; Keep looking.
     (else
      (%piece-train-run-to-pos (cdr lst) (- position (piece-length (car lst)))))))

  ;; From the offset in the combined text sequence, determine which piece in
  ;; the table and the offset in the piece and return them in a vector. The
  ;; 0th element of the result will contain the piece. The second element
  ;; will contain the offset into the piece. The third element of the vector
  ;; with contain the "index" of the piece in the list of pieces.
  (define (%piece-train-offset->piece-and-offset buffer offset)
    (when (< offset 0)
      (assertion-violation %piece-train-offset->piece-and-offset
                           "Offset cannot be less than zero" offset))
    (let ((v (make-vector 3)))
      (let loop ((piece-list (piece-train-pt buffer))
                 (remaining-length offset)
                 (idx 0))
        (cond
         ;; offset was larger than length of buffer, or the piece list was empty.
         ((null? piece-list) (error "%piece-train-offset->piece-and-offset"
                                    "piece-list was '()" piece-list))

         ;; This says the search is finished.
         ((< remaining-length (piece-length (car piece-list)))
          (let ((found-piece (car piece-list)))
            (vector-set! v 0 found-piece)
            (vector-set! v 1 (+ remaining-length (piece-start found-piece)))
            (vector-set! v 2 idx)
            v))

         ;; Look at next element in piece list.
         (else (loop (cdr piece-list)
                     (- remaining-length (piece-length (car piece-list)))
                     (+ 1 idx)))))))

  ;; Return the length of the piece-train before the given piece.
  (define (%piece-train-length-before-piece buffer piece-index)
    (letrec ((helper (lambda (lst sum cnt)
                       (cond ((zero? cnt) sum)
                             (else (helper (cdr lst)
                                           (+ sum (piece-length (car lst)))
                                           (- cnt 1)))))))
      (helper (piece-train-pt buffer) 0 piece-index)))

  ;; Internal procedure to delete text that is not at the beginning or end of the
  ;; buffer.
  (define (%piece-train-general-delete! buffer
                                        initial-affected-piece
                                        initial-buffer-offset initial-buffer-index
                                        final-affected-piece
                                        final-buffer-offset final-buffer-index)
    (let ((p1 (make-piece (piece-from-edits initial-affected-piece)
                          (piece-start initial-affected-piece)
                          (- initial-buffer-offset
                             (piece-start initial-affected-piece))))
          (p2 (make-piece (piece-from-edits final-affected-piece)
                          final-buffer-offset
                          (- (piece-length final-affected-piece)
                             (- final-buffer-offset
                                (piece-start final-affected-piece))))))
      (piece-train-pt-set!
       buffer
       (splice (piece-train-pt buffer)
               (filter (lambda (piece) (> (piece-length piece) 0))
                       (list p1 p2))
               initial-buffer-index (+ 1 (- final-buffer-index initial-buffer-index))))))


  )

;; (piece-train-internal-utils) ends here.
