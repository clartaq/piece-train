;;; (text-train-impl utils) Utilities used in the text-buffer.

;; Copyright (C) 2021 David D. Clark <david@axaluna.com>
;;
;; This file is part of the text-buffer library.
;;

(library (piece-train-impl utils)
  (export subvector
          vector-append
          subvector->list
          char-vector->string
          last
          take
          flatten
          split
          chop
          splice
          file->char-list)
  (import (scheme))

;;; Commentary:
;;;
;;; These functions are general-purpose utilities used by the text-buffer
;;; library. They have no knowledge of the text-buffer internals or API.

;;; Code:

  ;; Return a freshly allocated vector containing the same contents as
  ;; the source vector from index start (inclusive) to end (exclusive).
  ;; Always assumes end is greater than start and that start is non-negative.
  (define (subvector v start end)
    (cond
     [(> start end) (assertion-violation 'subvector
                                         "'start' must be less than 'end'"
                                         end)]
     [(negative? start) (assertion-violation 'subvector
                                             "'start' must not be negative."
                                             start)]
     [else
      (let ((sv (make-vector (- end start))))
        (letrec ((left-to-right-loop
                  (lambda (target source s-end i j)
                    (when(< i s-end)
                      (vector-set! target j
                                   (vector-ref source i))
                      (left-to-right-loop target source s-end (+ i 1) (+ j 1))))))
          (left-to-right-loop sv v end start 0)
          sv))]))

  ;; Return a newly allocated vector that contains the contents of v1
  ;; followed by the contents of v2. NOTE: This is not at all equivalent
  ;; to the procedure in SRFI 43.
  (define (vector-append v1 v2)
    (cond
     ((zero? (vector-length v1)) (vector-copy v2))
     ((zero? (vector-length v2)) (vector-copy v1))
     (else (let* ((v1-len (vector-length v1))
                  (new-len (+ v1-len (vector-length v2)))
                  (new-vec (make-vector new-len)))
             ;; Copy the contents of v1 into to new vector.
             (let v1-loop ((i 0))
               (when (< i v1-len)
                 (vector-set! new-vec i (vector-ref v1 i))
                 (v1-loop (+ i 1))))
             ;; Copy the contents of v2 into the new vector.
             (let v2-loop ((i v1-len)
                           (j 0))
               (when (< i new-len)
                 (vector-set! new-vec i (vector-ref v2 j))
                 (v2-loop (+ i 1) (+ j 1))))
             new-vec))))

  ;; Convert a region of vector `v` between `start` (inclusive) and `end`
  ;; (exclusive) to a list and return it.
  (define (subvector->list v start end)
    (let loop ((index (- end 1))
               (acc '()))
      (cond
       [(< index start) acc]
       [else (loop (- index 1) (cons (vector-ref v index) acc))])))

  ;; Return the characters in the vector as a string.
  (define (char-vector->string v)
    (list->string (vector->list v)))

  ;; Return the last item in a list or the empty list if the
  ;; list is empty.
  (define (last lst)
    (cond ((null? lst) '())
          ((= (length lst) 1) (car lst))
          (else (last (cdr lst)))))

  ;; Return a list of the first n elements of a list.
  (define (take lst n)
    (let loop ((my-list lst)
               (count-down n)
               (acc '()))
      (cond ((or (zero? count-down)  (null? my-list))
             (reverse acc))
            (else (loop (cdr my-list)
                        (- count-down 1)
                        (cons (car my-list) acc))))))

  ;; From this Stackoverflow answer:
  ;; https://codereview.stackexchange.com/a/260199/199535

  ;; Return a flattened list from which all '() have been removed.
  (define (flatten tree)
    (let ((lst '()))
      (letrec ((traverse (lambda (subtree)
                           (cond ((null? subtree) '())
                                 ((pair? subtree)
                                  (traverse (car subtree))
                                  (traverse (cdr subtree)))
                                 (else (set! lst (cons subtree lst)))))))
        (traverse tree)
        (reverse lst))))

  ;; Start on splice. The following few functions are from a StackOverflow
  ;; answer: https://stackoverflow.com/a/47203032/193509

  ;; Split the list at the zero-based index forming left and right parts. Pass
  ;; the left and right parts to the continuation for continued processing.
  ;; Returns the result of executing the continuation.
  ;; (split '(1 2 3 4 5) 2 (lambda x x) => ((1 2) (3 4 5))
  (define (split lst index cont)
    (let loop ((left '())
               (right lst)
               (i index))
      (cond ((> i 0)
             (loop (cons (car right) left)
                   (cdr right)
                   (- i 1)))
            (else (cont (reverse left) right)))))

  ;; Remove the given number of elements from the beginning of the list and
  ;; return the shortened list.
  ;; (chop '(3 4 5) 1) => (4 5)
  (define (chop lst n)
    (cond ((> n 0) (chop (cdr lst) (- n 1)))
          (else lst)))

  ;; Splice the second list into the first start at the index.
  ;; Simultaneously, delete the given number of elements from
  ;; the result at the position of the slice.
  ;; (splice '(1 2 3 4 5) '(a b c) 2 1) => (1 2 a b c 4 5)
  (define (splice lst1 lst2 index to-delete)
    (split lst1 index
           (lambda (left right)
             (append left lst2 (chop right to-delete)))))

  ;; Read the contents of the file at path and return a list of characters
  ;; it contains. In the event of an error, return '().
  (define (file->char-list path)
    (guard (con
            ((i/o-file-does-not-exist-error? con) '()))
      (string->list (call-with-input-file path get-string-all))))

  )
;; (piece-train-impl utils) ends here.
