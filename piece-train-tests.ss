;;;
;;; Run some tests for the piece-train data structure.
;;;

;; To get the following to import correctly, I added the following line:
;; export CHEZSCHEMELIBDIRS="./:/Users/David/projects/scheme/thunderchez:"
;; to my .zprofile file.

(import (easy-io)
        (srfi s64 testing)
        (piece-train))

(define max-verbosity 5)

(define message-prefix
  '#(
     "##### "
     " ####   "
     "  ###     "
     "   ##       "
     "    #         "
     ))

;; The maximum level of message we want to display.
(define piece-train-tests-verbosity 4)

;; Display a message preceeded by a heading and indentation based
;; on the level argument.
(define (status-msg level . msgs)
  (when (> piece-train-tests-verbosity 0)
    (let* ((lvl (- (if (> level max-verbosity)
                       max-verbosity
                       level) 1)))
      (display (vector-ref message-prefix lvl))
      (for-each display msgs)
      (newline))))

;; A test runner.

(define (turn-red)
  (display "\x1b;[38;5;196m"))

(define (turn-green)
  (display "\x1b;[38;5;46m"))

(define (turn-white)
  (display "\x1b;[38;5;15m"))

(define (my-simple-runner filename)
  (let ((runner (test-runner-null))
        (my-out-port (current-output-port))
        (num-passed 0)
        (num-failed 0))
    (test-runner-reset runner)
    (test-runner-on-test-end!
     runner
     (lambda (runner)
       (case (test-result-kind runner)
         ((pass xpass) (set! num-passed (+ num-passed 1)))
         ((fail xfail) (let ((alist (test-result-alist runner)))
                         (set! num-failed (+ num-failed 1))

                         (turn-red)
                         (display "  Failed: ")
                         (turn-white)
                         (display (test-runner-test-name runner))
                         (newline)

                         (display "Expected: ")
                         (turn-green)
                         (display (cdr (assoc 'expected-value alist)))
                         (turn-white)
                         (newline)

                         (turn-white)
                         (display "  Actual: ")
                         (turn-red)
                         (display (cdr (assoc 'actual-value alist)))
                         (turn-white)
                         (newline)))
         (else #t))))
    (test-runner-on-final! runner
                           (lambda (runner)
                             (format my-out-port "Passing tests: ~d.~%Failing tests: ~d.~%~%"
                                     num-passed num-failed)))
    runner))

(test-runner-factory
 (lambda () (my-simple-runner "./tmp/my-test.log")))

;; Some tests.
(turn-white)

(test-begin "piece-train")

(println)
(status-msg 1 "General Purpose Utilities")
(test-begin "1. General Purpose Utilities")

(status-msg 2 "subvector")
(test-eqv '#() (subvector '#() 0 0))
(test-equal '#(a 1 b 2) (subvector '#(a 1 b 2 c 3 d 4) 0 4))
(test-equal '#(c 3 d 4) (subvector '#(a 1 b 2 c 3 d 4) 4 8))
(test-equal '#(a) (subvector '#(a 1 b 2 c 3 d 4) 0 1))
(test-equal '#(4) (subvector '#(a 1 b 2 c 3 d 4) 7 8))
(test-equal '#() (subvector '#(a 1 b 2 c 3 d 4) 7 7))

(status-msg 2 "vector-append")
(test-equal '#() (vector-append '#() '#()))
(test-equal '#(a b c) (vector-append '#(a b c) '#()))
(test-equal '#(1 2 3) (vector-append '#() '#(1 2 3)))
(test-equal '#(a b c 1 2 3) (vector-append '#(a b c) '#(1 2 3)))

(status-msg 2 "subvector->list")
(test-eqv '() (subvector->list '#() 0 0))
(test-equal '(a 1 b 2) (subvector->list '#(a 1 b 2 c 3 d 4) 0 4))
(test-equal '(c 3 d 4) (subvector->list '#(a 1 b 2 c 3 d 4) 4 8))
(test-equal '(a) (subvector->list '#(a 1 b 2 c 3 d 4) 0 1))
(test-equal '(4) (subvector->list '#(a 1 b 2 c 3 d 4) 7 8))
(test-equal '() (subvector->list '#(a 1 b 2 c 3 d 4) 7 7))

(status-msg 2 "char-vector->string")
(test-equal "" (char-vector->string '#()))
(test-equal #f (char-vector->string '#(a b c)))
(test-equal "abc" (char-vector->string '#(#\a #\b #\c)))
(test-equal #f (char-vector->string '#(#\a #\b 3 #\c)))

(status-msg 2 "last")
(test-equal 'c (last '(a b c)))
(test-equal 'a (last '(a)))
(test-equal '() (last '()))

(status-msg 2 "take")
(test-equal '(a) (take '(a b c) 1))
(test-equal '() (take '() 1))
(test-equal '() (take '(a b c) 0))
(test-equal '(a b c) (take '(a b c) 3))
(test-equal '(a b c) (take '(a b c) 6))

(status-msg 2 "flatten")
(test-equal '() (flatten '()))
(test-equal '(a) (flatten '(a)))
(test-equal '(a b c) (flatten '(a b c)))
(test-equal '(a b c d e f g h) (flatten '((()) a (b c (((d)))) (e f (g)) (h ((()))))))
(test-equal '(a b c) (flatten '(() (a) b () ((c)) ())))

(status-msg 2 "split")
(test-equal '((1 2) (3 4 5)) (split '(1 2 3 4 5) 2 (lambda x x)))

(status-msg 2 "chop")
(test-equal '(4 5) (chop '(3 4 5) 1))

(status-msg 2 "splice")
(test-equal '(1 2 a b c 4 5) (splice '(1 2 3 4 5) '(a b c) 2 1))

;; Of course, the test requires a file with the given name that contains
;; the expected text.
(status-msg 2 "file->char-list")
(test-equal '(#\S #\o #\m #\e #\space #\t #\e #\x #\t #\. #\newline
              #\A #\space #\L #\i #\n #\e #\? #\newline)
            (file->char-list "test.txt"))

(test-end "1. General Purpose Utilities")
(status-msg 1 "Done Testing General Propose Utilities")

(println)
(status-msg 1 "Internal Utilities")
(test-begin "2. Internal Utilities")

(test-begin "2.1 %piece->char-vector")

(status-msg 2 "%piece->char-vector")
(let* ((td "Four character base buffer")
       (tb (piece-train-from-string "Text"))
       (piece (piece-train-pt tb)))
  (status-msg 3 td)
  (test-equal td '#(#\T #\e #\x #\t)
              (%piece->char-vector tb (car (piece-train-pt tb)))))

(let* ((td "Two-piece, hand-built buffer")
       (tb (piece-train-from-string "A Piece of"))
       (piece (make-piece #t 0 1))
       (new-piece (make-piece #t 0 5))
       (new-piece-list (append (piece-train-pt tb) (list new-piece))))
  (status-msg 3 td)
  (piece-train-pt-set! tb new-piece-list)
  (%piece-train-append-edits! tb '(#\space #\T #\e #\x #\t))
  (test-equal '#(#\A #\space #\P #\i #\e #\c #\e #\space #\o #\f)
              (%piece->char-vector tb (car (piece-train-pt tb))))
  (test-equal td '#(#\space #\T #\e #\x #\t)
              (%piece->char-vector tb (cadr (piece-train-pt tb)))))

(test-end "2.1 %piece->char-vector")

(status-msg 2 "%piece-train-offset->piece-and-offset")
(test-begin "2.2 %piece-train-offset->piece-and-offset")

;; %piece-train-append-edits! is used in the construction of several
;; test cases. If the tests work, we assume that the procedure does too.

;; Testing that, given a buffer and a character offset, assure that
;; `%piece-train-offset->piece-and-offset` returns the correct piece
;; from the piece list.

(let ((td "Single-piece, two-character buffer")
      (tb (piece-train-from-string "ab")))
  (status-msg 3 td)
  (let* ((my-res (%piece-train-offset->piece-and-offset tb 0))
         (piece (vector-ref my-res 0))
         (vec-to-use (%vec-to-use tb piece)))
    (test-eqv "Trying to get first character in two character base."
              #\a (vector-ref vec-to-use 0))

    (let* ((my-res (%piece-train-offset->piece-and-offset tb 1))
           (piece (vector-ref my-res 0))
           (vec-to-use (%vec-to-use tb piece)))
      (test-eqv "Trying to get second character in two character base."
                #\b (vector-ref vec-to-use 1)))))

(let* ((td "Two-piece, hand-built buffer")
       (tb (piece-train-from-string "a"))
       (new-piece (make-piece #t 0 1))
       (new-piece-list (append (piece-train-pt tb) (list new-piece))))
  (status-msg 3 td)
  (piece-train-pt-set! tb new-piece-list)
  (%piece-train-append-edits! tb '(#\b))
  (test-equal td "ab" (piece-train-summary tb))
  (test-equal td 2 (piece-train-length tb))

  (let* ((my-res (%piece-train-offset->piece-and-offset tb 0))
         (piece (vector-ref my-res 0))
         (vec-to-use (%vec-to-use tb piece)))
    (test-eqv
     "Trying to get first character in one character base and one character added."
     #\a (vector-ref vec-to-use 0)))

  (let* ((my-res (%piece-train-offset->piece-and-offset tb 1))
         (piece (vector-ref my-res 0))
         (offset (vector-ref my-res 1))
         (vec-to-use (%vec-to-use tb piece)))
    (test-eqv
     "Trying to get second character in one character base and one character added."
     #\b (vector-ref vec-to-use offset))))

(let* ((td "Three-piece hand-built buffer")
       (tb (piece-train-from-string "a"))
       (first-edit-piece (make-piece #t 0 1))
       (second-edit-piece (make-piece #t 1 1))
       (new-piece-list (append (piece-train-pt tb)
                               (list first-edit-piece second-edit-piece))))
  (status-msg 3 td)
  (piece-train-pt-set! tb new-piece-list)
  (%piece-train-append-edits! tb '(#\b #\c))
  (test-equal "Checking that tb was built correctly" "abc" (piece-train-summary tb))

  (let* ((my-res (%piece-train-offset->piece-and-offset tb 0))
         (piece (vector-ref my-res 0))
         (offset (vector-ref my-res 1))
         (vec-to-use (%vec-to-use tb piece)))
    (test-eqv "Trying to get first character in one character base and one character added."
              #\a (vector-ref vec-to-use offset)))

  (let* ((my-res (%piece-train-offset->piece-and-offset tb 1))
         (piece (vector-ref my-res 0))
         (offset (vector-ref my-res 1))
         (vec-to-use (%vec-to-use tb piece)))
    (test-eqv "Trying to get second character in one character base and two character added."
              #\b (vector-ref vec-to-use offset)))

  (let* ((my-res (%piece-train-offset->piece-and-offset tb 2))
         (piece (vector-ref my-res 0))
         (offset (vector-ref my-res 1))
         (vec-to-use (%vec-to-use tb piece)))
    (test-eqv "Trying to get third character in one character base and two character added."
              #\c (vector-ref vec-to-use offset))))

(let ((td "Three piece consecutive function-made buffer.")
      (tb (piece-train-from-string "a")))
  (status-msg 3 td)
  (piece-train-insert! tb 0 "b")
  (piece-train-insert! tb 1 "c")
  ;; NOTE: Not the usual order
  (test-equal "Testing that buffer was built correctly" "bca" (piece-train-summary tb))
  (status-msg 4 "Getting first character from offset 0")
  (let* ((my-res (%piece-train-offset->piece-and-offset tb 0))
         (piece (vector-ref my-res 0))
         (offset (vector-ref my-res 1))
         (vec-to-use (%vec-to-use tb piece)))
    (test-eqv "Trying to get first character in one character base with two characters added"
              #\b (vector-ref vec-to-use offset)))

  (status-msg 4 "Getting second character from offset 0 in edits (offset 1 in buffer)")
  (let* ((my-res (%piece-train-offset->piece-and-offset tb 1))
         (piece (vector-ref my-res 0))
         (offset (vector-ref my-res 1))
         (vec-to-use (%vec-to-use tb piece)))
    (test-eqv "Trying to get second character in one character base with two characters added"
              #\c (vector-ref vec-to-use offset)))

  (status-msg 4 "Getting third character from offset 2 (offset 0 in base)")
  (let* ((my-res (%piece-train-offset->piece-and-offset tb 2))
         (piece (vector-ref my-res 0))
         (offset (vector-ref my-res 1))
         (vec-to-use (%vec-to-use tb piece)))
    (test-eqv "Trying to get third character in one character base with two characters added"
              #\a (vector-ref vec-to-use offset))))

(let ((td "Three piece non-consecutive, function-made buffer")
      (tb(piece-train-from-string "a")))
  (status-msg 3 td)
  (piece-train-insert! tb 0 "c")
  (piece-train-insert! tb 0 "b")
  (test-equal "Testing that buffer was built correctly" "bca" (piece-train-summary tb))

  (status-msg 4 "Getting first character from offset 0")
  (let* ((my-res (%piece-train-offset->piece-and-offset tb 0))
         (piece (vector-ref my-res 0))
         (offset (vector-ref my-res 1))
         (vec-to-use (%vec-to-use tb piece)))
    (test-eqv "Trying to get first character in one character base with two characters added"
              #\b (vector-ref vec-to-use offset)))

  (status-msg 4 "Getting second character from offset 0 in edits (offset 1 in buffer)")
  (let* ((my-res (%piece-train-offset->piece-and-offset tb 1))
         (piece (vector-ref my-res 0))
         (offset (vector-ref my-res 1))
         (vec-to-use (%vec-to-use tb piece)))
    (test-eqv "Trying to get second character in one character base with two characters added"
              #\c (vector-ref vec-to-use offset)))

  (status-msg 4 "Getting third character from offset 2 in buffer (offset 0 in base)")
  (let* ((my-res (%piece-train-offset->piece-and-offset tb 2))
         (piece (vector-ref my-res 0))
         (offset (vector-ref my-res 1))
         (vec-to-use (%vec-to-use tb piece)))
    (test-eqv "Trying to get third character in one character base with two characters added"
              #\a (vector-ref vec-to-use offset))))

(test-end "2.2 %piece-train-offset->piece-and-offset")

(test-end "2. Internal Utilities")
(status-msg 1 "Done Testing Internal Utilities")

(println)
(status-msg 1 "Construction")
(test-begin "3. Construction")

(status-msg 2 "piece-train-from-string")

;; Many of the tests require that this procedure works, so there is
;; not much testing that occurs here.
(let ((tb (piece-train-from-string "A String")))
  (test-equal "A String" (piece-train-summary tb))
  (test-equal 8 (piece-train-length tb)))

(status-msg 2 "piece-train-from-file")

;; Assume that accessors work as documented. Suitable test
;; file must be present.
(let ((tb (piece-train-from-file "test.txt")))
  (test-equal "Some text.
A Line?
" (piece-train-summary tb))
  (test-equal 19 (piece-train-length tb)))

(test-end "3. Construction")
(status-msg 1 "Done Testing Construction")

(println)
(status-msg 1 "Buffer Length and Summary Tests")
(test-begin "4. Buffer Length and Summary")

(let ((tb (piece-train-from-string "Text")))
  (test-eqv "Length of four character base piece." 4 (piece-train-length tb))
  (test-equal "Summary of four character base piece."
              "Text" (piece-train-summary tb)))

(let* ((tb (piece-train-from-string "A Piece of"))
       (piece (make-piece #t 0 1))
       (new-piece (make-piece #t 0 5))
       (new-piece-list (append (piece-train-pt tb) (list new-piece))))
  (piece-train-pt-set! tb new-piece-list)
  (%piece-train-append-edits! tb '(#\space #\T #\e #\x #\t))
  (test-eqv "Length of one character base and one character edit."
            15 (piece-train-length tb))
  (test-equal "Summary of two piece base and edit."
              "A Piece of Text" (piece-train-summary tb)))

(test-end "4. Buffer Length and Summary")
(status-msg 1 "Done Testing Buffer Length and Summary")

(println)
(status-msg 1 "Character Retrieval")
(test-begin "5. Character Retrieval")

(let ((tb (piece-train-from-string "a")))
  (piece-train-append! tb "b")
  (test-equal "ab" (piece-train-summary tb))
  (test-equal "Retrieving from a two character buffer."
              #\a (piece-train-char-at tb 0))
  (test-equal "Retrieving 'b' from a two character buffer."
              #\b (piece-train-char-at tb 1)))

(test-end "5. Character Retrieval")
(status-msg 1 "Done testing Character Retrieval")

(println)
(status-msg 1 "Appends")
(test-begin "6. Appends")

(let ((td "Append to the base vector.")
      (tb (piece-train-from-string "Some Text")))
  (status-msg 2 td)
  (piece-train-append! tb " Appended")
  (test-equal "Some Text Appended" (piece-train-summary tb)))

(let* ((td "Append to the edits vector.")
       (tb (piece-train-from-string "A Piece of"))
       (piece (make-piece #t 0 1))
       (new-piece (make-piece #t 0 5))
       (new-piece-list (append (piece-train-pt tb) (list new-piece))))
  (status-msg 2 td)
  (piece-train-pt-set! tb new-piece-list)
  (%piece-train-append-edits! tb '(#\space #\T #\e #\x #\t))
  (piece-train-append! tb " Appended")
  (test-equal "A Piece of Text Appended" (piece-train-summary tb)))

(test-end "6. Appends")
(status-msg 1 "Done Testing Appends.")

(println)
(status-msg 1 "Inserts Testing")
(test-begin "7. Inserts")

(status-msg 2 "Inserts into a Short Base")
(test-begin "7.1 Insert into a Short Base")

(let ((td "At front of two character base")
      (tb (piece-train-from-string "ac")))
  (status-msg 3 td)
  (piece-train-insert! tb 0 "b")
  (test-equal td "bac" (piece-train-summary tb)))

(let ((td "Into middle of two character base")
      (tb (piece-train-from-string "ac")))
  (status-msg 3 td)
  (piece-train-insert! tb 1 "b")
  (test-equal td "abc" (piece-train-summary tb)))

(let ((td "At end of two-character base")
      (tb (piece-train-from-string "ac")))
  (status-msg 3 td)
  (piece-train-insert! tb 2 "b")
  (test-equal td "acb" (piece-train-summary tb)))

(test-end "7.1 Insert into a Short Base")

(status-msg 2 "Into a Longer Base")
(test-begin "7.2 Inserts into the middle of the piece-train.")

(let ((tb (piece-train-from-string "SomeText")))
  (piece-train-insert! tb 4 #\Z)
  (test-equal "Inserting a single character into the base character vector."
              "SomeZText" (piece-train-summary tb)))

(let ((tb (piece-train-from-string "SomeText")))
  (piece-train-insert! tb 4 (list #\Z))
  (test-equal "Inserting a list with a single character into the base character vector."
              "SomeZText" (piece-train-summary tb)))

(let ((tb (piece-train-from-string "SomeText")))
  (piece-train-insert! tb 4 "Z")
  (test-equal "Inserting a string with a single character into the base character vector."
              "SomeZText" (piece-train-summary tb)))

(test-end  "7.2 Inserts into the middle of the piece-train.")

(status-msg 2 "At the Beginning of the Buffer")
(test-begin "7.3. Inserts at the beginning of the piece-train.")

(let ((tb (piece-train-from-string "Some Text")))
  (piece-train-insert! tb 0 #\X)
  (test-equal "Inserting a single 'X' character at the beginning"
              "XSome Text" (piece-train-summary tb)))

(let ((tb (piece-train-from-string "Some Text")))
  (piece-train-insert! tb 0 '(#\X))
  (test-equal "Inserting a list with a single character at the beginning"
              "XSome Text" (piece-train-summary tb)))

(let ((tb (piece-train-from-string "Some Text")))
  (piece-train-insert! tb 0 "X")
  (test-equal "Inserting a string with a single character at the beginning"
              "XSome Text" (piece-train-summary tb)))

(test-end "7.3. Inserts at the beginning of the piece-train.")

(status-msg 2 "At the End of the Buffer")
(test-begin "7.4. Inserts at the end of the piece-train.")

(let ((tb (piece-train-from-string "Some Text")))
  (piece-train-insert! tb 9 #\Y)
  (test-equal "Inserting a single 'Y' character at the end."
              "Some TextY" (piece-train-summary tb)))

(let ((tb (piece-train-from-string "Some Text")))
  (piece-train-insert! tb 9 '(#\Y))
  (test-equal "Inserting a list with a single character at the end."
              "Some TextY" (piece-train-summary tb)))

(let ((tb (piece-train-from-string "Some Text")))
  (piece-train-insert! tb 9 ", Yuck")
  (test-assert (string=? "Some Text, Yuck" (piece-train-summary tb)))
  (test-equal "Inserting a multi-character string at the end."
              "Some Text, Yuck" (piece-train-summary tb)))

(test-end "7.4. Inserts at the end of the piece-train.")

(status-msg 2 "Multiple Consecutive Inserts")
(test-begin "7.5. Multiple consecutive inserts.")

(let ((tb (piece-train-from-string "Some")))
  (piece-train-insert! tb 4 " text to")
  (piece-train-insert! tb 12 " play with.")
  (test-equal "Inserting two additional pieces at end"
              "Some text to play with." (piece-train-summary tb)))

(let ((tb (piece-train-from-string "Some")))
  (piece-train-insert! tb 4 " play with.")
  (piece-train-insert! tb 4 " text to")
  (test-equal "Inserting new piece in the middle."
              "Some text to play with." (piece-train-summary tb)))

(let ((tb (piece-train-from-string "Some")))
  (piece-train-insert! tb 4 " play with.")
  (piece-train-insert! tb 4 " text to")
  (test-equal "Inserting string into middle of two-piece buffer"
              "Some text to play with." (piece-train-summary tb)))

(test-end "7.5. Multiple consecutive inserts.")

(status-msg 2 "Multiple Appends.")
(test-begin "7.6. Multiple appends")

(let ((tb (piece-train-from-string "Some")))
  (piece-train-append! tb " text to")
  (piece-train-append! tb " play with.")
  (test-equal "Appending two additional pieces at end"
              "Some text to play with." (piece-train-summary tb)))

(test-end "7.6. Multiple appends.")

(status-msg 2 "Deep Inserts")
(test-begin "7.7. Deep inserts.")

(let ((tb (piece-train-from-string "abc")))
  (piece-train-insert! tb 3 "def")
  (piece-train-insert! tb 3 "-ghij-")
  (piece-train-insert! tb 6 "-klmn-")
  (piece-train-insert! tb 9 "-opqr-")
  (test-equal "Deep insert"
              "abc-gh-kl-opqr-mn-ij-def" (piece-train-summary tb)))

(test-end "7.7. Deep inserts.")

(test-end "7. Inserts")
(status-msg 1 "Done Testing Inserts")

(println)
(status-msg 1 "Deletes")
(test-begin "8. Deletes")

(let ((td "Deleting a single character from the start of the base")
      (tb (piece-train-from-string "Text")))
  (status-msg 2 td)
  (piece-train-delete! tb 0 1)
  (test-equal td "ext" (piece-train-summary tb)))

(let ((td "Deleting two characters from the start of the base")
      (tb (piece-train-from-string "Text")))
  (status-msg 2 td)
  (piece-train-delete! tb 0 2)
  (test-equal td "xt" (piece-train-summary tb)))

(let ((td "Deleting part of an edit that starts the buffer")
      (tb (piece-train-from-string "Text")))
  (status-msg 2 td)
  (piece-train-insert! tb 0 "Some ")
  (test-equal "Some Text" (piece-train-summary tb))
  (piece-train-delete! tb 0 2)
  (test-equal td "me Text" (piece-train-summary tb)))

(let ((td "Completely deleting an edit at the start of a buffer")
      (tb (piece-train-from-string "Text")))
  (status-msg 2 td)
  (piece-train-insert! tb 0 "Some ")
  (piece-train-delete! tb 0 7)
  (test-equal td "xt" (piece-train-summary tb)))

(let ((td "Exactly deleting an edit at the start of a buffer")
      (tb (piece-train-from-string "Text")))
  (status-msg 2 td)
  (piece-train-insert! tb 0 "Some ")
  (piece-train-delete! tb 0 5)
  (test-equal td "Text" (piece-train-summary tb)))

(let ((td "Deleting a single character from the middle")
      (tb (piece-train-from-string "Text")))
  (status-msg 2 td)
  (piece-train-delete! tb 2 1)
  (test-equal td "Tet" (piece-train-summary tb)))

(let ((td "Deleting two characters from the start")
      (tb (piece-train-from-string "Text")))
  (status-msg 2 td)
  (piece-train-delete! tb 0 2)
  (test-equal td "xt" (piece-train-summary tb)))

(let ((td "Deleting across piece boundary")
      (tb (piece-train-from-string "Text")))
  (status-msg 2 td)
  (piece-train-insert! tb 4 " and more")
  (piece-train-delete! tb 2 6)
  (test-equal td "Te more" (piece-train-summary tb)))

;; FIX: Not really multiple piece boundaries since using append.
(let ((td "Deleting across multiple piece boundaries")
      (tb (piece-train-from-string "Text")))
  (status-msg 2 td)
  (piece-train-append! tb " and more")
  (piece-train-append! tb " to play with")
  (piece-train-delete! tb 2 16)
  (test-equal td "Telay with" (piece-train-summary tb)))

(let ((td "Deleting across 3 piece table boundaries")
      (tb (piece-train-from-string "Text")))
  (status-msg 2 td)
  (piece-train-append! tb " to play with")
  (piece-train-insert! tb 4 " and more")
  (piece-train-delete! tb 2 16)
  (test-equal td "Telay with" (piece-train-summary tb)))

(let ((td "Deleting a single character from the end of the base")
      (tb (piece-train-from-string "Text")))
  (status-msg 2 td)
  (piece-train-delete! tb 3 1)
  (test-equal td "Tex" (piece-train-summary tb)))

(let ((td "Deleting two characters from the end of a two-piece buffer")
      (tb (piece-train-from-string "Text")))
  (status-msg 2 td)
  (piece-train-append! tb " is good")
  (test-equal "Text is good" (piece-train-summary tb))
  (piece-train-delete! tb 10 2)
  (test-equal td "Text is go" (piece-train-summary tb)))

(let ((td "Deleting two characters from the end of a three piece buffer")
      (tb (piece-train-from-string "Text")))
  (status-msg 2 td)
  (piece-train-append! tb "good")
  (piece-train-insert! tb 4 " is ")
  (test-equal "Text is good" (piece-train-summary tb))
  (piece-train-delete! tb 10 2)
  (test-equal td "Text is go" (piece-train-summary tb)))

(let ((td "Deleting from the second piece to the end of a three-piece buffer")
      (tb (piece-train-from-string "Text")))
  (status-msg 2 td)
  (piece-train-append! tb "good")
  (piece-train-insert! tb 4 " is ")
  (test-equal "Text is good" (piece-train-summary tb))
  (piece-train-delete! tb 6 6)
  (test-equal td "Text i" (piece-train-summary tb)))

(let ((td "Deleting from the middle of a three piece buffer")
      (tb (piece-train-from-string "Text")))
  (status-msg 2 td)
  (piece-train-append! tb "good")
  (piece-train-insert! tb 4 " itches ")
  (test-equal "Text itches good" (piece-train-summary tb))
  (piece-train-delete! tb 6 4)
  (test-equal td "Text is good" (piece-train-summary tb)))

(let ((td "Deleting the entire contents of the buffer")
      (tb (piece-train-from-string "Some Text")))
  (status-msg 2 td)
  (piece-train-delete! tb 0 (piece-train-length tb))
  (test-equal td "" (piece-train-summary tb))
  (test-equal td 0 (piece-train-length tb)))

(test-end "8. Deletes")
(status-msg 1 "Done Testing Deletes")

(test-begin "9. Summaries")

;; Since `piece-train-summary` is used in so many of the other
;; tests, we assume that is adequate testing.

(test-end "9. Summaries")

(println)
(status-msg 1 "Sequences")
(test-begin "10. Sequences")

(status-msg 2 "Base Sequence")
(test-begin "10.1 Base Sequence")

(let* ((test-str "A Sequence")
       (td "Getting the entire base sequence")
       (nothing (status-msg 3 td))
       (tb (piece-train-from-string test-str))
       (seq (piece-train-sequence-at tb 0 (string-length test-str))))
  (test-equal td test-str seq))

(let* ((td "Getting the beginning of the base sequence")
       (test-str "Two Words")
       (tb (piece-train-from-string test-str)))
  (status-msg 3 td)
  (test-equal td "Two" (piece-train-sequence-at tb 0 3)))

(let* ((td "Getting the end of the base sequence")
       (tb (piece-train-from-string "Two Words")))
  (status-msg 3 td)
  (test-equal td "Words" (piece-train-sequence-at tb 4 5)))

(let* ((td "Getting the middle of the base sequence")
       (tb (piece-train-from-string "Three More Words")))
  (status-msg 3 td)
  (test-equal td "More" (piece-train-sequence-at tb 6 4)))

(test-end "10.1 Base Sequence")

(status-msg 2 "Appended Sequence")
(test-begin "10.2 Appended Sequence")

(let* ((td "Getting the entire appended sequence")
       (jumbung (status-msg 3 td))
       (test-str "A Sequence")
       (test-str2 " Appended")
       (tb (piece-train-from-string test-str)))
  (piece-train-append! tb test-str2)
  (test-equal td (string-append test-str test-str2) "A Sequence Appended")
  (test-equal td test-str2 (piece-train-sequence-at tb 10 9)))

(let* ((td  "Getting the beginning of the appended sequence")
       (tb (piece-train-from-string "Two")))
  (status-msg 3 td)
  (piece-train-append! tb " Words")
  (test-equal td " Wo" (piece-train-sequence-at tb 3 3)))

(let* ((td "Getting the end of the appended sequence")
       (tb (piece-train-from-string "Two ")))
  (status-msg 3 td)
  (piece-train-append! tb "Words")
  (test-equal td "Words" (piece-train-sequence-at tb 4 5)))

(let* ((td "Retrieving across base and appended edit")
       (tb (piece-train-from-string "Two")))
  (status-msg 3 td)
  (piece-train-append! tb " Words")
  (test-equal td "o W" (piece-train-sequence-at tb 2 3)))

(let ((td "Retrieving from inserted piece back into base")
      (tb (piece-train-from-string "A piece of text")))
  (status-msg 3 td)
  (piece-train-insert! tb 1 " long")
  (test-equal td "A long piece of text" (piece-train-summary tb))
  (test-equal td "ong pi" (piece-train-sequence-at tb 3 6)))

(let ((td "Retrieving from base to insert to base again")
      (tb (piece-train-from-string "A piece text")))
  (status-msg 3 td)
  (piece-train-insert! tb 8 "of ")
  (test-equal "Assure correct test string assembly"
              "A piece of text" (piece-train-summary tb))
  (test-equal td "e of t" (piece-train-sequence-at tb 6 6)))

(let ((td "Retrieving across text deleted from base")
      (tb (piece-train-from-string "A piece of text")))
  (status-msg 3 td)
  (piece-train-delete! tb 2 9)
  (test-equal "Assure correct test string assembly"
              "A text" (piece-train-summary tb))
  (test-equal td "A text" (piece-train-sequence-at tb 0 6))
  (test-equal td "ex" (piece-train-sequence-at tb 3 2)))

(let ((td "Retrieving across multiple inserts")
      (tb (piece-train-from-string "play")))
  (piece-train-append! tb " with")
  (piece-train-insert! tb 0 "Some")
  (piece-train-insert! tb 4 " to ")
  (piece-train-insert! tb 4 " text")
  (test-equal "Assure correct test string assembly"
              "Some text to play with"
              (piece-train-summary tb))
  (test-equal td "text to play" (piece-train-sequence-at tb 5 12)))

(let ((td "Retrieving across multiple edit pieces")
      (tb (piece-train-from-string "A piece of text")))
  (status-msg 3 td)
  (piece-train-append! tb " now")
  (piece-train-insert! tb 1 " long")
  (piece-train-delete! tb 6 9)
  (piece-train-insert! tb 11 " here")
  (piece-train-insert! tb 11 " is")
  (test-equal "Assure correct test string assembly"
              "A long text is here now" (piece-train-summary tb))
  (test-equal td "text is here" (piece-train-sequence-at tb 7 12)))

(test-end "10.2 Appended Sequence")

(test-end "10. Sequences")
(status-msg 1 "Done Testing Sequences")

(println)

(test-end "piece-train")
