;;------------------------------------------------------------------------------
;; Procedures for convenient printing.
;;

(library (easy-io)
  (export char-control?
          char->readable
          string->readable
          println
          println-in-raw)
  (import (chezscheme))

  ;; Return #t if the character is an ASCII control character,
  ;; false otherwise.
  (define (char-control? c)
    (let ((char-num (char->integer c)))
      (or (= char-num 127)
          (< char-num 32))))

  ;; If c is a control character, it is converted into a two-character
  ;; string that indicates its special status by preceding it with a "^".
  (define (char->readable c)
    (if (char-control? c)
        (string #\^ (integer->char (+ 64 (char->integer c))))
        (string c)))

  ;; Return a transform of the input string with embedded control chars converted
  ;; to human-readable form showing a "^" prepended to it.
  (define (string->readable s)
    (let loop ((lst (string->list s))
               (acc ""))
      (if (null? lst)
          acc
          (loop (cdr lst) (string-append acc (char->readable (car lst)))))))

  ;; Display a series of arguments followed by a newline.
  (define (println . args)
    (for-each display args)
    (newline))

  ;; When in raw mode, lines need to be ended with CR/LF pairs to act
  ;; like normal printing.
  (define (println-in-raw . args)
    (for-each display args)
    (display (string #\return #\newline))))
