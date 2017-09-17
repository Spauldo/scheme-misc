#!/usr/bin/guile \
-e start -s
!#

;;; print-field.scm --- Read whitespace-separated lines and output a specific
;;;                     field.
;;;
;;; This was written as an exercise in response to a question on StackExchange.
;;; Works kinda like awk '{print $n};', except not nearly as flexible or useful.
;;;
;;; Copyright (c) 2017 Jeff Spaulding <sarnet@gmail.com>
;;;
;;; Permission to use, copy, modify, and distribute this software for any
;;; purpose with or without fee is hereby granted, provided that the above
;;; copyright notice and this permission notice appear in all copies.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(use-modules (rnrs io ports))

;;; Reads one line from current-input-port and prints the field indicated by
;;; field-num.  If the line does not have enough fields, it prints a newline.
;;; Returns the field, an empty string, or #f if end of file is reached.
(define (get-field field-num)
  (let ((line (get-line (current-input-port))))
    (if (eof-object? line)
        #f
        (let ((fields (string-tokenize line)))
          (if (< field-num (length fields))
              (let ((field (list-ref fields field-num)))
                (put-string (current-output-port)
                            (string-append field "\n"))
                field)
              (and (put-string (current-output-port) "\n")
                   ""))))))

;;; Repeat get-field until until end of file is reached
(define (get-all-fields field-num)
  (if (get-field field-num)
      (get-all-fields field-num)
      #f))

(define (start args)
  (if (and (> (length args) 1)
           (integer? (string->number (list-ref args 1))))
      (get-all-fields (1- (string->number (list-ref args 1))))
      (display (string-join
                `("Usage:" ,(list-ref args 0) "<field-number>\n")
                " "))))
