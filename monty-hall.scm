#!/usr/bin/guile \
-e start -s
!#

;;; monty-hall.scm --- Monty Hall problem simulator
;;;
;;; For information on the Monty Hall problem, see
;;; https://en.wikipedia.org/wiki/Monty_Hall_problem
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


(use-modules (srfi srfi-43))

;;; Our random state
(define rand-state (random-state-from-platform))

;;; Simulate a Monty Hall scenario
;;; change should be #t if the player changes doors, #f if the player sticks
;;; with the first door chosen.
(define (monty change)
  (let ((winning-door (random 3 rand-state))    ; Door with the prize
        (player-choice (random 3 rand-state))   ; Door the player chooses
        (monty-doors #f)                        ; Doors Monty can choose to open
        (opened-door #f))                       ; Door Monty opens

    ;; The doors Monty can choose to open
    (set! monty-doors (list->vector (delete player-choice
                                            (delete winning-door '(0 1 2)))))

    ;; The door Monty chooses to open
    (set! opened-door
          (vector-ref monty-doors
                      (random (vector-length monty-doors) rand-state)))

    ;; Does the player change?
    (when change
      (set! player-choice (car (delete opened-door
                                       (delete player-choice '(0 1 2))))))

    (= winning-door player-choice)))

(define (run-monty n change)
  (define (rm-helper i total)
    (if (< i 0)
        (/ total n)
        (rm-helper (1- i) (if (monty change)
                              (1+ total)
                              total))))

  (exact->inexact (rm-helper n 0)))

(define (start args)
  (let ((trials (if (and (> (length args) 1)
                         (integer? (string->number (cadr args))))
                    (string->number (cadr args))
                    1000)))
    (format #t "Using ~A trials for each of the following: " trials)
    (newline)
    (format #t "  Win rate if player changes doors:      ~A" (run-monty trials #t))
    (newline)
    (format #t "  Win rate if player keeps first choice: ~A" (run-monty trials #f))
    (newline)))
