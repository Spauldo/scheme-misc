;;; electrical-constants.scm
;;;
;;; This module contains various constants used in electronics
;;;
;;; Do note that I have made gratuitous use of unicode here.  I am fully aware
;;; that there is a common view that programs should not deviate from ASCII in
;;; code, and I generally follow that rule.  However, I am writing this for
;;; myself and prefer using the occasional non-ASCII character for mathematics
;;; and science.  I will accept no pull requests "correcting" this.  I will
;;; consider pull requests that change some of these if I have used the
;;; characters incorrectly.
;;;
;;; Sadly, subscripting and superscripting in Unicode do not use combining
;;; characters, and Unicode has a limited subset of subscripted characters
;;; available.  I have decided to not use subscripts in variable names.
;;;
;;; For people who are using Emacs, most of these are easily available by
;;; toggling your input method to TeX and using TeX codes such as \phi or
;;; \hbar.  For variables using unicode combining characters (such as overline),
;;; you type the character and then follow it by inserting the unicode combining
;;; character (C-x 8 [ret] -> select the character name).
;;;
;;; If you're coding in Scheme (or any LISP) without Emacs...  why?
;;;
;;;
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

(define-module (spauldo constants electrical-constants)
  #:autoload (spauldo constants math-constants) (π)
  #:autoload (spauldo constants physics-constants) (α ℏ c elem-charge)
  #:export (ke))

;;; Coulomb's constant
(define ke
  (* α (/ (* ℏ c) (* elem-charge elem-charge))))
