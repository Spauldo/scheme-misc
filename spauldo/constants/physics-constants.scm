;;; physics-constants.scm
;;;
;;; This module contains various constants used in physics
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

(define-module (spauldo constants physics-constants)
  :autoload (spauldo constants math-constants)
  :export (c Na G R̅ kB elem-charge ε0 μ0 h ℏ α))

;;; Speed of Light in a Vacuum (meters per second)
(define c
  299792458)

;;; Avogadro's constant
;;; Using value from the International Avogadro Coordination in 2011
(define Na
  (* 6.02214078 (expt 10 23)))

;;; Gravitational constant
;;; Using 2014 CODATA value
(define G
  (* 6.67408 (expt 10 -11)))

;;; Gas constant
;;; Variable name is 'R' followed by U+0305 COMBINING OVERLINE
(define R̅
  8.3144598)

;;; Boltzmann constant
(define kB
  (/ R̅ Na))

;;; Elementary Charge constant (e)
(define elem-charge
  (* 1.6021766208 (expt 10 -19)))

;;; Vacuum Permittivity constant
(define ε0
  (* 8.854187817 (expt 10 -12)))

;;; Vacuum Permeability constant (i.e. Magnetic constant)
(define μ0
  (* 4 π (expt 10 -7)))

;;; Planck constant
(define h
  (* 6.626070040 (expt 10 -34)))

;;; Reduced Planck constant (\hbar in LaTeX)
(define ℏ
  (/ h (* 2 π)))

;;; Fine Structure constant
(define α
  (* (/ 1 (* 4 π ε0))
     (/ (* elem-charge elem-charge) (* ℏ c))))
