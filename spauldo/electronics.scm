(define-module (spauldo electronics)
  #:use-module (srfi srfi-1)
  #:use-module (spauldo constants electical-constants)
  #:export (voltage-divider ohm-i ohm-v ohm-r
			    power-vi power-ir power-vr
			    cap-charge cap-energy
			    series-resistors parallel-resistors
			    series-capacitors parallel-capacitors
			    rc-time-constant
			    rc-charge-current rc-discharge-current
			    rc-charge-voltage rc-discharge-voltage
			    rc-charge-charge rc-discharge-charge))

;;; --------- Exported Functions ---------

;;; --- Ohm's Law ---

(define (ohm-i volts Ω)
  (/ (exact->inexact volts) Ω))

(define (ohm-v amps Ω)
  (* amps Ω))

(define (ohm-r volts amps)
  (/ (exact->inexact volts) amps))

;;; Power
(define (power-vi volts amps)
  (* volts amps))

(define (power-ir amps Ω)
  (* amps amps Ω))

(define (power-vr volts Ω)
  (/ (* volts volts) Ω))

;;; --- Capacitance ---
;;; All arguments labeled "capacitors" take any number of capacitances in farads

(define (cap-charge farads volts)
  (* farads volts))

(define (cap-energy farads volts)
  (* 1/2 farads volts volts))

(define (series-capacitors . capacitors)
  (cond ((null? capacitors)
         0)
        ((< (length capacitors) 2)
         (car capacitors))
        (#t
         (/ 1.0 (fold add-reciprocal 0 capacitors)))))

(define (parallel-capacitors . capacitors)
  (fold + 0 capacitors))

;;; --- Resistance ---
;;; All arguments labeled "resistors" take any number of resistances in ohms

(define (series-resistors . resistors)
  (fold + 0 resistors))

(define (parallel-resistors . resistors)
  (cond ((null? resistors)
         0)
        ((< (length resistors) 2)
         (car resistors))
        (#t
         (/ 1.0 (fold add-reciprocal 0 resistors)))))

;;; --- RC Circuits ---
;;; v0 is the voltage across the resistor at time=0 for charging
;;; vCi is the voltage across the capacitor at time=0 for discharging

(define (rc-time-constant Ω farads)
  (* Ω farads))

(define (rc-charge-current time v0 Ω farads)
  (* (/ v0 Ω) (exp (/ (- time) (rc-time-constant Ω farads)))))

(define (rc-charge-voltage time v0 Ω farads)
  (* v0 (- 1 (exp (/ (- time) (rc-time-constant Ω farads))))))

(define (rc-charge-charge time v0 Ω farads)
  (* farads v0 (- 1 (exp (/ (- time) (rc-time-constant Ω farads))))))

(define (rc-discharge-current time vCi Ω farads)
  (* (/ vCi Ω) (exp (/ (- time) (rc-time-constant Ω farads)))))

(define (rc-discharge-voltage time v0 Ω farads)
  (* vCi (exp (/ (- time) (rc-time-constant Ω farads)))))

(define (rc-discharge-charge time v0 Ω farads)
  (* farads vCi (exp (/ (- time) (rc-time-constant Ω farads)))))

;;; --- Misc ---

;;; Voltage Divider
;;; Note r1 is connected to the more positive supply
(define (voltage-divider volts-in r1-Ω r2-Ω)
  (exact->inexact (* volts-in (/ r2-Ω (+ r1-Ω r2-Ω)))))

;;; --------- Non-Exported Functions ---------

;;; Add the reciprocal of n to total
;;; Used in some calculations
(define (add-reciprocal n total)
    (+ total (/ 1.0 n)))
