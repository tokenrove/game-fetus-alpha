;;; event.lisp -- Event handler code.
;;;
;;; Very basic SDL event handling, mapping some keys to symbols.
;;;
;;; Need to:
;;; * implement "console" mode and "keyboard" mode, and allow poll or
;;; wait in either mode.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :game-fetus-alpha)

;; Who could need any events other than these?
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +ev-quit+ 0)
  (defconstant +ev-up+ 1)
  (defconstant +ev-down+ 2)
  (defconstant +ev-left+ 3)
  (defconstant +ev-right+ 4)
  (defconstant +ev-button-a+ 5)
  (defconstant +ev-button-b+ 6)
  (defconstant +last-ev-code+ 8)

  ;; SDL keysym names; really, I'd rather grovel them, but for the
  ;; moment, this works.
  (defconstant +keysym-backspace+ 8)
  (defconstant +keysym-escape+ 27)
  (defconstant +keysym-q+ 113)
  (defconstant +keysym-x+ 120)
  (defconstant +keysym-z+ 122)
  (defconstant +keysym-up+ 273)
  (defconstant +keysym-down+ 274)
  (defconstant +keysym-right+ 275)
  (defconstant +keysym-left+ 276)

  ;; Eventually we're going to need to support ideas like having more
  ;; than one controller.
  (let ((hash (make-hash-table)))
    (dolist (x  (list (cons +keysym-escape+ +ev-quit+)
		      (cons +keysym-q+ +ev-quit+)
		      (cons +keysym-up+ +ev-up+)
		      (cons +keysym-down+ +ev-down+)
		      (cons +keysym-left+ +ev-left+)
		      (cons +keysym-right+ +ev-right+)
		      (cons +keysym-z+ +ev-button-a+)
		      (cons +keysym-x+ +ev-button-b+)))
      (setf (gethash (car x) hash) (cdr x)))

    (defparameter *xlate-symbol->map-idx* hash
      "Translates an SDL KeySym value to a symbolic *event-map* key.")))

(defvar *event-map* (make-sequence 'simple-bit-vector +last-ev-code+)
  "Hash which tracks whether or not a button is being pressed.")


;;;; API-LEVEL ROUTINES.

;;; Any point having a with-event-handling macro?  I think this is
;;; probably real first-init stuff, and probably doesn't need a with-
;;; wrapper.
(defun event-init ()
  (ll-event-init))

(defun event-shutdown ()
  (ll-event-shutdown))


(defun wipe-events ()
  "Clears any ghost events that might be in the event map because of
an interruption or similar."
  (setf *event-map* (bit-xor *event-map* *event-map*)))


;; XXX refactor
(defun event-update ()
  "function EVENT-UPDATE

Checks for events, updates internal input state."
  (cffi:with-foreign-object (event 'll-event)
    (do* ((rv #1=(ll-poll-event event) #1#))
	 ((= rv 0))
      (cffi:with-foreign-slots ((type value axis) event ll-event)
	(case type
	  (:key-down
	   (awhen (gethash value *xlate-symbol->map-idx*)
	     (setf (bit *event-map* it) 1)))
	  (:key-up
	   (awhen (gethash value *xlate-symbol->map-idx*)
	     (setf (bit *event-map* it) 0)))
	  (:joy-move
	   (case axis
	     (0 (cond ((plusp value)
		       (setf (bit *event-map* +ev-right+) 1
			     (bit *event-map* +ev-left+) 0))
		      ((minusp value)
		       (setf (bit *event-map* +ev-right+) 0
			     (bit *event-map* +ev-left+) 1))
		      (t
		       (setf (bit *event-map* +ev-right+) 0
			     (bit *event-map* +ev-left+) 0))))
	     (1 (cond ((plusp value)
		       (setf (bit *event-map* +ev-down+) 1
			     (bit *event-map* +ev-up+) 0))
		      ((minusp value)
		       (setf (bit *event-map* +ev-down+) 0
			     (bit *event-map* +ev-up+) 1))
		      (t
		       (setf (bit *event-map* +ev-down+) 0
			     (bit *event-map* +ev-up+) 0))))))
	  (:joy-button-down
	   (awhen (case value
		    (0 +ev-button-a+)
		    (1 +ev-button-b+))
	     (setf (bit *event-map* it) 1)))
	  (:joy-button-up
	   (awhen (case value
		    (0 +ev-button-a+)
		    (1 +ev-button-b+))
	     (setf (bit *event-map* it) 0))))))))


(defun event-pressedp (event)
  "Returns T if EVENT is pressed, NIL otherwise."
  (= (bit *event-map* event) 1))


(defun get-key-event ()
  (cffi:with-foreign-object (event 'll-event)
    (do ((rv #1=(ll-wait-event event) #1#))
	((= rv 0))
      (cffi:with-foreign-slots ((type value) event ll-event)
	(when (eql type :key-down)
	  (return value))))))
