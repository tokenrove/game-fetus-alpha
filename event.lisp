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
  (defconstant +scancode-mask+ (ash 1 30))
  (defconstant +keysym-backspace+ 8)
  (defconstant +keysym-escape+ 27)
  (defconstant +keysym-q+ 113)
  (defconstant +keysym-x+ 120)
  (defconstant +keysym-z+ 122)
  (defconstant +keysym-up+ (logior 82 +scancode-mask+))
  (defconstant +keysym-down+ (logior 81 +scancode-mask+))
  (defconstant +keysym-right+ (logior 79 +scancode-mask+))
  (defconstant +keysym-left+ (logior 80 +scancode-mask+))

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

(defun wipe-events ()
  "Clears any ghost events that might be in the event map because of
an interruption or similar."
  (setf *event-map* (bit-xor *event-map* *event-map*)))


;; XXX refactor
(defun event-update ()
  "function EVENT-UPDATE

Checks for events, updates internal input state."
  (sdl:with-event (event)
    (sdl:pump-events)
    (loop while (sdl:poll-event event)
          do (let ((type (sdl:event-type event)))
               (case type
                 ((:key-down :key-up)
                  (awhen (gethash (sdl:event-keysym event) *xlate-symbol->map-idx*)
                    (setf (bit *event-map* it) (if (eql type :key-down) 1 0))))
                 (:quit
                  (setf (bit *event-map* +ev-quit+) 1)))))))


(defun event-pressedp (event)
  "Returns T if EVENT is pressed, NIL otherwise."
  (= (bit *event-map* (ecase event
                        (:quit 0)
                        (:up 1)
                        (:down 2)
                        (:left 3)
                        (:right 4)
                        (:button-a 5)
                        (:button-b 6))) 1))


(defun get-key-event ()
  (sdl:with-event (event)
    (loop do (sdl:wait-event event)
          until (eql (sdl:event-type event) :key-down))
    (sdl:event-keysym event)))

#+5am
(5am:test (events :suite fetus:acceptance)
  (with-display ()
    (fetus/os:with-directory-of-system (:game-fetus-alpha)
      (with-font (font "./t/f500.ttf" 18)
        (wipe-events)
        (loop
          (clear-display)
          (event-update)
          (paint-string font "Testing events..." 0 0 0 0 255)
          (when (event-pressedp :quit)
            (return nil))
          (loop for key in '(:left :right :up :down :button-a :button-b)
                for y from 0 by 24
                do (when (event-pressedp key)
                     (paint-string font (symbol-name key) 30 y 0 255 0)))
          (present-display)))))
  (5am:pass))
