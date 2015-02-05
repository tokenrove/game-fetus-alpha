(in-package :game-fetus-alpha)

;;; XXX Gross
(defvar *frame-end* 0)

(defun timer-get-ticks () (sdl:get-ticks))
(defun timer-start-frame (frame-length)
  (setf *frame-end* (+ (sdl:get-ticks) frame-length)))
(defun timer-end-frame ()
  (let ((now (sdl:get-ticks)))
    (when (< now *frame-end*)
      (sdl:delay (- *frame-end* now)))))
