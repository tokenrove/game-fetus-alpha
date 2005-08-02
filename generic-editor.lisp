;;; Generic editor routines, for GFa tilemap editor, aequus noctis
;;; room editor, and whatever.
;;;
;;; Julian Squires <tek@wiw.org> / 2004

(in-package :game-fetus-alpha)

;;;; USER-INTERFACE ROUTINES

(defun prompt-for-yes-or-no (font message)
  (loop
     (draw-status-message font message 128 128 128)
     (fetus:refresh-display)
   (let ((event (fetus:get-key-event)))
     (cond ((= event (char-code #\y)) (return t))
	   ((= event (char-code #\n)) (return nil))))))


(defun prompt-for-string (font message &key (symbol-mode nil))
  (do ((string (make-array '(10) :element-type 'base-char
			   :fill-pointer 0 :adjustable t)))
      (nil)
    (draw-status-message font (format nil "~A~A" message string) 128 128 128)
    (refresh-display)
    (let ((event (get-key-event)))
      (cond ((= event 13) (return string))
	    ((<= (char-code #\Space) event (char-code #\~))
	     (let ((char (code-char event)))
	       (when symbol-mode
		 (setf char (char-upcase char))
		 (when (eql char #\Space) (setf char #\-)))
	       (vector-push-extend char string)))
	    ((= event +keysym-backspace+)
	     (unless (zerop (fill-pointer string))
	       (vector-pop string)))))))


(defun prompt-for-integer (font message)
  (do ((number 0))
      (nil)
    (draw-status-message font (format nil "~A~A" message number) 128 128 128)
    (refresh-display)
    (let ((event (get-key-event)))
      (cond ((= event 13) (return number))
	    ((< 47 event 58) (setf number (+ (* number 10) (- event 48))))
	    ((= event 8) (setf number (floor number 10)))))))


(defun draw-status-message (font message r g b)
  (draw-filled-rectangle 8 (- (display-height) 32) (- (display-width) 16) 24
			 r) ;(gfx-map-rgb r g b)
  (draw-rectangle 8 (- (display-height) 32) (- (display-width) 16) 24
		  32) ;(gfx-map-rgb 32 32 32)
  (paint-string font message 10 (- (display-height) 30) 255 255 255))

