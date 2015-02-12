;;;
;;; sprite.lisp -- Simple sprite management routines.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :game-fetus-alpha)

;;;; General Sprites

(defclass sprite ()
  ((image :accessor sprite-image)
   (blit-offset :accessor sprite-blit-offset)
   (frames :accessor sprite-frames)
   (animations :accessor sprite-animations)
   (x :accessor sprite-x)
   (y :accessor sprite-y)
   (priority :accessor sprite-priority)
   (cur-anim :accessor sprite-cur-anim)
   (cur-frame :accessor sprite-cur-frame)
   (frame-counter :accessor sprite-frame-counter))
  (:documentation "An animated display object, with a concept of
priority and screen position."))


(defun new-sprite-from-alist (alist)
  "Creates a new sprite from an alist with keys :image, :frames, and
:animations."
  (let ((sprite (make-instance 'sprite)))
    (setf (sprite-image sprite)
	  (load-image (cadr (assoc :image alist)) t))
    (setf (sprite-blit-offset sprite) (cadr (assoc :blit-offset alist)))
    (setf (sprite-frames sprite) (cadr (assoc :frames alist)))
    (setf (sprite-animations sprite) (cadr (assoc :animations alist)))
    (setf (sprite-x sprite) 0 (sprite-y sprite) 42 ; XXX sane defaults?
	  (sprite-priority sprite) 0
	  (sprite-cur-anim sprite) (caar (sprite-animations sprite))
	  (sprite-cur-frame sprite) 0
	  (sprite-frame-counter sprite) 0)
    sprite))

(defun draw-sprite (sprite)
  "Draw a sprite's current frame and update animations."
  ;; get current frame
  (let ((flist (cdr (assoc (sprite-cur-anim sprite)
			   (sprite-animations sprite))))
	(u (sprite-x sprite))
	(v (sprite-y sprite)))
    (blit-image (sprite-image sprite) u v
		:src-rect (nth (car (nth (sprite-cur-frame sprite)
					 flist))
			       (sprite-frames sprite)))
    ;; update frame
    (decf (sprite-frame-counter sprite))
    (when (minusp (sprite-frame-counter sprite))
      (incf (sprite-cur-frame sprite))
      (when (>= (sprite-cur-frame sprite) (length flist))
	(setf (sprite-cur-frame sprite) 0))
      (setf (sprite-frame-counter sprite)
	    (cdr (nth (sprite-cur-frame sprite) flist))))))


(defun set-sprite-animation (sprite anim)
  "Set the current animation associated with SPRITE.  Resets the
sprite's frame count if ANIM is different from the current animation.
Returns T if the animation was updated, NIL otherwise."
  (unless (eql anim (sprite-cur-anim sprite))
    (let ((flist (cdr (assoc anim (sprite-animations sprite)))))
      (setf (sprite-cur-anim sprite) anim
	    (sprite-cur-frame sprite) 0
	    (sprite-frame-counter sprite) (cdar flist))
      t)))

(fetus/test:define-screencap-comparison-test
    (sprite-loads-and-draws-correctly-at-various-positions)
  (let ((s (new-sprite-from-alist '((:image "t/irongate.pcx")
                                    (:blit-offset (0 . 0))
                                    (:frames ((0 0 32 96)))
                                    (:animations ((:default (0 . 60))))))))
    (draw-sprite s)
    (setf (sprite-x s) 300
          (sprite-y s) 10)
    (draw-sprite s)
    (setf (sprite-x s) -10
          (sprite-y s) 180)
    (draw-sprite s)
    (setf (sprite-x s) 320
          (sprite-y s) 200)
    (draw-sprite s)))
