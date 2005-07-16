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
  ;; XXX set correct frame count, etc etc
  (unless (eql anim (sprite-cur-anim sprite))
    (setf (sprite-cur-anim sprite) anim)
    (setf (sprite-cur-frame sprite) 0)
    t))

;;;; Sprite Manager

(defclass sprite-manager ()
  ((sprites :accessor sprite-manager-sprites)
   (cmp-fn :accessor sprite-manager-cmp-fn)))

(defun create-sprite-manager (cmp-fn)
  "Creates a 2D sprite manager, which deals with keeping track of, and
drawing, sprites.  CMP-FN designates a comparison function by which
the sprites will be sorted during UPDATE-ALL-SPRITES.  See that
function documentation for details."
  (let ((manager (make-instance 'sprite-manager)))
    (setf (sprite-manager-sprites manager) nil)
    (setf (sprite-manager-cmp-fn manager) cmp-fn)
    manager))

(defun destroy-sprite-manager (manager)
  "Destroys MANAGER, and any sprites associated with it.  Note that
this means that those sprites must not be used again after this
function has been called."
  (dolist (sprite (sprite-manager-sprites manager))
    (awhen (sprite-image sprite)
      (free-image it))
    (setf (sprite-image sprite) nil)))

(defun add-sprite-to-manager (manager sprite)
  "Registers SPRITE in MANAGER to be drawn on calls to
UPDATE-ALL-SPRITES, and destroyed when DESTROY-SPRITE-MANAGER is
called."
  (push sprite (sprite-manager-sprites manager)))

;; XXX should we also free-image the sprite here?
(defun remove-sprite-from-manager (manager sprite)
  "Unregisters SPRITE, such that it will no longer be drawn, nor will
be it destroyed when DESTROY-SPRITE-MANAGER is called."
  (setf (sprite-manager-sprites manager)
	(remove sprite (sprite-manager-sprites manager))))

(defun update-all-sprites (manager)
  "Sorts and draws all sprites registered with MANAGER.  The
comparison function associated with MANAGER should be a predicate
which is passed the SPRITE-PRIORITY associated with the two sprites to
be compared.  The user is responsible for setting the SPRITE-PRIORITY
of their sprites with whatever data they need to effectively compare
two sprites."
  ;; XXX could be a lot more efficient if we cared.
  (awhen (sprite-manager-cmp-fn manager)
    (setf (sprite-manager-sprites manager)
	  (stable-sort (sprite-manager-sprites manager)
		       it
		       :key #'sprite-priority)))

  (dolist (sprite (sprite-manager-sprites manager))
    (draw-sprite sprite)))


(defmacro with-sprite-manager ((manager-var sort-fn) &body body)
  `(let ((,manager-var (create-sprite-manager ,sort-fn)))
     (unwind-protect
	  (progn ,@body)
       (destroy-sprite-manager ,manager-var))))