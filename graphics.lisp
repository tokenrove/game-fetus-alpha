;;;
;;; graphics.lisp -- Basic graphics primitives.
;;;
;;; As-simple-as-possible SDL implementations, generally.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :game-fetus-alpha)

(defparameter *desired-width* 320)
(defparameter *desired-height* 240)
(defparameter *desired-bpp* 8)

(defvar *vbuffer* nil
  "Pointer to video double buffer.")


(defun create-display (&key (fullscreen-p nil) (scale 1))
  "function CREATE-DISPLAY &key fullscreen-p scale => t

Initialize the display, optionally fullscreen (beware setting this
option on undebugged code!)."
  (setf *vbuffer*
	(maybe-null->nil
	 (ll-gfx-init fullscreen-p scale
		      *desired-width* *desired-height* *desired-bpp*)))
  (assert *vbuffer*)
  t)

(defun new-image-buffer (w h)
  "function NEW-IMAGE-BUFFER w h => surface

Creates a new graphics surface with the specified dimensions, and the
same format as the current display.  Returns the new surface, or NIL
if something went wrong."
  (maybe-null->nil (ll-gfx-new-image-buffer w h)))

(defun destroy-display ()
  "Shuts down the display and frees the double buffer."
  (ll-gfx-shutdown *vbuffer*))

(defun refresh-display ()
  "Flips the double buffer, updates current display."
  (ll-gfx-refresh-display *vbuffer*))

(defun surface-w (sface) (ll-gfx-surface-w sface))
(defun surface-h (sface) (ll-gfx-surface-h sface))
(defun display-width () (surface-w *vbuffer*))
(defun display-height () (surface-h *vbuffer*))

(defun fill-background (color &optional (destination *vbuffer*))
  "Fills the destination with a solid color."
  (ll-gfx-fill-rect destination (cffi:null-pointer) color))

(defun load-image (filename &optional (colorkeyp nil))
  "function LOAD-IMAGE filename &optional colorkeyp => surface

Loads an image (in pretty much any sane format), optionally with color
zero flagged as transparent (when colorkeyp)."
  (and filename
       (maybe-null->nil (ll-gfx-load-image filename colorkeyp))))

(defun free-image (image)
  "Deallocates resources used by an image surface."
  (when image (ll-gfx-free-surface image)))

(defun use-image-palette (image)
  "function USE-IMAGE-PALETTE image

Sets the current display palette to whatever image is carrying around
with it."
  (when image (ll-gfx-use-image-palette image *vbuffer*)))

(eval-when (:compile-toplevel)
  (defmacro with-gfx-rect-from-list ((var list) &body body)
    `(cffi:with-foreign-object (,var 'gfx-rect)
      (loop for slot in '(x y w h)
            for value in ,list
            do (setf (cffi:foreign-slot-value ,var 'gfx-rect slot) value))
      ,@body))
  (defmacro with-gfx-rect-boa ((var x y w h) &body body)
    `(cffi:with-foreign-object (,var 'gfx-rect)
      (loop for slot in '(x y w h)
            for value in (list ,x ,y ,w ,h)
            do (setf (cffi:foreign-slot-value ,var 'gfx-rect slot) value))
      ,@body)))

(defun blit-image (img x y &key (src-rect nil) (destination *vbuffer*))
  "function BLIT-IMAGE image x y

Blits the supplied image (with transparency if it's color-keyed) to
the destination (by default, the double buffer), at point (X, Y),
optionally clipping by src-rect."
  (when img
    ;; XXX this silly Y change is going to be removed.
    (decf y (if src-rect (fourth src-rect) (surface-h img)))
    (if src-rect
	(with-gfx-rect-from-list (rect src-rect)
	  (ll-gfx-blit-surface img rect destination x y))
	(ll-gfx-blit-surface img (cffi:null-pointer) destination x y))
    t))

;;; Y'know, in Double Draggin', I implemented Wu-Rokne-Wyvill line
;;; drawing, because I was sick of Bresenham all the time.  After that
;;; I realized it totally wasn't worth it.
(eval-when (:compile-toplevel)
  ;; Internal macro; used in DRAW-LINE and HELPER-DRAW-LINE.
  (defmacro inner-draw-line (swap-p &body plot-body)
    `(progn
       (setf d (the fixnum (ash (- ,@(if swap-p '(ax ay) '(ay ax))) -1)))
       (loop
	  ,@plot-body
	  (when (= ,@(if swap-p '(p-y q-y) '(p-x q-x))) (return))
	  (when (>= d 0)
	    (incf ,@(if swap-p '(p-x sign-x) '(p-y sign-y)))
	    (decf d ,(if swap-p 'ay 'ax)))
	  (incf ,@(if swap-p '(p-y sign-y) '(p-x sign-x)))
	  (incf d ,(if swap-p 'ax 'ay))))))

(defun draw-line (p-x p-y q-x q-y color &optional (surface *vbuffer*))
  "Draw a line between points (p-x, p-y) and (q-x, q-y), in color
COLOR, on SURFACE."
  (declare (optimize speed)
	   (type fixnum p-x p-y q-x q-y))
  (ll-gfx-lock-surface surface)
  (let* ((delta-x (- q-x p-x))
	 (delta-y (- q-y p-y))
	 (ax (the fixnum (* 2 (abs delta-x))))
	 (ay (the fixnum (* 2 (abs delta-y))))
	 (sign-x (if (> delta-x 0) 1 -1))
	 (sign-y (if (> delta-y 0) 1 -1))
	 (d 0))
    (declare (type fixnum d))
    (flet ((in-drawable-region-p ()
	     (and (<= 0 p-x (surface-w surface))
		  (<= 0 p-y (surface-h surface)))))
      (if (< ay ax)
	  (inner-draw-line nil
			   (when (in-drawable-region-p)
			     (ll-gfx-draw-pixel surface p-x p-y color)))
	  (inner-draw-line t
			   (when (in-drawable-region-p)
			     (ll-gfx-draw-pixel surface p-x p-y color))))))

  (ll-gfx-unlock-surface surface))

(defun draw-rectangle (x y w h color &optional (surface *vbuffer*))
  "Draws an unfilled rectangle with border color COLOR on SURFACE."
  (draw-line x y (+ x w) y color surface)
  (draw-line x y x (+ y h) color surface)
  (draw-line x (+ y h) (+ x w) (+ y h) color surface)
  (draw-line (+ x w) y (+ x w) (+ y h) color surface))

(defun draw-filled-rectangle (x y w h color &optional (surface *vbuffer*))
  "Draws a filled rectangle with solid color COLOR on SURFACE."
  (with-gfx-rect-boa (rect x y w h)
    (ll-gfx-fill-rect surface rect color)))

(defun draw-triangle (xs ys color &optional (surface *vbuffer*))
  "Draws an unfilled triangle where XS and YS are lists containing
three integers each; the X and Y coordinates for each of the three
points, respectively.  The triangle is drawn in color COLOR on
SURFACE."
  (mapcar (lambda (opa opb)
	    (draw-line (funcall opa xs) (funcall opa ys)
		       (funcall opb xs) (funcall opb ys)
		       color surface))
	  (list #'first #'second #'third)
	  (list #'second #'third #'first)))

(defun draw-filled-triangle (xs ys color &optional (surface *vbuffer*))
  "As DRAW-TRIANGLE, but fills the triangle with solid color COLOR."
  (let ((list (list)))
    (mapcar (lambda (opa opb)
	      (setf list
		    (helper-draw-line (funcall opa xs) (funcall opa ys)
				      (funcall opb xs) (funcall opb ys)
				      list (surface-h surface)))
	      nil)
	    (list #'first #'second #'third)
	    (list #'second #'third #'first))
    (dolist (raster list)
      (draw-filled-rectangle (second raster) (first raster)
			     (- (third raster) (second raster))
			     1 color surface))))

;; Internal function used in draw-filled-triangle.
(defun helper-draw-line (p-x p-y q-x q-y list y-max)
  "Determines what coordinates a line would cover if plotted from
 (P-X, P-Y) to (Q-X, Q-Y).  For each scanline the line would cover, an
entry is placed in LIST of the form (Y MIN MAX), being the scanline
coordinate, the minimum horizontal extent, and the maximum horizontal
extent, respectively.  If an entry for that scanline already exists,
the minimum or maximum values are adjusted if the line would exceed
them.  Scanlines with coordinates less than 0 or greater than Y-MAX
are ignored.  Returns the new value of LIST.  (May also destructively
modify LIST)"
  (declare (optimize speed)
	   (type fixnum p-x p-y q-x q-y)
	   (type list list))
  (let* ((delta-x (- q-x p-x))
	 (delta-y (- q-y p-y))
	 (ax (the fixnum (* 2 (abs delta-x))))
	 (ay (the fixnum (* 2 (abs delta-y))))
	 (sign-x (if (> delta-x 0) 1 -1))
	 (sign-y (if (> delta-y 0) 1 -1))
	 (d 0))
    (flet ((listplot (x y)
	     (aif (find y list :key #'first)
		  (if (< x (second it))
		      (setf (second it) x)
		      (when (> x (third it))
			(setf (third it) x)))
		  (push (list y x x) list))))
      (declare (type fixnum d))

      (cond ((< ay ax)
	     (inner-draw-line nil
	       (when (<= 0 p-y y-max) (listplot p-x p-y))))
	    (t
	     (inner-draw-line t
	       (when (<= 0 p-y y-max) (listplot p-x p-y)))))))
  list)

