;;;
;;; font.lisp -- Simple font routines.
;;;
;;; Uses SDL-TTF to do very primitive font things.  This is all pretty
;;; ugly, but who really cares about fonts?
;;;
;;; Things to do here:
;;; * add support for oldschool bitmap fonts.
;;; * add metric functions.
;;; * add support for font styles.
;;; * allow painting to surfaces other than *vbuffer*.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :game-fetus-alpha)

(defun font-init ()
  "This must be called before any other font routines are used."
  (ll-font-init))

#+nil(defun text-size (font string)
       (cl-sdl-ttf:size-text font string))

(defmacro with-temporary-surface ((variable surface) &body body)
  `(let ((,variable (maybe-null->nil ,surface)))
    (when ,variable
      (unwind-protect
	   (progn ,@body)
	(free-image ,variable)))))

(defun paint-string (font string x y r g b)
  "Paints STRING on *VBUFFER* using FONT, at position (X,Y)."
  (uffi:with-cstring (cstring string)
    (with-temporary-surface (face (ll-font-render-solid font cstring r g b))
      (blit-image face x (+ y (surface-h face))))))

(defun paint-blended-string (font string x y r g b)
  "Paints STRING on *VBUFFER* using FONT, at position (X,Y)."
  (uffi:with-cstring (cstring string)
    (with-temporary-surface (face
			     (ll-font-render-blended font cstring r g b))
      (blit-image sface x (+ y (surface-h sface))))))

(defun paint-shaded-string (font string x y r1 g1 b1 r2 g2 b2)
  "Paints STRING on *VBUFFER* using FONT, at position (X,Y)."
  (uffi:with-cstring (cstring string)
    (with-temporary-surface (face
			     (ll-font-render-shaded font cstring r1 g1 b1
						    r2 g2 b2))
      (blit-image sface x (+ y (surface-h sface))))))

(defun destroy-font (font)
  "Deallocates FONT appropriately."
  (unless (null font)
    (ll-font-close font)))

(defun load-font (file &optional (ptsize 12))
  "Loads font, with point size PTSIZE, from FILE, and returns it, or
NIL."
  (uffi:with-cstring (name file)
    (maybe-null->nil (ll-font-open name ptsize))))

(defmacro with-font ((font-var file &optional (ptsize 12)) &body body)
  `(let ((,font-var (load-font ,file ,ptsize)))
     (unwind-protect
	  (progn ,@body)
       (destroy-font ,font-var))))