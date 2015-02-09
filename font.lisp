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

#+nil(defun text-size (font string)
       (cl-sdl-ttf:size-text font string))

(defmacro with-temporary-surface ((variable surface) &body body)
  `(let ((,variable ,surface))
    (when ,variable
      (unwind-protect
	   (progn ,@body)
	(free-image ,variable)))))

(defun map-color (r g b)
  (logior r (ash g 8) (ash b 16)))


(defun paint-string (font string x y r g b)
  "Paints STRING on *VBUFFER* using FONT, at position (X,Y)."
  (let ((c (map-color r g b)))
    (with-temporary-surface (face (sdl-ttf:render-text-solid font string c))
      (blit-image face x (+ y (surface-h face))))))

(defun paint-blended-string (font string x y r g b)
  "Paints STRING on *VBUFFER* using FONT, at position (X,Y)."
  (let ((c (map-color r g b)))
    (with-temporary-surface (face
                             (sdl-ttf:render-text-blended font string c))
      (blit-image face x (+ y (surface-h face))))))

(defun paint-shaded-string (font string x y r1 g1 b1 r2 g2 b2)
  "Paints STRING on *VBUFFER* using FONT, at position (X,Y)."
  (let ((fg (map-color r1 g1 b1))
        (bg (map-color r2 g2 b2)))
    (with-temporary-surface (face
                             (sdl-ttf:render-text-shaded font string fg bg))
        (blit-image face x (+ y (surface-h face))))))

(defmacro with-font ((var file &optional (ptsize 12)) &body body)
  `(sdl-ttf:with-font (,var ,file ,ptsize)
     ,@body))

(fetus/test:define-screencap-comparison-test
    (font-rendering-produces-visible-output)
  (with-font (font "./t/f500.ttf" 24)
    (paint-string font "this is a test" 10 10 #xff #xff #xff)))
