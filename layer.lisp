;;;
;;; Simple 2D tilemap layer code.
;;;
;;; Basically just proof of concept stuff at the moment, to provide
;;; the interface.  I'll improve this later.
;;;
;;; Julian Squires <tek@wiw.org> / 2004

(in-package :game-fetus-alpha)

;; supply set of tiles and tilemap, get back layer handler.

;; (make-tileset tile-w tile-h)
;; (add-image-to-tileset tiles image)
;; (make-layer map tiles)