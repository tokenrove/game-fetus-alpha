;;; FFI for a simple game toolkit.
;;;
;;; Julian Squires <tek@wiw.org> / 2004

(in-package :game-fetus-alpha)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; XXX eventually needs to figure out how to find the data.
  (let ((location "./low-level.so"))
    (assert
      (uffi:load-foreign-library location
                                 :module "low-level"
                                 :supporting-libraries '("c")
                                 :force-load t)
      (location) "Couldn't find or load ~A." location)))

(defun maybe-null->nil (ptr)
  "Convenience function -- if the supplied pointer is NULL, returns
NIL.  Returns the pointer itself otherwise."
  (unless (uffi:null-pointer-p ptr) ptr))

(defun bool->int (pred) (if pred 1 0))


;; XXX: This is kind of evil.
(defmacro with-foreign-objects (bindings &rest body)
  #+(or cmu scl sbcl)
  (let ((bindings (mapcar
		   (lambda (x)
		     (list (car x)
			   (uffi::convert-from-uffi-type (cadr x)
							 :allocation)))
		   bindings)))
    #+(or cmu scl)
    `(alien:with-alien ,bindings
       ,@body)
    #+sbcl
    `(sb-alien:with-alien ,bindings
       ,@body))
  #-(or cmu scl sbcl)
  `(uffi:with-foreign-objects ,bindings ,@body))

(defmacro pointer-to-object (object)
  #+(or cmu scl) `(alien:alien-sap ,object)
  #+sbcl `(sb-alien:alien-sap ,object)
  #-(or cmu scl sbcl) object)


;;;; GRAPHICS

;; Careful!  This should correspond with the C SDL_Rect type.
(uffi:def-struct gfx-rect (x :short) (y :short)
		 (w :unsigned-short) (h :unsigned-short))

(declaim (inline ll-gfx-init))
(uffi:def-function "ll_gfx_init" ((fullscreen-p :int)
			     (width :int)
			     (height :int)
			     (bpp :int))
  :returning :pointer-void :module "low-level")
(declaim (inline ll-gfx-shutdown))
(uffi:def-function "ll_gfx_shutdown" ((vbuffer :pointer-void))
  :returning :void :module "low-level")

(declaim (inline ll-gfx-new-image-buffer))
(uffi:def-function "ll_gfx_new_image_buffer" ((width :int)
					 (height :int))
  :returning :pointer-void :module "low-level")
(declaim (inline ll-gfx-load-image))
(uffi:def-function "ll_gfx_load_image" ((filename :cstring)
				   (colorkey :int))
  :returning :pointer-void :module "low-level")
(declaim (inline ll-gfx-free-surface))
(uffi:def-function "ll_gfx_free_surface" ((sface :pointer-void))
  :returning :void :module "low-level")

(declaim (inline ll-gfx-use-image-palette))
(uffi:def-function "ll_gfx_use_image_palette" ((image :pointer-void)
					  (vbuffer :pointer-void))
  :returning :void :module "low-level")

(declaim (inline ll-gfx-refresh-display))
(uffi:def-function "ll_gfx_refresh_display" ((vbuffer :pointer-void))
  :returning :void :module "low-level")

(declaim (inline ll-gfx-surface-w))
(uffi:def-function "ll_gfx_surface_w" ((sface :pointer-void))
  :returning :int :module "low-level")
(declaim (inline ll-gfx-surface-w))
(uffi:def-function "ll_gfx_surface_h" ((sface :pointer-void))
  :returning :int :module "low-level")

(declaim (inline ll-gfx-lock-surface))
(uffi:def-function "ll_gfx_lock_surface" ((sface :pointer-void))
  :returning :void :module "low-level")
(declaim (inline ll-gfx-unlock-surface))
(uffi:def-function "ll_gfx_unlock_surface" ((sface :pointer-void))
  :returning :void :module "low-level")

(declaim (inline ll-gfx-blit-surface))
(uffi:def-function "ll_gfx_blit_surface" ((src :pointer-void)
				     (srect (* gfx-rect))
				     (dst :pointer-void)
				     (drect (* gfx-rect)))
  :returning :void :module "low-level")
(declaim (inline ll-gfx-blit-surface-stub))
(uffi:def-function "ll_gfx_blit_surface_stub" ((src :pointer-void)
                                          (x :int)
                                          (y :int)
                                          (w :int)
                                          (h :int)
                                          (dst :pointer-void)
                                          (x2 :int)
                                          (y2 :int))
  :returning :void :module "low-level")


(declaim (inline ll-gfx-draw-pixel))
(uffi:def-function "ll_gfx_draw_pixel" ((sface :pointer-void)
				   (x :int)
				   (y :int)
				   (color :int))
  :returning :void :module "low-level")
(declaim (inline ll-gfx-fill-rect))
(uffi:def-function "ll_gfx_fill_rect" ((sface :pointer-void)
				  (rect (* gfx-rect))
				  (color :int))
  :returning :void :module "low-level")
(declaim (inline ll-gfx-fill-rect-stub))
(uffi:def-function "ll_gfx_fill_rect_stub" ((sface :pointer-void)
                                       (x :int)
                                       (y :int)
                                       (w :int)
                                       (h :int)
                                       (color :int))
  :returning :void :module "low-level")

;;;; EVENT

(uffi:def-struct ll-event
    (type :int)
  (value :int)
  (axis :int))

;; XXX should grovel or something.  This will do for now.
(defconstant +ll-event-key-down+ 2)
(defconstant +ll-event-key-up+ 3)
(defconstant +ll-event-joy-move+ 4)
(defconstant +ll-event-joy-button-down+ 5)
(defconstant +ll-event-joy-button-up+ 6)

(declaim (inline ll-event-init))
(uffi:def-function "ll_event_init" () :returning :void :module "low-level")
(declaim (inline ll-event-shutdown))
(uffi:def-function "ll_event_shutdown" () :returning :void :module "low-level")

(declaim (inline ll-poll-event))
(uffi:def-function "ll_poll_event" ((event (* ll-event)))
  :returning :int :module "low-level")
(declaim (inline ll-wait-event))
(uffi:def-function "ll_wait_event" ((event (* ll-event)))
  :returning :int :module "low-level")

;;;; TIMER

(declaim (inline timer-get-ticks timer-start-frame timer-end-frame))
(uffi:def-function "timer_get_ticks" () :module "low-level"
	      :returning :unsigned-long)
(uffi:def-function "timer_start_frame" ((frame-length :int))
  :module "low-level" :returning :void)
(uffi:def-function "timer_end_frame" () :module "low-level" :returning :int)

;;;; FONT

(declaim (inline ll-font-init))
(uffi:def-function "ll_font_init" ()
  :returning :void :module "low-level")

(declaim (inline ll-font-open))
(uffi:def-function "ll_font_open" ((filename :cstring)
			      (ptsize :int))
  :returning :pointer-void :module "low-level")
(declaim (inline ll-font-close))
(uffi:def-function "ll_font_close" ((font :pointer-void))
  :returning :void :module "low-level")

(declaim (inline ll-font-render-solid))
(uffi:def-function "ll_font_render_solid" ((font :pointer-void)
				       (string :cstring)
				       (r :int)
				       (g :int)
				       (b :int))
  :returning :pointer-void :module "low-level")
(declaim (inline ll-font-render-blended))
(uffi:def-function "ll_font_render_blended" ((font :pointer-void)
					(string :cstring)
					(r :int)
					(g :int)
					(b :int))
  :returning :pointer-void :module "low-level")
(declaim (inline ll-font-render-shaded))
(uffi:def-function "ll_font_render_shaded" ((font :pointer-void)
				       (string :cstring)
				       (r1 :int)
				       (g1 :int)
				       (b1 :int)
				       (r2 :int)
				       (g2 :int)
				       (b2 :int))
  :returning :pointer-void :module "low-level")
