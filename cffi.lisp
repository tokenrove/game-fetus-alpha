
(in-package :game-fetus-alpha)


(defun maybe-null->nil (ptr) (if (cffi:null-pointer-p ptr) nil ptr))

(cffi:define-foreign-library low-level
  (:unix "low-level.so"))

(push "/home/julian/projects/game-fetus-alpha/" cffi:*foreign-library-directories*)

(cffi:use-foreign-library low-level)

(cffi:defcstruct gfx-rect
  (x :short) (y :short)
  (w :unsigned-short) (h :unsigned-short))

(declaim (inline ll-gfx-init ll-gfx-shutdown))
(cffi:defcfun "ll_gfx_init" :pointer
  (fullscreen-p :boolean)
  (scale :int)
  (width :int)
  (height :int)
  (bpp :int))
(cffi:defcfun "ll_gfx_shutdown" :void (vbuffer :pointer))

(declaim (inline ll-gfx-new-image-buffer ll-gfx-load-image ll-gfx-free-surface))
(cffi:defcfun "ll_gfx_new_image_buffer" :pointer
  (width :int)
  (height :int))
(cffi:defcfun "ll_gfx_load_image" :pointer
  (filename :string)
  (colorkey :boolean))
(cffi:defcfun "ll_gfx_free_surface" :void (sface :pointer))

(declaim (inline ll-gfx-use-image-palette))
(cffi:defcfun "ll_gfx_use_image_palette" :void
  (image :pointer)
  (vbuffer :pointer))

(declaim (inline ll-gfx-refresh-display))
(cffi:defcfun "ll_gfx_refresh_display" :void (vbuffer :pointer))

(declaim (inline ll-gfx-surface-w ll-gfx-surface-h))
(cffi:defcfun "ll_gfx_surface_w" :int (sface :pointer))
(cffi:defcfun "ll_gfx_surface_h" :int (sface :pointer))

(declaim (inline ll-gfx-lock-surface ll-gfx-unlock-surface))
(cffi:defcfun "ll_gfx_lock_surface" :void (sface :pointer))
(cffi:defcfun "ll_gfx_unlock_surface" :void (sface :pointer))

(declaim (inline ll-gfx-blit-surface))
(cffi:defcfun "ll_gfx_blit_surface" :void
  (src :pointer)
  (srect :pointer)
  (dst :pointer)
  (x :int)
  (y :int))

(declaim (inline ll-gfx-draw-pixel ll-gfx-fill-rect))
(cffi:defcfun "ll_gfx_draw_pixel" :void
  (sface :pointer)
  (x :int)
  (y :int)
  (color :int))

(cffi:defcfun "ll_gfx_fill_rect" :void
  (sface :pointer)
  (rect :pointer)
  (color :int))

;;;; EVENT

;; XXX should grovel or something.  This will do for now.
(cffi:defcenum event-type
  (:key-down 2)
  :key-up
  :joy-move
  :joy-button-down
  :joy-button-up)

(cffi:defcstruct ll-event
  (type event-type)
  (value :int)
  (axis :int))

(declaim (inline ll-event-init ll-event-shutdown ll-poll-event ll-wait-event))
(cffi:defcfun "ll_event_init" :void)
(cffi:defcfun "ll_event_shutdown" :void)

(cffi:defcfun "ll_poll_event" :int (event :pointer))
(cffi:defcfun "ll_wait_event" :int (event :pointer))

;;;; TIMER

(declaim (inline timer-get-ticks timer-start-frame timer-end-frame))
(cffi:defcfun "timer_get_ticks" :unsigned-long)
(cffi:defcfun "timer_start_frame" :void (frame-length :int))
(cffi:defcfun "timer_end_frame" :int)

;;;; FONT

(cffi:defcstruct sdl-color
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (unused :uint8))

(declaim (inline ll-font-init ll-font-open ll-font-close))
(cffi:defcfun "ll_font_init" :void)

(cffi:defcfun "ll_font_open" :pointer
  (filename :string)
  (ptsize :int))

(cffi:defcfun "ll_font_close" :void (font :pointer))

(declaim (inline ll-font-render-solid ll-font-render-blended ll-font-render-shaded))
(cffi:defcfun "ll_font_render_solid" :pointer
  (font :pointer)
  (string :string)
  (fg sdl-color))

(cffi:defcfun "ll_font_render_blended" :pointer
  (font :pointer)
  (string :string)
  (fg sdl-color))

(cffi:defcfun "ll_font_render_shaded" :pointer
  (font :pointer)
  (string :string)
  (fg sdl-color)
  (bg sdl-color))
