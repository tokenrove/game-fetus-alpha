
(defpackage :game-fetus-alpha
  (:nicknames :fetus)
  (:use :cl :anaphora)
  (:export #:with-display
           #:new-image-buffer
           #:clear-display
           #:present-display
	   #:surface-w
	   #:surface-h
	   #:display-width
	   #:display-height
	   #:fill-background
	   #:load-image
	   #:free-image
	   #:use-image-palette
	   #:blit-image
	   #:draw-line
	   #:draw-rectangle
	   #:draw-filled-rectangle
	   #:draw-triangle
           #:draw-filled-triangle)
  ;; events
  (:export #:+ev-quit+
           #:+ev-up+
           #:+ev-down+
           #:+ev-left+
           #:+ev-right+
           #:+ev-button-a+
           #:+ev-button-b+
           #:event-init
           #:event-shutdown
           #:wipe-events
           #:event-update
           #:event-pressedp
           #:get-key-event)
  ;; timer
  (:export #:timer-get-ticks
           #:timer-start-frame
           #:timer-end-frame)
  ;; font
  (:export #:font-init
           #:paint-string
           #:paint-blended-string
           #:paint-shaded-string
           #:destroy-font
           #:load-font
           #:with-font)
  ;; sprite
  (:export #:sprite-x
           #:sprite-y
           #:sprite-priority
           #:sprite-blit-offset
           #:new-sprite-from-alist
           #:draw-sprite
           #:set-sprite-animation
           #:create-sprite-manager
           #:destroy-sprite-manager
           #:add-sprite-to-manager
           #:remove-sprite-from-manager
           #:update-all-sprites
           #:with-sprite-manager)
  ;; layer
  ;; generic editor
  (:export #:prompt-for-yes-or-no
           #:prompt-for-string
           #:prompt-for-integer
           #:draw-status-message))
