
(defpackage :game-fetus-alpha
  (:nicknames :fetus)
  (:use :cl :anaphora)
  ;; graphics
  (:export #:with-display
           #:new-image-buffer
           #:clear-display
           #:present-display
           #:contents-of-current-display
           #:get-local-color
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
  (:export #:event-init
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
  (:export #:sprite
           #:sprite-x
           #:sprite-y
           #:sprite-priority
           #:sprite-blit-offset
           #:new-sprite-from-alist
           #:draw-sprite
           #:set-sprite-animation)
  ;; layer
  ;; generic editor
  (:export #:prompt-for-yes-or-no
           #:prompt-for-string
           #:prompt-for-integer
           #:draw-status-message)
  ;; Test suites
  (:export #:unit #:integration #:acceptance))

(defpackage :game-fetus-alpha/os
  (:nicknames :fetus/os)
  (:use :cl :alexandria)
  (:export #:with-current-directory #:with-directory-of-system
           #:with-environment-variable))

(defpackage :game-fetus-alpha/test
  (:nicknames :fetus/test)
  (:use :cl :game-fetus-alpha :fiveam)
  (:export #:with-dummy-sdl
           #:display-same-as-expected-image-p
           #:define-screencap-comparison-test))
