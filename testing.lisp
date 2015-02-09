;;;; Test scaffolding

(in-package :game-fetus-alpha/test)

(defmacro with-dummy-sdl (&body body)
  `(fetus/os:with-environment-variable ("SDL_VIDEODRIVER" "dummy")
     ,@body))

(cffi:defctype size-t :long)
(cffi:defcfun ("memcmp" memcmp) :int (s1 :pointer) (s2 :pointer) (n size-t))

(defun surfaces-equal (a b)
  "Compare two SDL:SURFACEs by pixels, bytewise."
  (and (= (sdl:pitch-of a) (sdl:pitch-of b))
       (= (sdl:height-of a) (sdl:height-of b))
       (= 0 (memcmp (sdl:pixels-of a) (sdl:pixels-of b) (* (sdl:pitch-of a) (sdl:height-of a))))))

(defun display-same-as-image-p (expected &optional failure-path)
  "Compare the current display contents, pixel-by-pixel, with the surface EXPECTED.

If FAILURE-PATH is supplied, save the current display contents there
as a PNG if the comparison is false or the comparison image failed to
load."
  (let ((current-display (fetus:contents-of-current-display)))
    (or (and expected (surfaces-equal expected current-display))
        (when failure-path
          (sdl-image:save-png current-display failure-path)
          nil))))

(defun display-same-as-expected-image-p (test-symbol)
  "Compare the current display contents with a PNG loaded from a
location based on TEST-SYMBOL.  Do not expose this to arbitrary input,
as TEST-SYMBOL is not sanity checked."
  (let ((name (symbol-name test-symbol)))
    (display-same-as-image-p
     (sdl-image:load (concatenate 'string "./t/expected-result-" name ".png"))
           (concatenate 'string "./t/failure-result-" name ".png"))))
