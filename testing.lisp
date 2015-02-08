;;;; Test scaffolding

(in-package :game-fetus-alpha/test)

(def-suite unit :description "Fast tests")
(def-suite offscreen :description "Unit tests using dummy SDL video" :in unit)
(def-suite integration :description "Non-interactive, potentially slow, tests")
(def-suite acceptance :description "Interactive or high-level tests")

(in-suite unit)

(defmacro with-dummy-sdl (&body body)
  `(fetus/os:with-environment-variable ("SDL_VIDEODRIVER" "dummy")
     ,@body))

(cffi:defctype size-t :long)
(cffi:defcfun ("memcmp" memcmp) :int (s1 :pointer) (s2 :pointer) (n size-t))

(defun surfaces-equal (a b)
  "Compare two SDL:SURFACEs by pixels, bytewise."
  (and (= (sdl:pitch-of a) (sdl:pitch-of b))
       (= (sdl:height-of a) (sdl:height-of b))
       (memcmp (sdl:pixels-of a) (sdl:pixels-of b) (* (sdl:pitch-of a) (sdl:height-of a)))))

(defun display-same-as-image? (expected-path &optional failure-path)
  "Compare the current display contents, pixel-by-pixel, with the PNG
loaded from EXPECTED-PATH.

If FAILURE-PATH is supplied, save the current display contents there
as a PNG if the comparison is false or the comparison image failed to
load."
  (let ((image (sdl-image:load expected-path))
        (current-display (fetus:contents-of-current-display)))
    (or (and image (surfaces-equal image current-display))
        (when failure-path
          (sdl-image:save-png current-display failure-path)
          nil))))
