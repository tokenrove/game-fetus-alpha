;; -*- Lisp -*-

(in-package :asdf-user)

(defsystem game-fetus-alpha
  :depends-on (:anaphora :cl-sdl-cipht)
  :components
  ((:file "package")
   ;; low-level
   (:file "graphics" :depends-on ("package"))
   (:file "event" :depends-on ("package"))
   (:file "timer" :depends-on ("package"))
   ;; middle-level
   (:file "font" :depends-on ("package" "graphics"))
   (:file "sprite" :depends-on ("package" "graphics"))
   (:file "layer" :depends-on ("package" "graphics"))
   (:file "generic-editor" :depends-on ("package" "graphics" "event"))))
