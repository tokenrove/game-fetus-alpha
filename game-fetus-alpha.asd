;; -*- Lisp -*-

(defpackage #:game-fetus-alpha-system (:use #:cl #:asdf))
(in-package #:game-fetus-alpha-system)

(defsystem game-fetus-alpha
  :depends-on (:cffi :anaphora)
  :components
  ((:file "package")
   ;; low-level
   ;(:file "uffi" :depends-on ("package"))
   (:file "cffi" :depends-on ("package"))
   (:file "graphics" :depends-on ("package" "cffi"))
   (:file "event" :depends-on ("package" "cffi"))
   ;; middle-level
   (:file "font" :depends-on ("package" "graphics" "cffi"))
   (:file "sprite" :depends-on ("package" "graphics"))
   (:file "layer" :depends-on ("package" "graphics"))
   (:file "generic-editor" :depends-on ("package" "graphics" "event"))))
