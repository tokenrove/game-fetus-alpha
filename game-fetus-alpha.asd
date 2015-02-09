;; -*- Lisp -*-

(in-package :asdf-user)

(defsystem game-fetus-alpha
  :description "A crude library of game abstractions supporting Demon of the Fall"
  :author "Julian Squires <julian@cipht.net>"
  :license "GPL-3"
  :depends-on (:anaphora :alexandria :cl-sdl-cipht :fiveam)
  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :unit :game-fetus-alpha)))
  :serial t
  :components
  ((:file "package")
   (:file "test-suites")
   ;; low-level
   (:file "testing")
   (:file "os")
   (:file "graphics")
   (:file "event")
   (:file "timer")
   ;; middle-level
   (:file "font")
   (:file "sprite")
   (:file "layer")
   (:file "generic-editor")))
