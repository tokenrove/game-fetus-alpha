(in-package :game-fetus-alpha)

(5am:def-suite unit
  :description "Fast-running tests")
(5am:def-suite integration
  :description "Broader non-interactive tests")
(5am:def-suite acceptance
  :description "Interactive tests")

(5am:def-suite offscreen
  :description "Unit tests using dummy SDL video"
  :in unit)

(5am:in-suite unit)
