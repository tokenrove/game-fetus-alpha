;;;; Light abstractions over OS/Lisp implementation facilities.
;;;;
;;;; This is intended to be for a few small things that were too
;;;; trivial to bother bringing in something like osicat.  I can
;;;; imagine it might grow to support finding the OS-appropriate place
;;;; to put configuration files or cached data.

(in-package :game-fetus-alpha/os)

#+sbcl (import '(sb-posix:getcwd sb-posix:chdir))
#+clozure
(defun getcwd () (ccl:current-directory))
#+clozure
(defun chdir (path) (setf (ccl:current-directory) path))

#+sbcl (import '(sb-posix:setenv sb-posix:getenv sb-posix:unsetenv))
#+clozure (import '(ccl:setenv ccl:getenv ccl:unsetenv))


(defmacro with-current-directory ((path) &body body)
  "Evaluate BODY with PATH as the current working directory.  This
sets both the OS current working directory and
*DEFAULT-PATHNAME-DEFAULTS*."
  (let ((previous-dir (gensym "PREVIOUS-DIR")))
    (once-only (path)
     `(let ((,previous-dir (getcwd)))
        (unwind-protect
             (let ((*default-pathname-defaults* (pathname ,path)))
               (chdir ,path)
               ,@body)
          (chdir ,previous-dir))))))

(defmacro with-directory-of-system ((system) &body body)
  "Evaluate BODY with the current working directory set to the
directory where the ASDF system SYSTEM is found."
  `(with-current-directory ((asdf/system:system-relative-pathname ,system "." :type :directory))
     ,@body))

#+5am
(5am:test (nested-with-directory-of-system :suite fetus:unit)
  (with-directory-of-system (:game-fetus-alpha)
    (let ((a (getcwd)) b)
      (with-directory-of-system (:fiveam)
        (setf b (getcwd)))
      (5am:is (equal a (getcwd)))
      (5am:is (not (equal a b))))))

(defmacro with-environment-variable ((env-var value) &body body)
  (let ((old-value (gensym "OLD-VALUE")))
    (once-only (env-var value)
      `(let ((,old-value (getenv ,env-var)))
         (setenv ,env-var ,value 1)
         (unwind-protect (progn ,@body)
           (if ,old-value
               (setenv ,env-var ,old-value 1)
               (unsetenv ,env-var)))))))

#+5am
(5am:test with-environment-variable
  (let ((a (getenv "HOME")))
    (with-environment-variable ("HOME" "/tmp/")
      (5am:is (equal (getenv "HOME") "/tmp/")))
    (5am:is (equal (getenv "HOME") a))))
