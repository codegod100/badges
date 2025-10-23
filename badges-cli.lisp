#!/usr/bin/env -S sbcl --script
;;; badges-cli.lisp - Simple command-line tool for signing and verifying PNG images

;; Load Quicklisp if available so dependencies resolve cleanly
(let* ((home (user-homedir-pathname))
       (ql-setup (merge-pathnames #p"quicklisp/setup.lisp" home)))
  (when (probe-file ql-setup)
    (load ql-setup)))

(require 'asdf)

;; Ensure ASDF can locate the current project
(let* ((script-path (or *load-truename* *compile-file-pathname*))
  (script-dir (and script-path (uiop:pathname-directory-pathname script-path))))
  (when script-dir
    (pushnew script-dir asdf:*central-registry* :test #'equal)))

(asdf:load-system :badges)

(defun print-usage ()
  (format t "Usage:~%" )
  (format t "  ./badges-cli.lisp sign <input.png> <output.png> <private.key>~%")
  (format t "  ./badges-cli.lisp verify <input.png> <public.key>~%")
  (format t "  ./badges-cli.lisp keygen <private.key> <public.key>~%")
  (format t "~%Options:~%  -h, --help   Show this message~%"))

(defun handle-sign (args)
  (if (/= (length args) 3)
      (progn
        (format *error-output* "sign expects <input.png> <output.png> <private.key>~%")
        1)
      (destructuring-bind (input output private-key) args
        (badges:sign-image input output private-key)
        0)))

(defun handle-verify (args)
  (if (/= (length args) 2)
      (progn
        (format *error-output* "verify expects <input.png> <public.key>~%")
        1)
      (destructuring-bind (input public-key) args
        (if (badges:verify-image input public-key)
            0
            3))))

(defun handle-keygen (args)
  (if (/= (length args) 2)
      (progn
        (format *error-output* "keygen expects <private.key> <public.key>~%")
        1)
      (destructuring-bind (private-key public-key) args
        (badges:keygen private-key public-key)
        0)))

(defun main ()
  (let ((args (uiop:command-line-arguments)))
    (cond
      ((or (null args)
           (member (string-downcase (first args)) '("-h" "--help") :test #'string=))
       (print-usage)
       (if (null args) 1 0))
      (t
       (let* ((command (string-downcase (first args)))
              (rest-args (rest args)))
         (cond
           ((string= command "sign") (handle-sign rest-args))
           ((string= command "verify") (handle-verify rest-args))
           ((string= command "keygen") (handle-keygen rest-args))
           (t
            (format *error-output* "Unknown command: ~A~%" command)
            (print-usage)
            1)))))))

(handler-case
    (uiop:quit (main))
  (error (e)
    (format *error-output* "Error: ~A~%" e)
    (uiop:quit 2)))
