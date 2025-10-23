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
  (format t "  ./badges-cli.lisp sign [options] <input.png> <output.png> <private.key>~%")
  (format t "  ./badges-cli.lisp verify <input.png> <public.key>~%")
  (format t "  ./badges-cli.lisp keygen <private.key> <public.key>~%")
  (format t "~%Sign options:~%")
  (format t "  --subject <name>      Beneficiary or subject being attested~%")
  (format t "  --issuer <name>       Entity issuing the attestation~%")
  (format t "  --purpose <text>      Purpose or reason for the signature~%")
  (format t "  --note <text>         Additional free-form note~%")
  (format t "  --issued-at <iso8601> Override the generated timestamp~%")
  (format t "  --meta key=value      Add custom metadata (repeatable)~%")
  (format t "~%General options:~%  -h, --help   Show this message~%"))

(defun parse-sign-arguments (args)
  "Split ARGS into positional parameters and a metadata plist."
  (let ((metadata '())
        (positionals '())
        (error-message nil))
    (labels ((consume-value (option)
               (if args
                   (pop args)
                   (progn
                     (setf error-message (format nil "~A expects a value" option))
                     nil)))
             (set-meta (key value)
               (when value
                 (setf (getf metadata key) value))))
  (loop while args while (null error-message)
            for arg = (pop args)
            do (cond
                 ((string= arg "--subject")
                  (set-meta :subject (consume-value arg)))
                 ((string= arg "--issuer")
                  (set-meta :issuer (consume-value arg)))
                 ((string= arg "--purpose")
                  (set-meta :purpose (consume-value arg)))
                 ((string= arg "--note")
                  (set-meta :note (consume-value arg)))
                 ((string= arg "--issued-at")
                  (set-meta :issued-at (consume-value arg)))
                 ((string= arg "--meta")
                  (let ((kv (consume-value arg)))
                    (when kv
                      (let ((sep (position #\= kv)))
                        (if sep
                            (let* ((raw-key (subseq kv 0 sep))
                                   (raw-val (subseq kv (1+ sep)))
                                   (keyword (intern (string-upcase raw-key) :keyword)))
                              (set-meta keyword raw-val))
                            (setf error-message "--meta expects KEY=VALUE"))))))
                 ((and (> (length arg) 2)
                       (string= (subseq arg 0 2) "--"))
                  (setf error-message (format nil "Unknown option: ~A" arg)))
                 (t
                  (push arg positionals))))
      (values (nreverse positionals) metadata error-message))))

(defun handle-sign (args)
  (multiple-value-bind (positionals metadata error-message)
      (parse-sign-arguments args)
    (cond
      (error-message
       (format *error-output* "~A~%" error-message)
       1)
      ((/= (length positionals) 3)
       (format *error-output* "sign expects <input.png> <output.png> <private.key>~%")
       1)
      (t
       (destructuring-bind (input output private-key) positionals
         (badges:sign-image input output private-key :metadata metadata)
         0)))))

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
