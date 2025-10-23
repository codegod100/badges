;;;; main.lisp - Command-line interface

(in-package #:badges)

(defun print-usage ()
  (format t "~
Usage:
  Generate keypair:
    sbcl --load main.lisp --eval '(badges:keygen \"private.key\" \"public.key\")' --quit

  Sign an image:
    sbcl --load main.lisp --eval '(badges:sign-image \"input.png\" \"output.png\" \"private.key\")' --quit

  Verify an image:
    sbcl --load main.lisp --eval '(badges:verify-image \"signed.png\" \"public.key\")' --quit
~%"))

(defun keygen (private-key-file public-key-file)
  "Generate a new keypair and save to files"
  (format t "Generating Ed25519 keypair...~%")
  (multiple-value-bind (priv pub)
      (generate-keypair)
    (format t "Saving private key to: ~A~%" private-key-file)
    (save-key priv private-key-file)
    (format t "Saving public key to: ~A~%" public-key-file)
    (save-key pub public-key-file)
    (format t "✓ Keypair generated successfully!~%")
    (format t "~%WARNING: Keep your private key secret!~%")
    t))

;; Export the keygen function
(export 'keygen)
