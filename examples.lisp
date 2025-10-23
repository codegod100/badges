;;;; examples.lisp - Usage examples

(in-package #:badges)

;;; Example 1: Complete workflow
(defun example-workflow ()
  "Demonstrate the complete signing and verification workflow"
  
  ;; Step 1: Generate keypair
  (format t "~%=== Step 1: Generate Keypair ===~%")
  (multiple-value-bind (priv pub)
      (generate-keypair)
    
    ;; Save keys
    (save-key priv "example-private.key")
    (save-key pub "example-public.key")
    (format t "Keys saved!~%")
    
    ;; Step 2: Sign an image (assuming test.png exists)
    (format t "~%=== Step 2: Sign Image ===~%")
    (when (probe-file "test.png")
      (sign-image "test.png" "test-signed.png" "example-private.key")
      
      ;; Step 3: Verify the signed image
      (format t "~%=== Step 3: Verify Image ===~%")
      (verify-image "test-signed.png" "example-public.key")
      
      ;; Step 4: Try to verify with wrong key (should fail)
      (format t "~%=== Step 4: Verify with Different Key (should fail) ===~%")
      (multiple-value-bind (priv2 pub2)
          (generate-keypair)
        (save-key pub2 "wrong-public.key")
        (verify-image "test-signed.png" "wrong-public.key")))))

;;; Example 2: Check if image is signed
(defun image-signed-p (filepath)
  "Check if an image has a signature"
  (let* ((chunks (read-png-chunks filepath))
         (sig (extract-signature chunks)))
    (if sig
        (format t "Image ~A is signed.~%" filepath)
        (format t "Image ~A is NOT signed.~%" filepath))
    (not (null sig))))

;;; Example 3: Batch verification
(defun verify-images-in-directory (directory public-key-file)
  "Verify all PNG images in a directory"
  (let ((png-files (directory (merge-pathnames "*.png" directory))))
    (format t "Verifying ~A PNG files in ~A~%" (length png-files) directory)
    (dolist (file png-files)
      (format t "~%Checking: ~A~%" (file-namestring file))
      (handler-case
          (verify-image (namestring file) public-key-file)
        (error (e)
          (format t "Error: ~A~%" e))))))

;;; Example 4: Sign with custom wrapper
(defun sign-with-metadata (input-png output-png private-key-file author)
  "Sign image and add author metadata"
  (let* ((chunks (read-png-chunks input-png))
         (private-key (load-private-key private-key-file))
         (image-data (get-image-data chunks))
         (signature (sign-data image-data private-key))
         
         ;; Add author chunk
         (author-chunk (make-text-chunk "Author" author))
         (sig-chunk (make-text-chunk "Signature" signature)))
    
    ;; Insert both chunks before IEND
    (let ((result '()))
      (dolist (chunk chunks)
        (unless (string= (png-chunk-type chunk) "IEND")
          (push chunk result)))
      (push author-chunk result)
      (push sig-chunk result)
      (dolist (chunk chunks)
        (when (string= (png-chunk-type chunk) "IEND")
          (push chunk result)
          (return)))
      
      (write-png-chunks output-png (nreverse result))
      (format t "Signed and added author: ~A~%" author))))
