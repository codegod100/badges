;;;; verify.lisp - Image verification functionality

(in-package #:badges)

(defun verify-image (input-png public-key-file)
  "Verify the signature embedded in a PNG image"
  (format t "Reading PNG: ~A~%" input-png)
  (let* ((chunks (read-png-chunks input-png))
         (signature (extract-signature chunks)))
    
    (unless signature
      (format t "✗ No signature found in image!~%")
      (return-from verify-image nil))
    
    (let ((preview (subseq signature 0 (min 32 (length signature)))))
      (format t "Found signature: ~A...~%" preview))
    
    ;; Extract image data
    (let* ((image-data (get-image-data chunks))
           (public-key (load-public-key public-key-file)))
      
      (format t "Image data size: ~A bytes~%" (length image-data))
      (format t "Image hash: ~A~%" (subseq (hash-data image-data) 0 16))
      
      ;; Verify signature
      (format t "Verifying signature...~%")
      (if (verify-signature image-data signature public-key)
          (progn
            (format t "✓ Signature is VALID!~%")
            t)
          (progn
            (format t "✗ Signature is INVALID!~%")
            nil)))))
