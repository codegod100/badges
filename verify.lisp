;;;; verify.lisp - Image verification functionality

(in-package #:badges)

(defun verify-image (input-png public-key-file)
  "Verify the signature embedded in a PNG image"
  (format t "Reading PNG: ~A~%" input-png)
  (let ((chunks (read-png-chunks input-png)))
    (multiple-value-bind (signature metadata raw)
        (extract-signature chunks)
      (declare (ignore raw))
      (unless signature
        (format t "✗ No signature found in image!~%")
        (return-from verify-image nil))
      
      (let ((preview (subseq signature 0 (min 32 (length signature)))))
        (format t "Found signature: ~A...~%" preview))
      (%print-attestation metadata)
      
      ;; Extract image data
      (let* ((image-data (get-image-data chunks))
             (public-key (load-public-key public-key-file))
             (image-hash (hash-data image-data))
             (metadata-hash (or (cdr (assoc "image_hash" metadata :test #'string=))
                                (cdr (assoc "image-hash" metadata :test #'string=)))))
        
        (format t "Image data size: ~A bytes~%" (length image-data))
        (format t "Image hash: ~A~%" (subseq image-hash 0 16))
        (when metadata-hash
          (if (string= (string-upcase metadata-hash) image-hash)
              (format t "Metadata hash matches image contents.~%")
              (format t "⚠ Metadata hash (~A) does not match image!~%"
                      metadata-hash)))
        
        ;; Verify signature
        (format t "Verifying signature...~%")
        (if (verify-signature image-data signature public-key)
            (progn
              (format t "✓ Signature is VALID!~%")
              t)
            (progn
              (format t "✗ Signature is INVALID!~%")
              nil))))))
