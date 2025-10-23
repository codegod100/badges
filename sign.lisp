;;;; sign.lisp - Image signing functionality

(in-package #:badges)

(defun sign-image (input-png output-png private-key-file)
  "Sign an image and embed the signature in a tEXt chunk"
  (format t "Reading PNG: ~A~%" input-png)
  (let* ((chunks (read-png-chunks input-png))
         (image-data (get-image-data chunks))
         (private-key (load-private-key private-key-file)))
    
    (format t "Image data size: ~A bytes~%" (length image-data))
    (format t "Image hash: ~A~%" (subseq (hash-data image-data) 0 16))
    
    ;; Sign the image data
    (format t "Signing image...~%")
    (let ((signature (sign-data image-data private-key)))
      (let ((preview (subseq signature 0 (min 32 (length signature)))))
        (format t "Signature: ~A...~%" preview))
      
      ;; Embed signature in chunks
      (let ((signed-chunks (embed-signature chunks signature)))
        (format t "Writing signed PNG: ~A~%" output-png)
        (write-png-chunks output-png signed-chunks)
        (format t "✓ Image signed successfully!~%")
        t))))
