;;;; sign.lisp - Image signing functionality

(in-package #:badges)

(defun %normalize-signature-metadata (metadata image-hash)
  "Return a plist copy of METADATA enriched with defaults for signing."
  (let ((plist '()))
    ;; Copy non-nil entries into a fresh plist
    (loop for (key value) on metadata by #'cddr do
          (when value
            (setf (getf plist key) value)))
    ;; Normalize issued-at variants
    (let* ((issued (or (getf plist :issued-at)
                       (getf plist :issued_at)
                       (%current-iso8601-timestamp))))
      (remf plist :issued-at)
      (remf plist :issued_at)
      (setf (getf plist :issued-at) issued))
    ;; Always embed the image hash
    (setf (getf plist :image-hash) image-hash)
    plist))

(defun %print-attestation (metadata-alist)
  "Pretty-print attestation metadata alist."
  (when metadata-alist
    (format t "Attestation details:~%")
    (dolist (pair metadata-alist)
      (let ((key (car pair))
            (value (cdr pair)))
        (unless (member key '("version" "signature") :test #'string=)
          (let* ((label (substitute #\Space #\_ key))
                 (label (substitute #\Space #\- label))
                 (pretty (string-capitalize label)))
            (format t "  ~A: ~A~%" pretty value)))))))

(defun sign-image (input-png output-png private-key-file &key metadata)
  "Sign an image, embedding the signature (and metadata) inside a tEXt chunk."
  (format t "Reading PNG: ~A~%" input-png)
  (let* ((chunks (read-png-chunks input-png))
         (image-data (get-image-data chunks))
         (image-hash (hash-data image-data))
         (private-key (load-private-key private-key-file)))
    
    (format t "Image data size: ~A bytes~%" (length image-data))
    (format t "Image hash: ~A~%" (subseq image-hash 0 16))
    
    ;; Sign the image data
    (format t "Signing image...~%")
    (let* ((signature (sign-data image-data private-key))
           (normalized-metadata (%normalize-signature-metadata metadata image-hash))
           (payload (multiple-value-bind (payload metadata-alist)
                        (make-signature-payload signature normalized-metadata)
                      (%print-attestation metadata-alist)
                      payload)))
      (let ((preview (subseq signature 0 (min 32 (length signature)))))
        (format t "Signature: ~A...~%" preview))
      
      ;; Embed signature in chunks
      (let ((signed-chunks (embed-signature chunks payload)))
        (format t "Writing signed PNG: ~A~%" output-png)
        (write-png-chunks output-png signed-chunks)
        (format t "✓ Image signed successfully!~%")
        t))))
