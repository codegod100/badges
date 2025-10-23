;;;; crypto.lisp - Cryptographic operations

(in-package #:badges)

;;; Using Ed25519 signatures (modern elliptic curve scheme with small keys)

(defun generate-keypair ()
  "Generate a new Ed25519 keypair. Returns (values private-key public-key)."
  (ironclad:generate-key-pair :ed25519))

(defun hex-string-to-bytes (hex-string)
  "Convert hex string to byte array"
  (let ((bytes (make-array (/ (length hex-string) 2)
                          :element-type '(unsigned-byte 8))))
    (loop for i from 0 below (length bytes)
          for j from 0 by 2
          do (setf (aref bytes i)
                  (parse-integer hex-string :start j :end (+ j 2) :radix 16)))
    bytes))

(defun bytes-to-hex-string (bytes)
  "Convert byte array to hex string"
  (format nil "~{~2,'0x~}" (coerce bytes 'list)))

(defun ensure-octet-vector (sequence)
  "Return SEQUENCE as a simple (unsigned-byte 8) vector."
  (if (typep sequence '(simple-array (unsigned-byte 8) (*)))
      sequence
      (let* ((len (length sequence))
             (result (make-array len :element-type '(unsigned-byte 8))))
        (loop for i from 0 below len
              do (setf (aref result i) (elt sequence i)))
        result)))

(defun save-key (key filepath)
  "Save an Ed25519 key to a file (as hex string)."
  (with-open-file (stream filepath
                          :direction :output
                          :if-exists :supersede)
    (cond
      ((typep key 'ironclad:ed25519-private-key)
       (let* ((plist (ironclad:destructure-private-key key))
              (x (getf plist :x)))
         (format stream "~A~%" (bytes-to-hex-string x))))
      ((typep key 'ironclad:ed25519-public-key)
       (let* ((plist (ironclad:destructure-public-key key))
              (y (getf plist :y)))
         (format stream "~A~%" (bytes-to-hex-string y))))
      (t
       (error "Unsupported key type: ~S" (type-of key))))))

(defun load-public-key (filepath)
  "Load an Ed25519 public key from a file."
  (with-open-file (stream filepath :direction :input)
    (let* ((hex-string (string-trim '(#\Space #\Tab #\Newline #\Return)
                                    (read-line stream)))
           (y-bytes (hex-string-to-bytes hex-string)))
      (ironclad:make-public-key :ed25519 :y y-bytes))))

(defun load-private-key (filepath)
  "Load an Ed25519 private key from a file."
  (with-open-file (stream filepath :direction :input)
    (let* ((hex-string (string-trim '(#\Space #\Tab #\Newline #\Return)
                                    (read-line stream)))
           (x-bytes (hex-string-to-bytes hex-string)))
      ;; The public key can be derived from the private seed
      (let ((y-bytes (ironclad:ed25519-public-key x-bytes)))
        (ironclad:make-private-key :ed25519 :x x-bytes :y y-bytes)))))

(defun sign-data (data private-key)
  "Sign DATA with PRIVATE-KEY. Returns signature as a hex string."
  (let* ((octets (ensure-octet-vector data))
         (signature-bytes (ironclad:sign-message private-key octets))
         (hex-string (bytes-to-hex-string signature-bytes)))
    hex-string))

(defun verify-signature (data signature-hex public-key)
  "Verify SIGNATURE-HEX of DATA with PUBLIC-KEY. Returns T if valid, NIL otherwise."
  (handler-case
      (let* ((octets (ensure-octet-vector data))
             (sig-bytes (hex-string-to-bytes signature-hex)))
        (ironclad:verify-signature public-key octets sig-bytes))
    (error (e)
      (format t "Verification error: ~A~%" e)
      nil)))

(defun hash-data (data)
  "Compute SHA-256 hash of DATA for display purposes"
  (let* ((octets (ensure-octet-vector data))
         (digest (ironclad:digest-sequence :sha256 octets)))
    (bytes-to-hex-string digest)))
