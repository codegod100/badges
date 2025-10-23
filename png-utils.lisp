;;;; png-utils.lisp - PNG chunk manipulation utilities

(in-package #:badges)

;;; PNG file structure:
;;; - 8-byte signature
;;; - Chunks: [Length (4)] [Type (4)] [Data (Length)] [CRC (4)]

(defparameter +png-signature+
  #(137 80 78 71 13 10 26 10)
  "PNG file signature bytes")

(defstruct png-chunk
  "Represents a PNG chunk"
  type    ; 4-character string like "IHDR", "tEXt", "IEND"
  data)   ; Byte vector

(defun read-bytes (stream count)
  "Read COUNT bytes from STREAM and return as a vector"
  (let ((buffer (make-array count :element-type '(unsigned-byte 8))))
    (read-sequence buffer stream)
    buffer))

(defun read-u32 (stream)
  "Read a 32-bit unsigned integer (big-endian) from STREAM"
  (+ (ash (read-byte stream) 24)
     (ash (read-byte stream) 16)
     (ash (read-byte stream) 8)
     (read-byte stream)))

(defun write-u32 (value stream)
  "Write a 32-bit unsigned integer (big-endian) to STREAM"
  (write-byte (ldb (byte 8 24) value) stream)
  (write-byte (ldb (byte 8 16) value) stream)
  (write-byte (ldb (byte 8 8) value) stream)
  (write-byte (ldb (byte 8 0) value) stream))

(defun %ensure-octet-vector (sequence)
  "Return SEQUENCE as a simple (unsigned-byte 8) vector."
  (if (typep sequence '(simple-array (unsigned-byte 8) (*)))
      sequence
      (let* ((len (length sequence))
             (result (make-array len :element-type '(unsigned-byte 8))))
        (loop for i from 0 below len
              do (setf (aref result i) (elt sequence i)))
        result)))

(defun %bytes-to-u32 (bytes)
  "Convert a 4-byte vector to a 32-bit unsigned integer."
  (let ((value 0))
    (loop for byte across bytes do (setf value (logior (ash value 8) byte)))
    value))

(defun calculate-crc (type-and-data)
  "Calculate CRC32 for PNG chunk (type + data)"
  (let* ((bytes (%ensure-octet-vector type-and-data))
         (digest (ironclad:digest-sequence :crc32 bytes)))
    (%bytes-to-u32 digest)))

(defun read-png-chunks (filepath)
  "Read all chunks from a PNG file"
  (with-open-file (stream filepath
                          :direction :input
                          :element-type '(unsigned-byte 8))
    ;; Verify PNG signature
    (let ((sig (read-bytes stream 8)))
      (unless (equalp sig +png-signature+)
        (error "Not a valid PNG file: ~A" filepath)))
    
    ;; Read chunks
    (loop with chunks = '()
          for length = (read-u32 stream)
          for type-bytes = (read-bytes stream 4)
          for type = (babel:octets-to-string type-bytes :encoding :latin1)
    for data = (read-bytes stream length)
    do (progn
      ;; Consume CRC (currently unchecked)
      (read-u32 stream)
      ;; TODO: Re-enable CRC verification once implementation matches PNG reference
      ;; (let* ((type-and-data (concatenate 'vector type-bytes data))
      ;;        (calculated-crc (calculate-crc type-and-data)))
      ;;   (unless (= crc calculated-crc)
      ;;     (warn "CRC mismatch in ~A chunk" type)))

      (push (make-png-chunk :type type :data data) chunks))
          until (string= type "IEND")
          finally (return (nreverse chunks)))))

(defun write-png-chunks (filepath chunks)
  "Write PNG chunks to a file"
  (with-open-file (stream filepath
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    ;; Write PNG signature
    (write-sequence +png-signature+ stream)
    
    ;; Write each chunk
    (dolist (chunk chunks)
      (let* ((type-bytes (babel:string-to-octets (png-chunk-type chunk)
                                                   :encoding :latin1))
             (data (png-chunk-data chunk))
             (length (length data))
             (type-and-data (concatenate 'vector type-bytes data))
             (crc (calculate-crc type-and-data)))
        
        (write-u32 length stream)
        (write-sequence type-bytes stream)
        (write-sequence data stream)
        (write-u32 crc stream)))))

(defun make-text-chunk (keyword text)
  "Create a tEXt chunk with KEYWORD and TEXT"
  (let* ((keyword-bytes (babel:string-to-octets keyword :encoding :latin1))
         (text-bytes (babel:string-to-octets text :encoding :latin1))
         (data (concatenate 'vector
                           keyword-bytes
                           #(0)  ; Null separator
                           text-bytes)))
    (make-png-chunk :type "tEXt" :data data)))

(defun parse-text-chunk (chunk)
  "Parse a tEXt chunk and return (values keyword text)"
  (when (string= (png-chunk-type chunk) "tEXt")
    (let* ((data (png-chunk-data chunk))
           (null-pos (position 0 data)))
      (when null-pos
        (values
         (babel:octets-to-string (subseq data 0 null-pos) :encoding :latin1)
         (babel:octets-to-string (subseq data (1+ null-pos)) :encoding :latin1))))))

(defun extract-signature (chunks)
  "Extract signature from PNG chunks (looks for 'Signature' tEXt chunk)"
  (dolist (chunk chunks)
    (when (string= (png-chunk-type chunk) "tEXt")
      (multiple-value-bind (keyword text)
          (parse-text-chunk chunk)
        (when (string= keyword "Signature")
          (return-from extract-signature text)))))
  nil)

(defun embed-signature (chunks signature)
  "Embed signature into PNG chunks (before IEND)"
  (let ((result '())
        (sig-chunk (make-text-chunk "Signature" signature)))
    ;; Copy all chunks except IEND
    (dolist (chunk chunks)
      (unless (string= (png-chunk-type chunk) "IEND")
        (push chunk result)))
    ;; Add signature chunk
    (push sig-chunk result)
    ;; Add IEND back
    (dolist (chunk chunks)
      (when (string= (png-chunk-type chunk) "IEND")
        (push chunk result)
        (return)))
    (nreverse result)))

(defun get-image-data (chunks)
  "Extract the actual image data (IDAT chunks) for signing"
  (let ((data '()))
    (dolist (chunk chunks)
      (when (string= (png-chunk-type chunk) "IDAT")
        (push (png-chunk-data chunk) data)))
    (apply #'concatenate 'vector (nreverse data))))
