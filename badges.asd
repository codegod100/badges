;;;; badges.asd - ASDF system definition

(asdf:defsystem #:badges
  :description "Cryptographic signature embedding and verification for PNG images"
  :author "Your Name"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:ironclad        ; Cryptography library
               #:zpng            ; PNG manipulation
               #:chipz           ; Decompression
               #:salza2          ; Compression
               #:babel)          ; String encoding
  :components ((:file "package")
               (:file "png-utils")
               (:file "crypto")
               (:file "sign")
               (:file "verify")
               (:file "main")))
