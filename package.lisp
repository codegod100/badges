;;;; package.lisp

(defpackage #:badges
  (:use #:cl)
  (:export
   ;; Main API
   #:sign-image
   #:verify-image
   
   ;; Key management
   #:generate-keypair
   #:save-key
   #:load-key
   
   ;; Low-level PNG operations
   #:read-png-chunks
   #:write-png-chunks
   #:extract-signature
   #:embed-signature))
