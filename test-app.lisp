;;;; test-app.lisp - Example application demonstrating the complete workflow

(in-package #:badges)

(defun run-example-app ()
  "Complete demonstration using rabbit.jpg"
  (format t "~%╔════════════════════════════════════════════════════════════╗~%")
  (format t "║  PNG Signature Demo - Using rabbit.png                    ║~%")
  (format t "╚════════════════════════════════════════════════════════════╝~%~%")
  
  ;; Check if rabbit.png exists
  (unless (probe-file "rabbit.png")
    (format t "⚠ rabbit.png not found! Please add a rabbit.png file first.~%")
    (format t "You can use any PNG image and rename it to rabbit.png~%")
    (return-from run-example-app nil))
  
  ;; Step 1: Generate keypair
  (format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
  (format t "Step 1: Generating Ed25519 Keypair~%")
  (format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
  
  (multiple-value-bind (priv pub)
      (generate-keypair)
    (save-key priv "rabbit-private.key")
    (save-key pub "rabbit-public.key")
    (format t "✓ Private key saved to: rabbit-private.key~%")
    (format t "✓ Public key saved to: rabbit-public.key~%")
    
    ;; Step 2: Sign the image
    (format t "~%━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
    (format t "Step 2: Signing rabbit.png~%")
    (format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
    
    (sign-image "rabbit.png" "rabbit-signed.png" "rabbit-private.key")
    
    ;; Step 3: Extract and display signature
    (format t "~%━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
    (format t "Step 3: Extracting Signature from rabbit-signed.png~%")
    (format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
    
    (let* ((signed-chunks (read-png-chunks "rabbit-signed.png")))
      (multiple-value-bind (extracted-sig metadata raw)
          (extract-signature signed-chunks)
        (declare (ignore raw))
        (if extracted-sig
            (progn
              (format t "✓ Signature extracted successfully!~%")
              (format t "Signature (first 64 chars): ~A...~%"
                      (subseq extracted-sig 0 (min 64 (length extracted-sig))))
              (format t "Signature length: ~A characters~%" (length extracted-sig))
              (%print-attestation metadata))
            (format t "✗ No signature found!~%"))))
      
      ;; Step 4: Verify with correct key
      (format t "~%━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
      (format t "Step 4: Verifying with Correct Public Key~%")
      (format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
      
      (if (verify-image "rabbit-signed.png" "rabbit-public.key")
          (format t "~%🎉 SUCCESS: Signature is VALID!~%")
          (format t "~%❌ FAILED: Signature is INVALID!~%"))
      
      ;; Step 5: Verify with wrong key (should fail)
      (format t "~%━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
      (format t "Step 5: Testing with Wrong Key (should fail)~%")
      (format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
      
      (multiple-value-bind (wrong-priv wrong-pub)
          (generate-keypair)
        (save-key wrong-pub "wrong-public.key")
        (format t "Generated wrong key for testing...~%")
        
        (if (verify-image "rabbit-signed.png" "wrong-public.key")
            (format t "~%❌ UNEXPECTED: Signature verified with wrong key!~%")
            (format t "~%✓ EXPECTED: Signature correctly rejected with wrong key!~%")))
      
      ;; Step 6: Test modified image (should fail)
      (format t "~%━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
      (format t "Step 6: Testing Modified Image (should fail)~%")
      (format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
      
      ;; Tamper with the signature
      (let ((tampered-chunks (read-png-chunks "rabbit-signed.png")))
        ;; Remove signature and add a fake one
        (setf tampered-chunks 
              (remove-if (lambda (chunk)
                           (and (string= (png-chunk-type chunk) "tEXt")
                                (multiple-value-bind (keyword text)
                                    (parse-text-chunk chunk)
                                  (string= keyword "Signature"))))
                        tampered-chunks))
        ;; Add fake signature
        (multiple-value-bind (payload meta)
            (make-signature-payload "FAKE_SIGNATURE_1234567890ABCDEF"
                                     '(("version" . "2")
                                       ("scope" . "image+metadata")
                                       ("subject" . "Mallory")
                                       ("note" . "Tampered")))
          (declare (ignore meta))
          (setf tampered-chunks 
                (embed-signature tampered-chunks payload)))
        
        (write-png-chunks "rabbit-tampered.png" tampered-chunks)
        (format t "Created tampered image with fake signature...~%")
        
        (if (verify-image "rabbit-tampered.png" "rabbit-public.key")
            (format t "~%❌ UNEXPECTED: Tampered signature verified!~%")
            (format t "~%✓ EXPECTED: Tampered signature correctly rejected!~%")))
      
      ;; Summary
      (format t "~%╔════════════════════════════════════════════════════════════╗~%")
      (format t "║  Summary                                                   ║~%")
      (format t "╚════════════════════════════════════════════════════════════╝~%")
      (format t "~%Files created:~%")
      (format t "  • rabbit-private.key      - Private signing key (keep secret!)~%")
      (format t "  • rabbit-public.key       - Public verification key~%")
      (format t "  • rabbit-signed.png       - Signed image with embedded signature~%")
      (format t "  • rabbit-tampered.png     - Tampered image (for testing)~%")
      (format t "  • wrong-public.key        - Wrong key (for testing)~%")
      (format t "~%Next steps:~%")
      (format t "  1. Share rabbit-signed.png and rabbit-public.key with others~%")
      (format t "  2. They can verify: (badges:verify-image \"rabbit-signed.png\" \"rabbit-public.key\")~%")
      (format t "  3. Any modification to the image will invalidate the signature!~%")
  (format t "~%")))

;; Simple helper to check if an image has a signature
(defun check-signature-status (filepath)
  "Quick check if a file has a signature"
  (format t "~%Checking: ~A~%" filepath)
  (handler-case
      (let* ((chunks (read-png-chunks filepath))
             (sig (extract-signature chunks)))
        (if sig
            (progn
              (format t "✓ Has signature: ~A...~%" (subseq sig 0 (min 32 (length sig))))
              (format t "  Signature length: ~A bytes~%" (length sig))
              t)
            (progn
              (format t "✗ No signature found~%")
              nil)))
    (error (e)
      (format t "✗ Error reading file: ~A~%" e)
      nil)))

;; Quick verification command
(defun quick-verify (image-file key-file)
  "Quick verification with minimal output"
  (handler-case
      (if (verify-image image-file key-file)
          (progn
            (format t "~%✅ VERIFIED~%")
            t)
          (progn
            (format t "~%❌ INVALID~%")
            nil))
    (error (e)
      (format t "~%❌ ERROR: ~A~%" e)
      nil)))

;; Export the functions
(export '(run-example-app check-signature-status quick-verify))
