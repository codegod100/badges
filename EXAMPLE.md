# Example App Instructions

This directory contains `test-app.lisp` which demonstrates the complete workflow of signing and verifying `rabbit.jpg`.

## Quick Start

### Option 1: Using SBCL directly

```lisp
;; Start SBCL
sbcl

;; Load the system
(require 'asdf)
(asdf:load-system :badges)

;; Load the test app
(load "test-app.lisp")

;; Run the complete example
(badges::run-example-app)
```

### Option 2: Using the shell script

```bash
# Make it executable
chmod +x run-example.sh

# Run it
./run-example.sh
```

### Option 3: Manual step-by-step

```lisp
;; Load the system
(asdf:load-system :badges)
(in-package :badges)

;; 1. Generate keys
(keygen "rabbit-private.key" "rabbit-public.key")

;; 2. Sign the image
(sign-image "rabbit.jpg" "rabbit-signed.jpg" "rabbit-private.key")

;; 3. Check if signature exists
(let ((chunks (read-png-chunks "rabbit-signed.jpg")))
  (extract-signature chunks))

;; 4. Verify the signature
(verify-image "rabbit-signed.jpg" "rabbit-public.key")
```

## What the Example App Does

1. **Generates a keypair** - Creates `rabbit-private.key` and `rabbit-public.key`
2. **Signs rabbit.jpg** - Creates `rabbit-signed.jpg` with embedded signature
3. **Extracts the signature** - Demonstrates reading the signature from the tEXt chunk
4. **Verifies with correct key** - Should succeed ✓
5. **Tests with wrong key** - Should fail (proves security)
6. **Tests with tampered signature** - Should fail (proves integrity)

## Helper Functions

```lisp
;; Quick check if a file has a signature
(badges::check-signature-status "rabbit-signed.jpg")

;; Quick verification
(badges::quick-verify "rabbit-signed.jpg" "rabbit-public.key")
```

## Expected Output

```
╔════════════════════════════════════════════════════════════╗
║  PNG Signature Demo - Using rabbit.jpg                    ║
╚════════════════════════════════════════════════════════════╝

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Step 1: Generating Ed25519 Keypair
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
✓ Private key saved to: rabbit-private.key
✓ Public key saved to: rabbit-public.key

...

🎉 SUCCESS: Signature is VALID!
```

## Files Created

After running the example:
- `rabbit-signed.jpg` - Your signed image (with signature in tEXt chunk)
- `rabbit-private.key` - Keep this secret!
- `rabbit-public.key` - Share this for verification
- `rabbit-tampered.jpg` - Test file showing failed verification
- `wrong-public.key` - Test file for wrong key verification

## Troubleshooting

**"rabbit.jpg not found"**
- Add any PNG image and rename it to `rabbit.jpg`
- The script will try to create one with ImageMagick if available

**"Package BADGES does not exist"**
- Make sure you load the system first: `(asdf:load-system :badges)`

**"Component not found"**
- Install dependencies: `(ql:quickload '(:ironclad :zpng :babel))`
