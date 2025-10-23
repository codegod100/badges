# Badges - PNG Image Signing & Verification

A Common Lisp application for embedding cryptographic signatures into PNG images using Ed25519 signatures stored in PNG tEXt chunks.

## Features

- **Ed25519 Cryptography**: Fast, modern, and secure digital signatures
- **PNG tEXt Chunks**: Embeds signatures without affecting image display
- **Simple API**: Easy-to-use functions for signing and verification
- **Keypair Management**: Generate and save public/private key pairs

## Architecture

```
┌─────────────────────────────────────────────────────┐
│                    Badges System                     │
├─────────────────────────────────────────────────────┤
│  ┌───────────┐  ┌──────────┐  ┌─────────────────┐ │
│  │ PNG Utils │  │  Crypto  │  │ Sign/Verify API │ │
│  └───────────┘  └──────────┘  └─────────────────┘ │
│       ↓              ↓                  ↓           │
│  Read/Write     Ed25519          Main Interface    │
│  PNG Chunks    Signatures                          │
└─────────────────────────────────────────────────────┘
```

## Dependencies

- **ironclad**: Cryptography library (Ed25519)
- **zpng**: PNG file manipulation
- **babel**: String encoding
- **chipz/salza2**: Compression support

## Installation

```bash
# Install Quicklisp (if not already installed)
# Then load dependencies:
(ql:quickload :ironclad)
(ql:quickload :zpng)
(ql:quickload :babel)

# Load the system
(asdf:load-system :badges)
```

## Usage

### 1. Generate Keypair

```lisp
(badges:keygen "private.key" "public.key")
```

This creates:
- `private.key`: Keep this SECRET! Used for signing
- `public.key`: Share this. Used for verification

### 2. Sign an Image

```lisp
(badges:sign-image "input.png" "signed.png" "private.key")
```

This:
- Reads the input PNG
- Calculates signature of the image data
- Embeds signature in a tEXt chunk (keyword: "Signature")
- Writes the signed PNG

### 3. Verify an Image

```lisp
(badges:verify-image "signed.png" "public.key")
```

This:
- Reads the PNG
- Extracts the signature from the tEXt chunk
- Verifies it against the image data
- Returns T if valid, NIL if invalid

## Command-Line Usage

```bash
# Generate keys
sbcl --load main.lisp --eval '(badges:keygen "private.key" "public.key")' --quit

# Sign image
sbcl --load main.lisp --eval '(badges:sign-image "input.png" "output.png" "private.key")' --quit

# Verify image
sbcl --load main.lisp --eval '(badges:verify-image "signed.png" "public.key")' --quit
```

## How It Works

### PNG Structure
PNG files consist of chunks:
```
[PNG Signature: 8 bytes]
[IHDR chunk: Image header]
[IDAT chunk: Image data]
[tEXt chunk: Text metadata] ← Signature goes here!
[IEND chunk: End marker]
```

### Signature Process

**Signing:**
1. Read all PNG chunks
2. Extract IDAT chunks (actual image data)
3. Generate Ed25519 signature of image data
4. Create tEXt chunk with keyword "Signature" and signature hex
5. Insert before IEND chunk
6. Write modified PNG

**Verification:**
1. Read PNG chunks
2. Find tEXt chunk with "Signature" keyword
3. Extract IDAT chunks
4. Verify signature against image data using public key

### Security Notes

- Signatures are tied to the actual image data (IDAT chunks)
- Any modification to the image will invalidate the signature
- Private keys must be kept secure
- Public keys can be freely distributed

## API Reference

### High-Level API

- `(generate-keypair)` → (values private-key public-key)
- `(sign-image input-png output-png private-key-file)` → T
- `(verify-image input-png public-key-file)` → T or NIL
- `(keygen private-key-file public-key-file)` → T

### Low-Level API

- `(read-png-chunks filepath)` → list of chunks
- `(write-png-chunks filepath chunks)` → writes file
- `(extract-signature chunks)` → signature string or NIL
- `(embed-signature chunks signature)` → modified chunks list

## License

MIT License

## TODO

- [ ] Add support for additional metadata (timestamp, author)
- [ ] Support for other signature algorithms (RSA, ECDSA)
- [ ] Batch signing/verification
- [ ] GUI interface
- [ ] Support for other image formats
