#!/usr/bin/env bash
# run-example.sh - Easy runner for the example app

echo "════════════════════════════════════════════════════"
echo "  PNG Signature Example App"
echo "════════════════════════════════════════════════════"
echo ""

# Check if rabbit.png exists
if [ ! -f "rabbit.png" ]; then
    echo "⚠️  rabbit.png not found!"
    echo ""
    echo "Creating a simple test PNG image..."
    
    # Try to create a simple PNG using ImageMagick (if available)
    if command -v convert &> /dev/null; then
        convert -size 200x200 xc:white -pointsize 40 -draw "text 30,100 '🐰'" rabbit.png 2>/dev/null || \
        convert -size 200x200 xc:white -pointsize 40 -draw "text 30,100 'RABBIT'" rabbit.png
        echo "✓ Created test rabbit.png"
    else
        echo "ImageMagick not found. Please provide your own rabbit.png"
        echo ""
        echo "You can:"
        echo "  1. Add any PNG image and rename it to rabbit.png"
        echo "  2. Install ImageMagick and run this script again"
        exit 1
    fi
fi

echo "Loading Common Lisp and running example..."
echo ""

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

sbcl --noinform --disable-debugger \
     --load ~/quicklisp/setup.lisp \
     --eval "(require 'asdf)" \
     --eval "(push \"$SCRIPT_DIR/\" asdf:*central-registry*)" \
     --eval "(asdf:load-system :badges)" \
     --load test-app.lisp \
     --eval "(badges::run-example-app)" \
     --eval "(quit)"
