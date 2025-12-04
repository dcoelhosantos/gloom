#!/usr/bin/env bash
# Install Gloom syntax highlighting for Vim/Neovim

set -e

echo "Installing Gloom syntax highlighting..."

# Detect editor
if command -v nvim &> /dev/null; then
    EDITOR="neovim"
    VIM_DIR="$HOME/.config/nvim"
    echo "Detected: Neovim"
elif command -v vim &> /dev/null; then
    EDITOR="vim"
    VIM_DIR="$HOME/.vim"
    echo "Detected: Vim"
else
    echo "Error: Neither Vim nor Neovim found."
    exit 1
fi

# Create directories
mkdir -p "$VIM_DIR/syntax"
mkdir -p "$VIM_DIR/ftdetect"

# Copy syntax file
cp gloom.vim "$VIM_DIR/syntax/"
echo "✓ Copied gloom.vim to $VIM_DIR/syntax/"

# Create filetype detection
echo 'au BufRead,BufNewFile *.gloom set filetype=gloom' > "$VIM_DIR/ftdetect/gloom.vim"
echo "✓ Created filetype detection at $VIM_DIR/ftdetect/gloom.vim"

echo ""
echo "Installation complete!"
echo ""
echo "To verify, open a .gloom file in $EDITOR and run:"
echo "  :set filetype?"
echo ""
echo "Should show: filetype=gloom"
