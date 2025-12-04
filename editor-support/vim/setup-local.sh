#!/usr/bin/env bash
# Setup Gloom syntax highlighting locally (project-specific)
# This creates symlinks so changes to the syntax file are reflected immediately

set -e

echo "Setting up Gloom syntax highlighting locally..."

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

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

# Create symlinks instead of copying
SYNTAX_FILE="$SCRIPT_DIR/gloom.vim"
TARGET_SYNTAX="$VIM_DIR/syntax/gloom.vim"

if [ -L "$TARGET_SYNTAX" ]; then
    echo "✓ Symlink already exists at $TARGET_SYNTAX"
elif [ -f "$TARGET_SYNTAX" ]; then
    echo "! File exists at $TARGET_SYNTAX, backing up..."
    mv "$TARGET_SYNTAX" "$TARGET_SYNTAX.backup"
    ln -s "$SYNTAX_FILE" "$TARGET_SYNTAX"
    echo "✓ Created symlink (old file backed up)"
else
    ln -s "$SYNTAX_FILE" "$TARGET_SYNTAX"
    echo "✓ Created symlink at $TARGET_SYNTAX"
fi

# Create filetype detection
FTDETECT_FILE="$VIM_DIR/ftdetect/gloom.vim"
echo 'au BufRead,BufNewFile *.gloom set filetype=gloom' > "$FTDETECT_FILE"
echo "✓ Created filetype detection at $FTDETECT_FILE"

echo ""
echo "Local setup complete!"
echo ""
echo "The syntax file is symlinked, so any changes to:"
echo "  $SYNTAX_FILE"
echo "will be reflected immediately in your editor."
echo ""
echo "To verify, open a .gloom file in $EDITOR and run:"
echo "  :set filetype?"
echo ""
echo "Should show: filetype=gloom"
