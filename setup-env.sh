#!/usr/bin/env bash
# setup-env.sh - Set up Gloom development environment
# 
# This script:
# 1. Compiles the project with cabal build
# 2. Locates the generated binary
# 3. Creates a 'gloom' alias for the current session
# 4. Adds the binary to PATH temporarily
#
# Usage:
#   source ./setup-env.sh
#   . ./setup-env.sh
#
# Note: Use 'source' or '.' to export environment variables to the current shell session.
#       For Vim/Neovim syntax highlighting, use nix-shell instead.

set -e

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
RED='\033[0;31m'
RESET='\033[0m'

echo -e "${BLUE}=== Gloom Development Environment Setup ===${RESET}\n"

if [ ! -f "bird.cabal" ]; then
    echo -e "${RED}Error: bird.cabal not found.${RESET}"
    echo "Run this script from the project root directory."
    return 1 2>/dev/null || exit 1
fi

echo -e "${YELLOW}[1/3]${RESET} Building project..."
if cabal build; then
    echo -e "${GREEN}✓${RESET} Build successful.\n"
else
    echo -e "${RED}✗${RESET} Build failed."
    return 1 2>/dev/null || exit 1
fi

echo -e "${YELLOW}[2/3]${RESET} Locating binary..."
BINARY_PATH=$(find dist-newstyle -name "haskell" -type f -executable | head -1)

if [ -z "$BINARY_PATH" ]; then
    echo -e "${RED}✗${RESET} Binary 'haskell' not found."
    return 1 2>/dev/null || exit 1
fi

BINARY_DIR=$(dirname "$BINARY_PATH")
BINARY_FULL_PATH=$(realpath "$BINARY_PATH")

echo -e "${GREEN}✓${RESET} Binary found: ${BLUE}$BINARY_FULL_PATH${RESET}\n"

echo -e "${YELLOW}[3/3]${RESET} Setting up environment..."

# Cria um diretório bin local se não existir
BIN_DIR="$(pwd)/bin"
mkdir -p "$BIN_DIR"

# Cria um symlink 'gloom' para o binário
GLOOM_SYMLINK="$BIN_DIR/gloom"
if [ -L "$GLOOM_SYMLINK" ]; then
    rm "$GLOOM_SYMLINK"
fi
ln -s "$BINARY_FULL_PATH" "$GLOOM_SYMLINK"

# Adiciona o diretório bin ao PATH (temporariamente)
if [[ ":$PATH:" != *":$BIN_DIR:"* ]]; then
    export PATH="$BIN_DIR:$PATH"
fi

# Cria um alias 'gloom' como backup (para shells interativos)
alias gloom="$BINARY_FULL_PATH"

# Exporta variáveis com os caminhos (útil para scripts)
export GLOOM_BINARY="$BINARY_FULL_PATH"
export GLOOM_ROOT="$(pwd)"

echo -e "${GREEN}✓${RESET} Environment configured!\n"

echo -e "${BLUE}═══════════════════════════════════════════════${RESET}"
echo -e "${GREEN}Gloom environment ready!${RESET}"
echo -e "${BLUE}═══════════════════════════════════════════════${RESET}"
echo ""
echo -e "  ${YELLOW}Available commands:${RESET}"
echo -e "    ${GREEN}gloom${RESET} <file.gloom>     - Execute a Gloom file"
echo -e "    ${GREEN}gloom${RESET} --help           - Show help"
echo -e "    ${GREEN}gloom${RESET} --doc            - Open documentation"
echo -e "    ${GREEN}gloom${RESET} --check <file>   - Static analysis"
echo ""
echo -e "  ${YELLOW}PATH updated:${RESET}"
echo -e "    ${BLUE}$BIN_DIR${RESET} added to PATH"
echo -e "    Symlink: ${BLUE}$GLOOM_SYMLINK${RESET}"
echo ""
echo -e "  ${YELLOW}Exported variables:${RESET}"
echo -e "    ${BLUE}\$GLOOM_BINARY${RESET} = $BINARY_FULL_PATH"
echo -e "    ${BLUE}\$GLOOM_ROOT${RESET}   = $GLOOM_ROOT"
echo ""
echo -e "  ${YELLOW}Quick test:${RESET}"
echo -e "    ${GREEN}gloom --version${RESET}"
echo -e "    ${GREEN}which gloom${RESET}"
echo ""
echo -e "${BLUE}═══════════════════════════════════════════════${RESET}"
echo ""
echo -e "${YELLOW}Note:${RESET} This environment is temporary for this shell session."
echo -e "      To make it permanent, add to ~/.bashrc or ~/.zshrc:"
echo -e "      ${BLUE}export PATH=\"\$PATH:$BIN_DIR\"${RESET}"
echo ""
echo -e "${YELLOW}Vim/Neovim:${RESET} For syntax highlighting, use nix-shell:"
echo -e "      ${BLUE}nix-shell${RESET}"
echo ""
