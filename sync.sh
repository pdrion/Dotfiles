#!/bin/bash
# =============================================================================
# sync.sh — Synchronise les dotfiles entre machines
# Usage: dsync (alias)
# =============================================================================

DOTFILES_DIR="$HOME/dotfiles"
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

cd "$DOTFILES_DIR" || { echo -e "${RED}Erreur: $DOTFILES_DIR introuvable${NC}"; exit 1; }

echo ""
echo "⬢ Dotfiles sync"
echo "========================================="

# Pull les changements distants d'abord
echo -e "\n${YELLOW}[pull]${NC} Recuperation des changements..."
git pull --rebase origin main 2>&1 | sed 's/^/  /'

# Verifier s'il y a des modifs locales
if [ -n "$(git status --porcelain)" ]; then
    echo -e "\n${YELLOW}[modifs]${NC} Changements locaux detectes :"
    git status --short | sed 's/^/  /'

    echo ""
    git add -A
    git commit -m "sync: $(hostname -s) — $(date '+%Y-%m-%d %H:%M')" | sed 's/^/  /'

    echo -e "\n${YELLOW}[push]${NC} Envoi..."
    git push origin main 2>&1 | sed 's/^/  /'

    echo -e "\n${GREEN}[ok]${NC} Dotfiles synchronises!"
else
    echo -e "\n${GREEN}[ok]${NC} Deja a jour, rien a sync."
fi

# Recharger tmux si actif
if tmux list-sessions &>/dev/null; then
    tmux source-file ~/.tmux.conf 2>/dev/null
    echo -e "${GREEN}[ok]${NC} Config tmux rechargee."
fi

echo ""
