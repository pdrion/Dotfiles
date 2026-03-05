#!/bin/bash
# =============================================================================
# install.sh — Lie les dotfiles partages vers ~
# Usage: cd ~/dotfiles && ./install.sh
# =============================================================================

DOTFILES_DIR="$(cd "$(dirname "$0")" && pwd)"

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

link_file() {
    local src="$DOTFILES_DIR/$1"
    local dest="$HOME/$2"

    if [ -L "$dest" ]; then
        echo -e "${YELLOW}[skip]${NC} $dest (deja un lien)"
    elif [ -f "$dest" ]; then
        mv "$dest" "$dest.backup"
        ln -s "$src" "$dest"
        echo -e "${GREEN}[link]${NC} $dest (backup: $dest.backup)"
    else
        ln -s "$src" "$dest"
        echo -e "${GREEN}[link]${NC} $dest"
    fi
}

echo "Installation des dotfiles depuis $DOTFILES_DIR"
echo ""

link_file "tmux.conf" ".tmux.conf"
link_file "workspaces/tmux-start.sh" "workspaces/tmux-start.sh"
link_file "workspaces/tmux-attach.sh" "workspaces/tmux-attach.sh"
link_file "workspaces/tmux-help.sh" "workspaces/tmux-help.sh"

echo ""
echo "Fichiers NON partages (restent locaux a chaque machine) :"
echo "  ~/.tmux.local.conf    — surcharges tmux locales"
echo "  ~/.zshrc              — config shell (prompt, couleurs, aliases)"
echo "  tmux history/resurrect — sessions tmux"
echo ""
echo "Done!"
