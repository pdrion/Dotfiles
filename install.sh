#!/bin/bash
# =============================================================================
# install.sh — Installe les dotfiles sur une nouvelle machine
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
        return
    fi
    if [ -f "$dest" ]; then
        mv "$dest" "$dest.backup"
        echo -e "${GREEN}[backup]${NC} $dest → $dest.backup"
    fi
    ln -s "$src" "$dest"
    echo -e "${GREEN}[link]${NC} $dest"
}

echo ""
echo "⬢ Installation des dotfiles depuis $DOTFILES_DIR"
echo "========================================="
echo ""

# Creer les dossiers necessaires
echo -e "${GREEN}[mkdir]${NC} ~/workspaces/ et sous-dossiers projets"
mkdir -p ~/workspaces/{meetingbaas,phiphi_assist,art_spy,ringover,mac_mini_admin}

# Liens symboliques
link_file "tmux.conf" ".tmux.conf"
link_file "workspaces/tmux-start.sh" "workspaces/tmux-start.sh"
link_file "workspaces/tmux-attach.sh" "workspaces/tmux-attach.sh"
link_file "workspaces/tmux-help.sh" "workspaces/tmux-help.sh"
link_file "workspaces/README.md" "workspaces/README.md"

# Creer ~/.tmux.local.conf si absent
if [ ! -f ~/.tmux.local.conf ]; then
    echo "# Config locale $(hostname -s) — pas partagee" > ~/.tmux.local.conf
    echo -e "${GREEN}[create]${NC} ~/.tmux.local.conf"
fi

# Importer le profil Terminal.app bleu (machines locales uniquement)
if [ -z "$SSH_CONNECTION" ] && [ -f "$DOTFILES_DIR/local/Bleu-Local.terminal" ]; then
    open "$DOTFILES_DIR/local/Bleu-Local.terminal"
    sleep 1
    defaults write com.apple.Terminal "Default Window Settings" -string "Bleu Local"
    defaults write com.apple.Terminal "Startup Window Settings" -string "Bleu Local"
    echo -e "${GREEN}[terminal]${NC} Profil 'Bleu Local' importe et defini par defaut"
fi

# Ajouter les aliases dans .zshrc si pas deja present
if ! grep -q "Tmux Workspaces" ~/.zshrc 2>/dev/null; then
    cat >> ~/.zshrc << 'ALIASES'

# =============================================================================
# Tmux Workspaces — aliases et auto-attach
# =============================================================================
alias tw='~/workspaces/tmux-attach.sh'        # Selecteur de session
alias ts='~/workspaces/tmux-start.sh'         # Lancer toutes les sessions
alias tl='tmux list-sessions'                 # Lister les sessions
alias ta='tmux attach-session -t'             # Attacher direct: ta meetingbaas
alias tk='tmux kill-session -t'               # Tuer une session: tk meetingbaas
alias thelp='~/workspaces/tmux-help.sh'       # Pense-bete tmux + Claude Code
alias dsync='~/dotfiles/sync.sh'              # Sync dotfiles entre machines

# Raccourcis directs par projet
alias mb='tmux attach-session -t meetingbaas 2>/dev/null || echo "Session meetingbaas absente — lance ts"'
alias pa='tmux attach-session -t phiphi_assist 2>/dev/null || echo "Session phiphi_assist absente — lance ts"'
alias as='tmux attach-session -t art_spy 2>/dev/null || echo "Session art_spy absente — lance ts"'
alias ro='tmux attach-session -t ringover 2>/dev/null || echo "Session ringover absente — lance ts"'
alias mma='tmux attach-session -t mac_mini_admin 2>/dev/null || echo "Session mac_mini_admin absente — lance ts"'

# Auto-lancement des sessions tmux au login (si pas deja dans tmux)
if command -v tmux &>/dev/null && [ -z "$TMUX" ] && [ -z "$SSH_CONNECTION" ]; then
    if ! tmux has-session 2>/dev/null; then
        ~/workspaces/tmux-start.sh
    fi
fi
ALIASES
    echo -e "${GREEN}[zshrc]${NC} Aliases tmux ajoutes dans ~/.zshrc"
else
    echo -e "${YELLOW}[skip]${NC} Aliases deja dans ~/.zshrc"
fi

echo ""
echo "========================================="
echo -e "${GREEN}Done!${NC} Fais: source ~/.zshrc && ts"
echo ""
