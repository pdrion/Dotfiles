#!/bin/bash
# =============================================================================
# tmux-start.sh — Lance les 4 sessions de travail avec Claude Code
# Usage: ~/workspaces/tmux-start.sh
# =============================================================================

WORKSPACES_DIR="$HOME/workspaces"

# Couleurs pour le feedback
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

create_session() {
    local name="$1"
    local dir="$WORKSPACES_DIR/$name"

    if tmux has-session -t "$name" 2>/dev/null; then
        echo -e "${YELLOW}[skip]${NC} Session '$name' existe deja"
        return
    fi

    # Creer la session avec 3 fenetres utiles
    # Fenetre 1 "code" : terminal pret pour Claude Code (lance-le avec: claude, claude -c, claude -r)
    tmux new-session -d -s "$name" -c "$dir" -n "code"

    # Fenetre 2 "term" : terminal libre pour git, builds, etc.
    tmux new-window -t "$name" -n "term" -c "$dir"

    # Fenetre 3 "logs" : terminal libre pour logs, monitoring
    tmux new-window -t "$name" -n "logs" -c "$dir"

    # Revenir sur la premiere fenetre (Claude Code)
    tmux select-window -t "$name:1"

    echo -e "${GREEN}[ok]${NC}   Session '$name' creee (code+claude, term, logs)"
}

echo "========================================="
echo "  Lancement des sessions tmux"
echo "========================================="
echo ""

create_session "meetingbaas"
create_session "phiphi_assist"
create_session "art_spy"
create_session "ringoverr"
create_session "mac_mini_admin"

echo ""
echo "========================================="
echo "  Toutes les sessions sont pretes!"
echo "========================================="
echo ""
echo -e "${CYAN}Lance 'claude' ou 'claude -c' dans la fenetre 'code' pour bosser.${NC}"
echo ""
echo "Pour s'attacher: tw  (ou alias direct: mb, pa, as, ro, mma)"
echo ""
tmux list-sessions 2>/dev/null
