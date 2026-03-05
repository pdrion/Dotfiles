#!/bin/bash
# =============================================================================
# tmux-attach.sh — Selecteur interactif pour s'attacher a une session
# Usage: ~/workspaces/tmux-attach.sh  (ou alias: tw)
# =============================================================================

# Si deja dans tmux, proposer de switcher
if [ -n "$TMUX" ]; then
    tmux choose-tree -s
    exit 0
fi

# Verifier qu'il y a des sessions
if ! tmux list-sessions &>/dev/null; then
    echo "Aucune session tmux active."
    read -p "Lancer les sessions de travail ? (o/n) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Oo]$ ]]; then
        ~/workspaces/tmux-start.sh
    else
        exit 0
    fi
fi

echo ""
echo "========================================="
echo "  Sessions tmux disponibles"
echo "========================================="
echo ""

# Lister les sessions avec des numeros
sessions=()
i=1
while IFS= read -r line; do
    sessions+=("$line")
    name=$(echo "$line" | cut -d: -f1)
    windows=$(echo "$line" | grep -oP '\d+ windows')
    echo "  $i) $name  ($windows)"
    ((i++))
done < <(tmux list-sessions)

echo ""
echo "  0) Toutes (vue arborescente)"
echo ""

read -p "Choix [1]: " choice
choice=${choice:-1}

if [ "$choice" -eq 0 ] 2>/dev/null; then
    # Attacher a la premiere session puis ouvrir l'arbre
    first=$(echo "${sessions[0]}" | cut -d: -f1)
    tmux attach-session -t "$first" \; choose-tree -s
elif [ "$choice" -ge 1 ] && [ "$choice" -le "${#sessions[@]}" ] 2>/dev/null; then
    name=$(echo "${sessions[$((choice-1))]}" | cut -d: -f1)
    tmux attach-session -t "$name"
else
    echo "Choix invalide"
    exit 1
fi
