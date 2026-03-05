#!/bin/bash
# =============================================================================
# tmux-help.sh — Pense-bete tmux + Claude Code
# Usage: thelp
# =============================================================================

CYAN='\033[0;36m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BOLD='\033[1m'
DIM='\033[2m'
NC='\033[0m'

cat << 'HEADER'

  ╔═══════════════════════════════════════════════════╗
  ║       TMUX + CLAUDE CODE — PENSE-BETE            ║
  ║       Prefixe = Ctrl-b (standard)                ║
  ╚═══════════════════════════════════════════════════╝

HEADER

echo -e "${BOLD}━━━ MES ALIASES RAPIDES ━━━${NC}"
echo ""
echo -e "  ${GREEN}ts${NC}           Lancer toutes les sessions"
echo -e "  ${GREEN}tw${NC}           Selecteur interactif de session"
echo -e "  ${GREEN}tl${NC}           Lister les sessions actives"
echo -e "  ${GREEN}ta${NC} <nom>     S'attacher a une session  ${DIM}(ex: ta meetingbaas)${NC}"
echo -e "  ${GREEN}tk${NC} <nom>     Tuer une session           ${DIM}(ex: tk art_spy)${NC}"
echo -e "  ${GREEN}thelp${NC}        Afficher cette aide"
echo ""
echo -e "  ${CYAN}mb${NC}  → meetingbaas    ${CYAN}pa${NC}  → phiphi_assist"
echo -e "  ${CYAN}as${NC}  → art_spy        ${CYAN}ro${NC}  → ringoverr"
echo -e "  ${CYAN}mma${NC} → mac_mini_admin"
echo ""

echo -e "${BOLD}━━━ NAVIGUER ENTRE SESSIONS (dans tmux) ━━━${NC}"
echo ""
echo -e "  ${GREEN}Ctrl-b s${NC}     Arbre des sessions (naviguer + Enter)"
echo -e "  ${GREEN}Ctrl-b )${NC}     Session suivante"
echo -e "  ${GREEN}Ctrl-b (${NC}     Session precedente"
echo -e "  ${GREEN}Ctrl-b d${NC}     Se detacher (retour terminal, rien ne se perd)"
echo -e "  ${GREEN}Ctrl-b \$${NC}     Renommer la session"
echo ""

echo -e "${BOLD}━━━ NAVIGUER ENTRE FENETRES (dans une session) ━━━${NC}"
echo ""
echo -e "  ${GREEN}Ctrl-b n${NC}     Fenetre suivante"
echo -e "  ${GREEN}Ctrl-b p${NC}     Fenetre precedente"
echo -e "  ${GREEN}Ctrl-b w${NC}     Liste des fenetres"
echo -e "  ${GREEN}Ctrl-b 1${NC}     → code"
echo -e "  ${GREEN}Ctrl-b 2${NC}     → term"
echo -e "  ${GREEN}Ctrl-b 3${NC}     → logs"
echo -e "  ${GREEN}Ctrl-b c${NC}     Creer une nouvelle fenetre"
echo -e "  ${GREEN}Ctrl-b ,${NC}     Renommer la fenetre"
echo ""

echo -e "${BOLD}━━━ PANNEAUX (SPLITS) ━━━${NC}"
echo ""
echo -e "  ${GREEN}Ctrl-b |${NC}     Split horizontal (cote a cote)"
echo -e "  ${GREEN}Ctrl-b -${NC}     Split vertical (dessus/dessous)"
echo -e "  ${GREEN}Ctrl-b h/j/k/l${NC}  Naviguer entre panneaux"
echo -e "  ${GREEN}Ctrl-b H/J/K/L${NC}  Redimensionner (majuscules)"
echo -e "  ${GREEN}Ctrl-b x${NC}     Fermer le panneau actif"
echo -e "  ${GREEN}Ctrl-b z${NC}     Zoom/dezoom sur un panneau"
echo ""

echo -e "${BOLD}━━━ COPIER / COLLER ━━━${NC}"
echo ""
echo -e "  ${GREEN}Ctrl-b [${NC}     Entrer en mode copie (scroll/selection)"
echo -e "  ${GREEN}v${NC}            Commencer la selection"
echo -e "  ${GREEN}y${NC}            Copier dans le presse-papier macOS"
echo -e "  ${GREEN}q${NC}            Quitter le mode copie"
echo -e "  ${DIM}  (la souris fonctionne aussi pour scroller et selectionner)${NC}"
echo ""

echo -e "${BOLD}━━━ PERSISTENCE (SURVIT AU REBOOT) ━━━${NC}"
echo ""
echo -e "  ${GREEN}Ctrl-b Ctrl-s${NC}   Sauvegarder les sessions maintenant"
echo -e "  ${GREEN}Ctrl-b Ctrl-r${NC}   Restaurer les sessions"
echo -e "  ${DIM}  (sauvegarde auto toutes les 15 min via tmux-continuum)${NC}"
echo ""

echo -e "${BOLD}━━━ DIVERS ━━━${NC}"
echo ""
echo -e "  ${GREEN}Ctrl-b r${NC}     Recharger la config tmux"
echo -e "  ${GREEN}Ctrl-b ?${NC}     Liste de TOUS les raccourcis tmux"
echo ""

echo -e "${BOLD}━━━ CLAUDE CODE DANS TMUX ━━━${NC}"
echo ""
echo -e "  Claude Code tourne dans la fenetre ${CYAN}code${NC} de chaque session."
echo -e "  Quand tu te detaches (Ctrl-b d), Claude Code ${GREEN}continue de tourner${NC}."
echo -e "  Quand tu te re-attaches, tu retrouves ta conversation."
echo ""
echo -e "  ${YELLOW}Reprendre une conversation :${NC}"
echo -e "    ${GREEN}claude -c${NC}              Reprend la derniere conversation du dossier"
echo -e "    ${GREEN}claude -r${NC}              Selecteur interactif de conversations"
echo -e "    ${GREEN}claude -r \"mot-cle\"${NC}    Chercher une conversation par mot-cle"
echo -e "    ${GREEN}/resume${NC}                Depuis Claude Code: reprendre une conversation"
echo ""
echo -e "  ${YELLOW}Commandes utiles dans Claude Code :${NC}"
echo -e "    ${GREEN}/help${NC}                  Aide Claude Code"
echo -e "    ${GREEN}/clear${NC}                 Effacer le contexte"
echo -e "    ${GREEN}/compact${NC}               Compresser le contexte (economise des tokens)"
echo -e "    ${GREEN}/cost${NC}                  Voir la consommation"
echo ""

echo -e "${BOLD}━━━ WORKFLOW TYPE ━━━${NC}"
echo ""
echo -e "  ${DIM}# Matin: ouvrir un terminal, les sessions sont deja la${NC}"
echo -e "  ${GREEN}tw${NC}                      ${DIM}# Selecteur interactif${NC}"
echo -e "  ${GREEN}mb${NC}                      ${DIM}# Ou direct dans meetingbaas${NC}"
echo ""
echo -e "  ${DIM}# Dans tmux: switcher entre projets${NC}"
echo -e "  ${GREEN}Ctrl-b s${NC}                ${DIM}# Arbre des sessions${NC}"
echo -e "  ${GREEN}Ctrl-b )${NC}                ${DIM}# Session suivante${NC}"
echo ""
echo -e "  ${DIM}# Besoin d'un terminal a cote de Claude Code${NC}"
echo -e "  ${GREEN}Ctrl-b 2${NC}                ${DIM}# Aller sur fenetre 'term'${NC}"
echo -e "  ${GREEN}Ctrl-b |${NC}                ${DIM}# Ou splitter la fenetre actuelle${NC}"
echo ""
echo -e "  ${DIM}# Fin de journee: juste fermer le terminal${NC}"
echo -e "  ${DIM}# (tout tourne en arriere-plan, rien n'est perdu)${NC}"
echo ""
