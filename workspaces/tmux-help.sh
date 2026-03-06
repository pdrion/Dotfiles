#!/bin/bash
# =============================================================================
# tmux-help.sh — Pense-bete tmux + Claude Code
# Usage: thelp
# =============================================================================

C='\033[0;36m'
G='\033[0;32m'
Y='\033[1;33m'
R='\033[0;31m'
B='\033[1m'
D='\033[2m'
N='\033[0m'

{
echo -e "
  ${B}╔═══════════════════════════════════════════════════╗${N}
  ${B}║       TMUX + CLAUDE CODE — PENSE-BETE            ║${N}
  ${B}║       Prefixe = Ctrl-b (standard)                ║${N}
  ${B}╚═══════════════════════════════════════════════════╝${N}

${B}━━━ MES ALIASES ${D}(dans le terminal, PAS dans Claude Code)${N} ${B}━━━${N}

  ${G}ts${N}           Lancer toutes les sessions
  ${G}tw${N}           Selecteur interactif de session
  ${G}tl${N}           Lister les sessions actives
  ${G}ta${N} <nom>     S'attacher ${D}(ex: ta meetingbaas)${N}
  ${G}tk${N} <nom>     Tuer une session ${D}(ex: tk art_spy)${N}
  ${G}dsync${N}        Synchroniser les dotfiles
  ${G}thelp${N}        Cette aide

  ${C}mb${N} → meetingbaas     ${C}pa${N} → phiphi_assist
  ${C}as${N} → art_spy         ${C}ro${N} → ringover
  ${C}mma${N} → mac_mini_admin

${B}━━━ SESSIONS ${D}(= tes projets)${N} ${B}━━━${N}

  ${G}Ctrl-b s${N}     Arbre des sessions (naviguer + Enter)
  ${G}Ctrl-b )${N}     Session suivante
  ${G}Ctrl-b (${N}     Session precedente
  ${G}Ctrl-b d${N}     Se detacher (rien ne se perd)
  ${G}Ctrl-b \$${N}     Renommer la session

${B}━━━ FENETRES ${D}(= onglets)${N} ${B}━━━${N}

  ${G}Ctrl-b n${N}     Fenetre suivante
  ${G}Ctrl-b p${N}     Fenetre precedente
  ${G}Ctrl-b w${N}     Liste des fenetres
  ${G}Ctrl-b 1${N}     → code
  ${G}Ctrl-b 2${N}     → term
  ${G}Ctrl-b 3${N}     → logs
  ${G}Ctrl-b c${N}     Nouvelle fenetre
  ${G}Ctrl-b ,${N}     Renommer

${B}━━━ PANNEAUX (SPLITS) ━━━${N}

  ${G}Ctrl-b |${N}     Split horizontal (cote a cote)
  ${G}Ctrl-b -${N}     Split vertical (dessus/dessous)
  ${G}Ctrl-b hjkl${N}  Naviguer entre panneaux
  ${G}Ctrl-b HJKL${N}  Redimensionner
  ${G}Ctrl-b x${N}     Fermer le panneau
  ${G}Ctrl-b z${N}     Zoom/dezoom

${B}━━━ COPIER / COLLER ━━━${N}

  ${Y}Methode 1 — Mode copie tmux (recommande) :${N}
    ${G}Ctrl-b [${N}       Entrer en mode copie
    ${D}(tu peux scroller avec les fleches ou la souris)${N}
    ${G}Espace${N}         Commencer la selection
    ${D}(deplace le curseur avec les fleches pour selectionner)${N}
    ${G}Enter${N}          Copier la selection
    ${G}Ctrl-b ]${N}       Coller

  ${Y}Methode 2 — Copier vers le presse-papier macOS :${N}
    ${G}Ctrl-b [${N}       Entrer en mode copie
    ${G}v${N}              Commencer la selection
    ${G}y${N}              Copier dans le presse-papier (Cmd-V pour coller)

  ${G}q${N}              Quitter le mode copie sans rien faire

${B}━━━ PERSISTENCE ━━━${N}

  ${G}Ctrl-b Ctrl-s${N}   Sauvegarder maintenant
  ${G}Ctrl-b Ctrl-r${N}   Restaurer
  ${D}(sauvegarde auto toutes les 15 min)${N}

${B}━━━ CLAUDE CODE ━━━${N}

  ${Y}Lancer (dans le terminal) :${N}
    ${G}claude${N}         Nouvelle conversation
    ${G}claude -c${N}      Reprendre la derniere du dossier
    ${G}claude -r${N}      Choisir parmi les conversations passees

  ${R}ATTENTION :${N} claude -c et -r cherchent dans le dossier courant.
  Si tu n'as jamais lance claude dans ce dossier, il n'y a rien.
  Lance juste ${G}claude${N} pour commencer.

  ${Y}Dans Claude Code :${N}
    ${G}/resume${N}   ${G}/compact${N}   ${G}/cost${N}   ${G}/clear${N}   ${G}/help${N}

${B}━━━ WORKFLOW ━━━${N}

  Matin :  ${G}tw${N} ou ${C}mb/pa/as/ro/mma${N}
  Bosser : ${G}claude${N} ou ${G}claude -c${N} dans la fenetre code
  Switch : ${G}Ctrl-b s${N}
  Soir :   ${G}Ctrl-b d${N} (tout continue en arriere-plan)

  ${G}Ctrl-b r${N}     Recharger la config tmux
  ${G}Ctrl-b ?${N}     TOUS les raccourcis tmux

  ${D}(q pour quitter cette aide)${N}
"
} | less -R
