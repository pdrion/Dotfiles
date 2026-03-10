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
  ${B}║       Prefixe = Ctrl-b puis la touche             ║${N}
  ${B}╚═══════════════════════════════════════════════════╝${N}

${B}━━━ MES ALIASES ━━━${N}

  ${G}ts${N}    Lancer toutes les sessions
  ${G}tw${N}    Choisir une session
  ${G}tl${N}    Lister les sessions

  ${C}mb${N} → meetingbaas     ${C}pa${N} → phiphi_assist
  ${C}as${N} → art_spy         ${C}ro${N} → ringover
  ${C}mma${N} → mac_mini_admin

${B}━━━ NAVIGUER ━━━${N}

  ${G}Ctrl-b s${N}       Arbre des sessions ${D}(le plus utile !)${N}
  ${G}Ctrl-b 1/2/3${N}   Aller a la fenetre 1=code, 2=term, 3=logs
  ${G}Ctrl-b n${N}       Fenetre suivante
  ${G}Ctrl-b p${N}       Fenetre precedente
  ${G}Ctrl-b )${N}       Session suivante
  ${G}Ctrl-b (${N}       Session precedente
  ${G}Ctrl-b d${N}       Se detacher ${D}(tout continue de tourner)${N}

${B}━━━ FENETRES ━━━${N}

  ${G}Ctrl-b c${N}       Creer une fenetre
  ${G}Ctrl-b ,${N}       Renommer la fenetre
  ${G}Ctrl-b &${N}       Fermer la fenetre

  ${Y}Deplacer une fenetre :${N}
    1. ${G}Ctrl-b .${N}   puis taper le numero de destination
       ${D}Ex: la fenetre 3 doit aller en 1 → aller sur 3, Ctrl-b . , taper 1${N}
       ${D}Si le numero est deja pris, il faut d'abord echanger (voir ci-dessous)${N}

  ${Y}Echanger deux fenetres :${N}
    1. ${G}Ctrl-b :${N}   ${D}(ouvre le prompt tmux en bas)${N}
    2. Taper : ${G}swap-window -s 3 -t 1${N}   puis Entree
       ${D}Echange la fenetre 3 et la fenetre 1${N}

  ${Y}Renumeroter toutes les fenetres (1, 2, 3...) :${N}
    1. ${G}Ctrl-b :${N}
    2. Taper : ${G}move-window -r${N}   puis Entree

${B}━━━ PANNEAUX (SPLITS) ━━━${N}

  ${G}Ctrl-b |${N}       Splitter cote a cote
  ${G}Ctrl-b -${N}       Splitter dessus/dessous
  ${G}Ctrl-b h/j/k/l${N} Se deplacer entre panneaux
  ${G}Ctrl-b z${N}       Zoom un panneau ${D}(refaire pour dezoom)${N}
  ${G}Ctrl-b x${N}       Fermer le panneau
  ${G}Ctrl-b !${N}       Transformer le panneau en fenetre
  ${G}Ctrl-b Space${N}   Changer la disposition des panneaux
  ${G}Ctrl-b H/J/K/L${N} Redimensionner ${D}(avec Maj)${N}

${B}━━━ COPIER / COLLER ━━━${N}

  ${G}Ctrl-b [${N}       Mode copie ${D}(scroller, selectionner)${N}
  ${G}v${N}              Commencer la selection
  ${G}y${N}              Copier dans le presse-papier macOS
  ${G}q${N}              Quitter le mode copie
  ${G}Ctrl-b ]${N}       Coller depuis tmux

  ${D}La souris marche aussi pour scroller et selectionner.${N}

${B}━━━ CLAUDE CODE ━━━${N}

  ${G}claude${N}         Nouvelle conversation
  ${G}claude -c${N}      Reprendre la derniere
  ${G}claude -r${N}      Choisir une conversation passee

  ${Y}Dans Claude Code :${N}
    ${G}/resume${N}  ${G}/compact${N}  ${G}/cost${N}  ${G}/clear${N}  ${G}/help${N}

${B}━━━ PERSISTENCE ━━━${N}

  ${G}Ctrl-b Ctrl-s${N}  Sauvegarder les sessions
  ${G}Ctrl-b Ctrl-r${N}  Restaurer les sessions
  ${D}Sauvegarde auto toutes les 15 min.${N}

${B}━━━ JE VEUX... ━━━${N}

  ${Y}...remettre mes fenetres dans l'ordre code/term/logs${N}
    ${G}Ctrl-b :${N} puis ${G}swap-window -s 3 -t 1${N}
    ${D}Repeter pour chaque fenetre mal placee${N}

  ${Y}...virer une fenetre en trop${N}
    Aller dessus avec ${G}Ctrl-b 4${N} puis ${G}Ctrl-b &${N}

  ${Y}...tuer une session parasite${N}
    ${G}tk nom_session${N}   ${D}(alias dans le terminal)${N}

  ${Y}...voir ce qui tourne sur toutes les sessions${N}
    ${G}Ctrl-b w${N}   ${D}(liste toutes les fenetres de toutes les sessions)${N}

  ${Y}...envoyer un panneau dans une autre fenetre${N}
    ${G}Ctrl-b :${N} puis ${G}join-pane -t :2${N}   ${D}(envoie dans la fenetre 2)${N}

  ${Y}...recharger la config tmux${N}
    ${G}Ctrl-b r${N}

  ${D}(q pour quitter cette aide)${N}
"
} | less -R
