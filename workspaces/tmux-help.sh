#!/bin/bash
# =============================================================================
# tmux-help.sh — Pense-bete tmux + Claude Code (s'affiche dans less)
# Usage: thelp
# =============================================================================

less << 'EOF'

  ╔═══════════════════════════════════════════════════╗
  ║       TMUX + CLAUDE CODE — PENSE-BETE            ║
  ║       Prefixe = Ctrl-b (standard)                ║
  ╚═══════════════════════════════════════════════════╝


━━━ MES ALIASES (dans le terminal, PAS dans Claude Code) ━━━

  ts           Lancer toutes les sessions
  tw           Selecteur interactif de session
  tl           Lister les sessions actives
  ta <nom>     S'attacher (ex: ta meetingbaas)
  tk <nom>     Tuer une session (ex: tk art_spy)
  dsync        Synchroniser les dotfiles
  thelp        Cette aide

  mb → meetingbaas     pa → phiphi_assist
  as → art_spy         ro → ringoverr
  mma → mac_mini_admin


━━━ SESSIONS (= tes projets) ━━━

  Ctrl-b s     Arbre des sessions (naviguer + Enter)
  Ctrl-b )     Session suivante
  Ctrl-b (     Session precedente
  Ctrl-b d     Se detacher (rien ne se perd)
  Ctrl-b $     Renommer la session


━━━ FENETRES (= onglets) ━━━

  Ctrl-b n     Fenetre suivante
  Ctrl-b p     Fenetre precedente
  Ctrl-b w     Liste des fenetres
  Ctrl-b 1     → code
  Ctrl-b 2     → term
  Ctrl-b 3     → logs
  Ctrl-b c     Nouvelle fenetre
  Ctrl-b ,     Renommer


━━━ PANNEAUX (SPLITS) ━━━

  Ctrl-b |     Split horizontal (cote a cote)
  Ctrl-b -     Split vertical (dessus/dessous)
  Ctrl-b hjkl  Naviguer entre panneaux
  Ctrl-b HJKL  Redimensionner
  Ctrl-b x     Fermer le panneau
  Ctrl-b z     Zoom/dezoom


━━━ COPIER / COLLER ━━━

  Souris : selectionner = copie auto, clic droit = colle
  Ctrl-b [     Mode copie manuel (q pour quitter)


━━━ PERSISTENCE ━━━

  Ctrl-b Ctrl-s   Sauvegarder maintenant
  Ctrl-b Ctrl-r   Restaurer
  (sauvegarde auto toutes les 15 min)


━━━ CLAUDE CODE ━━━

  Lancer (dans le terminal) :
    claude         Nouvelle conversation
    claude -c      Reprendre la derniere du dossier
    claude -r      Choisir une conversation

  Dans Claude Code :
    /resume   /compact   /cost   /clear   /help


━━━ WORKFLOW ━━━

  Matin :  tw ou mb/pa/as/ro/mma
  Bosser : claude -c dans la fenetre "code"
  Switch : Ctrl-b s
  Soir :   Ctrl-b d (tout continue en arriere-plan)


  Ctrl-b r     Recharger la config tmux
  Ctrl-b ?     TOUS les raccourcis tmux

  (q pour quitter cette aide)
EOF
