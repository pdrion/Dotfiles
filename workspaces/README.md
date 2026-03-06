# Tmux Workspaces + Claude Code — Mac Mini Setup

## Principe

Chaque projet a une **session tmux permanente** avec Claude Code qui tourne dedans.
Tu te detaches, tu fermes ton terminal, tu reviens plus tard : **tout est toujours la**.

```
~/workspaces/
├── meetingbaas/       # Projet MeetingBaas
├── phiphi_assist/     # Projet Phiphi Assist
├── art_spy/           # Projet Art Spy
├── ringover/         # Projet Ringoverr
├── mac_mini_admin/    # Admin Mac Mini
├── tmux-start.sh      # Lance les 4 sessions + Claude Code
├── tmux-attach.sh     # Selecteur interactif
├── tmux-help.sh       # Aide complete (alias: thelp)
└── README.md          # Ce fichier
```

Chaque session a **3 fenetres** :
| # | Fenetre | Contenu |
|---|---------|---------|
| 1 | `code`  | **Claude Code** (demarre auto, reprend la derniere conversation) |
| 2 | `term`  | Terminal libre (git, builds, npm, etc.) |
| 3 | `logs`  | Terminal libre (logs, monitoring) |

---

## Aliases rapides

| Alias | Action |
|-------|--------|
| `ts`  | Lancer toutes les sessions + Claude Code |
| `tw`  | Selecteur interactif de session |
| `tl`  | Lister les sessions actives |
| `ta <nom>` | S'attacher a une session (`ta meetingbaas`) |
| `tk <nom>` | Tuer une session |
| `thelp` | Afficher le pense-bete complet |

### Raccourcis directs par projet

| Alias | Session |
|-------|---------|
| `mb`  | meetingbaas |
| `pa`  | phiphi_assist |
| `as`  | art_spy |
| `ro`  | ringover |
| `mma` | mac_mini_admin |

---

## Comment ca marche avec Claude Code

### Le principe cle : tmux garde tout en vie

Quand tu te **detaches** d'une session tmux (`Ctrl-b d`) ou que tu fermes le terminal :
- Claude Code **continue de tourner** en arriere-plan
- Ta conversation est **intacte**
- Quand tu te re-attaches (`mb`, `tw`, etc.) tu retrouves **exactement** ou tu en etais

C'est la magie de tmux : les processus tournent dans le serveur tmux, pas dans ton terminal.

### Demarrage automatique

Le script `ts` (tmux-start.sh) :
1. Cree les 4 sessions tmux
2. Lance `claude --continue` dans la fenetre "code" de chaque session
3. Claude Code reprend automatiquement la derniere conversation de chaque dossier projet

### Reprendre une conversation Claude Code

Si Claude Code a ete ferme (ex: apres un reboot) :

```bash
# Reprendre la derniere conversation du projet (le plus courant)
claude -c

# Selecteur interactif : voir toutes tes conversations passees
claude -r

# Chercher une conversation specifique
claude -r "authentication"
claude -r "bug fix"
```

Depuis l'interieur de Claude Code, tu peux aussi faire :
- `/resume` — selecteur interactif des conversations passees

### Commandes utiles dans Claude Code

| Commande | Action |
|----------|--------|
| `/help` | Aide Claude Code |
| `/clear` | Effacer le contexte (nouvelle conversation) |
| `/compact` | Compresser le contexte (economise des tokens) |
| `/cost` | Voir la consommation de tokens |
| `/resume` | Reprendre une conversation passee |

---

## Navigation tmux — Pense-bete complet

> Prefixe : **Ctrl-b** (appuyer Ctrl+b, relacher, puis la touche suivante)
>
> Pour l'aide rapide dans le terminal : `thelp`

### Sessions (= tes projets)

| Raccourci | Action |
|-----------|--------|
| `Ctrl-b s` | **Arbre des sessions** (le plus utile !) |
| `Ctrl-b )` | Session suivante |
| `Ctrl-b (` | Session precedente |
| `Ctrl-b d` | **Se detacher** (retour terminal, rien ne s'arrete) |
| `Ctrl-b $` | Renommer la session |

### Fenetres (= onglets dans une session)

| Raccourci | Action |
|-----------|--------|
| `Ctrl-b n` | Fenetre suivante |
| `Ctrl-b p` | Fenetre precedente |
| `Ctrl-b w` | Liste des fenetres |
| `Ctrl-b 1` | Aller a fenetre 1 (code/Claude) |
| `Ctrl-b 2` | Aller a fenetre 2 (term) |
| `Ctrl-b 3` | Aller a fenetre 3 (logs) |
| `Ctrl-b c` | Creer une nouvelle fenetre |
| `Ctrl-b ,` | Renommer la fenetre |
| `Ctrl-b &` | Fermer la fenetre |

### Panneaux (= splits dans une fenetre)

| Raccourci | Action |
|-----------|--------|
| `Ctrl-b \|` | Split horizontal (cote a cote) |
| `Ctrl-b -` | Split vertical (dessus/dessous) |
| `Ctrl-b h/j/k/l` | Naviguer entre panneaux |
| `Ctrl-b H/J/K/L` | Redimensionner (majuscules) |
| `Ctrl-b z` | Zoom/dezoom sur un panneau |
| `Ctrl-b x` | Fermer le panneau actif |

### Copier / Coller

| Raccourci | Action |
|-----------|--------|
| `Ctrl-b [` | Entrer en mode copie (scroll + selection) |
| `v` | Commencer la selection |
| `y` | Copier dans le presse-papier macOS |
| `q` | Quitter le mode copie |

> La souris fonctionne aussi pour scroller et selectionner.

### Persistence (survit au reboot)

| Raccourci | Action |
|-----------|--------|
| `Ctrl-b Ctrl-s` | Sauvegarder les sessions maintenant |
| `Ctrl-b Ctrl-r` | Restaurer les sessions |

Sauvegarde automatique toutes les 15 minutes (tmux-continuum).

---

## Workflow quotidien

### Matin — se connecter

```bash
# Ouvrir un terminal. Si c'est la premiere fois :
ts              # Lance les 4 sessions + Claude Code

# Ensuite, choisir un projet :
tw              # Selecteur interactif
# ou directement :
mb              # → meetingbaas
pa              # → phiphi_assist
```

### Pendant la journee — naviguer

```
# Dans tmux, switcher entre projets :
Ctrl-b s        # Arbre des sessions → naviguer → Enter
Ctrl-b )        # Session suivante rapide

# Dans une session, changer de fenetre :
Ctrl-b 1        # → Claude Code
Ctrl-b 2        # → Terminal
Ctrl-b n/p      # → Suivante / Precedente

# Besoin d'un terminal a cote :
Ctrl-b |        # Splitter la fenetre
```

### Soir — se deconnecter

```bash
# Option 1 : Se detacher (recommande)
Ctrl-b d        # Tout continue de tourner

# Option 2 : Fermer le terminal
# C'est pareil ! tmux tourne en arriere-plan.

# Le lendemain, tw ou mb pour reprendre.
```

### Apres un reboot

```bash
# tmux-continuum restaure les sessions automatiquement
# Puis relancer Claude Code dans chaque session :
ts              # Relance les sessions manquantes + Claude Code
```

---

## Ajouter un nouveau projet

1. Creer le dossier :
   ```bash
   mkdir -p ~/workspaces/nouveau_projet
   ```

2. Ajouter dans `~/workspaces/tmux-start.sh` :
   ```bash
   create_session "nouveau_projet"
   ```

3. Ajouter un alias dans `~/.zshrc` :
   ```bash
   alias np='tmux attach-session -t nouveau_projet 2>/dev/null || echo "Absent — lance ts"'
   ```

4. Recharger : `source ~/.zshrc`

---

## Depannage

| Probleme | Solution |
|----------|----------|
| "no server running" | `ts` pour relancer les sessions |
| Claude Code ferme | `claude -c` dans la fenetre code |
| Session disparue apres reboot | `Ctrl-b Ctrl-r` puis `ts` |
| Je suis perdu | `thelp` pour le pense-bete |
| Trop de sessions | `tk nom_session` pour en supprimer |
