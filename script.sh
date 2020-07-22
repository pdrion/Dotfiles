#!/usr/bin/env zsh
rm -rf  ~/.emacs.d
ln -s ~/Dotfiles/.emacs.d/ ~/.emacs.d
rm ~/.tmux.conf
ln -s ~/Dotfiles/.tmux.conf ~/.tmux.conf
rm ~/.zshrc
ln -s ~/Dotfiles/.zshrc ~/.zshrc
