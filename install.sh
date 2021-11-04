#!/bin/bash 

#emacs 
# create .emacs.d directory if not exists
mkdir -p ~/.emacs.d
ln -s "$(pwd)/init.el" ~/.emacs.d/init.el
ln -s "$(pwd)/config.org" ~/.emacs.d/config.org

# neovim 
mkdir -p ~/.config/nvim
ln -s "$(pwd)/init.vim" ~/.config/nvim/init.vim

# vim 
ln -s "$(pwd)/vimrc" ~/.vimrc

# ideavim 
ln -s "$(pwd)/ideavimrc" ~/.ideavimrc

# tmux 
ln -s "$(pwd)/tmux.conf" ~/.tmux.conf

# git 
ln -s "$(pwd)/gitconfig" ~/.gitconfig

# bash 
ln -s "$(pwd)/my_bash_config.sh" ~/.my_bash_config.sh
case $SHELL in
	*zsh*)
		echo "source ~/.my_bash_config.sh" >> .zshrc
		;;
	*bash*)
		echo "source ~/.my_bash_config.sh" >> .bashrc
		;;
esac
