#!/usr/bin/bash

#  ██████╗  █████╗ ███████╗██╗  ██╗
#  ██╔══██╗██╔══██╗██╔════╝██║  ██║
#  ██████╔╝███████║███████╗███████║
#  ██╔══██╗██╔══██║╚════██║██╔══██║
#  ██████╔╝██║  ██║███████║██║  ██║
#  ╚═════╝ ╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝

### use vi keybinding
set -o vi

# set neovim as default editor
EDITOR=nvim


# point code constants
function symbol() {
        case $1 in
                x)              echo -e "\U274C" ;;
                done)           echo -e "\U2713" ;;
                circle)         echo -e "\U25CF" ;;
                rfinger)        echo -e "\U1F449";;
                rarrow)         echo -e "\U1F87A";;
                rarrow2)        echo -e "\U27A4" ;;
                rarrow3)        echo -e "\U27F6" ;;
                hline)          echo -e "\U2500" ;;
                *)              echo -e "" ;;
        esac
}


# colored print function
function cprint() {
        case $1 in
                white           | w)    echo -e "\033[1;37m$2\033[0m" ;;
                red             | r)    echo -e "\033[0;31m$2\033[0m" ;;
                green           | g)    echo -e "\033[0;32m$2\033[0m" ;;
                blue            | b)    echo -e "\033[0;34m$2\033[0m" ;;
                yellow          | y)    echo -e "\033[1;33m$2\033[0m" ;;
                cyan            | c)    echo -e "\033[0;36m$2\033[0m" ;;
                ligtred         |lr)    echo -e "\033[1;31m$2\033[0m" ;;
                lightgreen      |lg)    echo -e "\033[1;32m$2\033[0m" ;;
                lightblue       |lb)    echo -e "\033[1;34m$2\033[0m" ;;
                lightcyan       |lc)    echo -e "\033[1;36m$2\033[0m" ;;
        esac
}


# print helper function: draw line
function draw_hline() {
        temp=""
        for ((i = 0; i < 50; i++)); do
                temp="$temp$(symbol hline)"
        done
        echo -e "$temp"
}



# get git status for thish folder and all subfolders  
function my_git_recursive_status(){
    clear
    for i in $(ls)
    do 
	cd $i 
	if [ -d .git ] 
	then 
	    echo -e $(cprint lc "$(symbol rarrow) $(pwd)")
	    git st 
	    echo "\n"
	fi
	cd ..
    done
}


 
# aliases
alias v='nvim'
alias vim='nvim'
alias fm='xdg-open'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias tlmgr='/usr/share/texmf-dist/scripts/texlive/tlmgr.pl --usermode'
alias cls='clear'

alias py='python'
alias venv='python -m venv'
alias pymain='source ~/.python-environments/main/bin/activate'
alias pyalgo='source ~/.python-environments/algo/bin/activate'
alias pyls='ls -l ~/.python-environments/'

alias ta='tmux attach -t'
alias ts='tmux new-session -s'
alias tl='tmux list-sessions'
alias tk='tmux kill-session -t'

alias mw="./mvnw"
alias gw='./gradlew'
alias lg='lazygit'

alias hh='hstr'
alias bat='batcat'
alias fd='fdfind'
