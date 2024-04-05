#!/usr/bin/bash
# ***********************************************************************
# ***
# ** My Personal ZSH Config
# ***
#
#  ██████╗  █████╗ ███████╗██╗  ██╗
#  ██╔══██╗██╔══██╗██╔════╝██║  ██║
#  ██████╔╝███████║███████╗███████║
#  ██╔══██╗██╔══██║╚════██║██╔══██║
#  ██████╔╝██║  ██║███████║██║  ██║
#  ╚═════╝ ╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝



# ***********************************************************************
# ***
# ** better defaults
# ***


# use vi keybinding
set -o vi

# set neovim as default editor
EDITOR=nvim


# colored prompt
#  export PS1="\e[1;32m \w > \e[m"
# force_color_prompt=yes

 
# ***********************************************************************
# ***
# *** aliases
# ***
alias ls='ls --color=auto'
alias ll='ls -al --color=auto'
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

alias bat='batcat'
alias fd='fdfind'

## configure nvm
source /usr/share/nvm/init-nvm.sh





# machine specific config 
export JAVA_HOME="/home/moamen/tools/jdks/temurin-21.0.2"
MAVEN_BIN="/home/moamen/tools/apache-maven-3.9.6/bin"
GRADLE_BIN="/home/moamen/tools/gradle-8.6/bin"
export DOTNET_ROOT=$HOME/.dotnet
export PATH="$PATH:$JAVA_HOME/bin:$MAVEN_BIN:$GRADLE_BIN:$DOTNET_ROOT:$DOTNET_ROOT/tools"
