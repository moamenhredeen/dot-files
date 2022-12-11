#! /bin/sh

#+ALIAS_SECTION GENERAL
alias v='nvim'
alias vim='nvim'
alias fm='xdg-open'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias tlmgr='/usr/share/texmf-dist/scripts/texlive/tlmgr.pl --usermode'

#+ALIAS_SECTION PYTHON
alias py='python'
alias venv='python -m venv'
alias pymain='source ~/.python-environments/main/bin/activate'
alias pyalgo='source ~/.python-environments/algo/bin/activate'
alias pyls='ls -l ~/.python-environments/'

#+ALIAS_SECTION TMUX
alias ta='tmux attach -t'
alias ts='tmux new-session -s'
alias tl='tmux list-sessions'
alias tk='tmux kill-session -t'

#+ALIAS_SECTION DOCKER
alias dockerli="sudo docker image ls"
alias dockerlc="sudo docker container ls"
alias dockerdi="sudo docker image rm"
alias dockerdc="sudo docker container rm"
alias dockerlg="sudo docker container logs"
alias dockerdd="sudo docker container prune -f"
alias dockerph="sudo docker push"
alias dockerpu="sudo docker pull"
alias dockerb="sudo docker build"
alias dockerr="sudo docker run --rm --name"
alias dockerrd="sudo docker run -d --name"
alias dockerrt="sudo docker run --name test-container --rm -it"

#+ALIAS_SECTION MAVEN
alias mvnag='mvn archetype:generate'
alias mvnc='mvn clean'
alias mvncd='mvn clean deploy'
alias mvnci='mvn clean install'
alias mvncom='mvn compile'
alias mvncp='mvn clean package'
alias mvnct='mvn clean test'
alias mvncv='mvn clean verify'
alias mvnd='mvn deploy'
alias mvndocs='mvn dependency:resolve -Dclassifier=javadoc'
alias mvndt='mvn dependency:tree'
alias mvnfmt='mvn fmt:format'
alias mvnp='mvn package'
alias mvns='mvn site'
alias mvnsrc='mvn dependency:sources'
alias mvnt='mvn test'

#+ALIAS_SECTION WINDOWS STYLE
alias cls='clear'


#+ALIAS_SECTION ARCHLINUX
alias pacmani='sudo pacman -S'
alias pacmans='pacman -Ss'
alias pacmanr='sudo pacman -R'
alias pacmanq='sudo pacman -Qs'
alias pacmanu='sudo pacman -Qtdq'
alias pacmanc='sudo pacman -R $(pacman -Qtdq)'


#+ALIAS_SECTION TOOLS
alias hh='hstr'
alias bat='batcat'
alias fd='fdfind'

############################################################################
#+ALIAS_SECTION MANAGED
alias tarf='tar -xzvf'
alias docker='sudo docker'
alias syss='sudo systemctl status'
alias sysr='sudo systemctl start'
alias lg='lazygit'


alias gw='./gradlew'
