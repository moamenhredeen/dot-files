#!/bin/bash

####################################################################################
#..................................................................................#
#.............AUTHOR      :Moamen Hredeen..........................................#
#.............DESCRIPTION :shell configuration.....................................#
#..................................................................................#
#__/\\\\\\\\\\\\\_______/\\\\\\\\\________/\\\\\\\\\\\____/\\\________/\\\_ .......#
#._\/\\\/////////\\\___/\\\\\\\\\\\\\____/\\\/////////\\\_\/\\\_______\/\\\_.......#
#.._\/\\\_______\/\\\__/\\\/////////\\\__\//\\\______\///__\/\\\_______\/\\\_......#
#..._\/\\\\\\\\\\\\\\__\/\\\_______\/\\\___\////\\\_________\/\\\\\\\\\\\\\\\_.....#
#...._\/\\\/////////\\\_\/\\\\\\\\\\\\\\\______\////\\\______\/\\\/////////\\\_....#
#....._\/\\\_______\/\\\_\/\\\/////////\\\_________\////\\\___\/\\\_______\/\\\_...#
#......_\/\\\_______\/\\\_\/\\\_______\/\\\__/\\\______\//\\\__\/\\\_______\/\\\_..#
#......._\/\\\\\\\\\\\\\/__\/\\\_______\/\\\_\///\\\\\\\\\\\/___\/\\\_______\/\\\_.#
#........_\/////////////____\///________\///____\///////////_____\///________\///__#
####################################################################################

### customizations
set -o vi
EDITOR=nvim

####################################################################################
# Helper Functions
####################################################################################

function symbol() {

        case $1 in
                x)
                        echo -e "\U274C"
                        ;;
                done)
                        echo -e "\U2713"
                        ;;
                circle)
                        echo -e "\U25CF"
                        ;;
                rfinger)
                        echo -e "\U1F449"
                        ;;
                rarrow)
                        echo -e "\U1F87A"
                        ;;
                rarrow2)
                        echo -e "\U27A4"
                        ;;
                rarrow3)
                        echo -e "\U27F6"
                        ;;
                hline)
                        echo -e "\U2500"
                        ;;
                *)
                        echo -e ""
                        ;;
        esac

}

function cprint() {
        prefix='\033['
        sufix='m'
        black="${prefix}0;30${sufix}"
        white="${prefix}1;37${sufix}"
        red="${prefix}0;31${sufix}"
        green="${prefix}0;32${sufix}"
        blue="${prefix}0;34${sufix}"
        cyan="${prefix}0;36${sufix}"
        yellow="${prefix}1;33${sufix}"
        red_light="${prefix}1;31${sufix}"
        green_light="${prefix}1;32${sufix}"
        blue_light="${prefix}1;34${sufix}"
        cyan_light="${prefix}1;36${sufix}"
        nocolor="${prefix}0${sufix}"
        case $1 in
                -w | --white)
                        echo -e "${white}$2${nocolor}"
                        ;;
                -r | --red)
                        echo -e "${red}$2${nocolor}"
                        ;;
                -g | --green)
                        echo -e "${green}$2${nocolor}"
                        ;;
                -b | --blue)
                        echo -e "${blue}$2${nocolor}"
                        ;;
                -y | --yellow)
                        echo -e "${yellow}$2${nocolor}"
                        ;;
                -c | --cyan)
                        echo -e "${cyan}$2${nocolor}"
                        ;;
                -rl | --red-light)
                        echo -e "${red}$2${nocolor}"
                        ;;
                -gl | --green-light)
                        echo -e "${green}$2${nocolor}"
                        ;;
                -bl | --blue-light)
                        echo -e "${blue}$2${nocolor}"
                        ;;
                -cl | --cyan-light)
                        echo -e "${cyan}$2${nocolor}"
                        ;;
        esac
}

function command_not_found() {
        echo -e "$(cprint -r $(symbol x)) sorry entered subcommand or option can not be found "
        echo -e "$(symbol rfinger)  use -h or --help to get help"
}

function option_not_found() {
        echo -e "$(cprint -r $(symbol x)) sorry option can be not found "
}

function draw_hline() {
        temp=""
        for ((i = 0; i < 50; i++)); do
                temp="$temp$(symbol hline)"
        done
        echo -e "$temp"
}

function print_section() {
        # Parameters :
        # $1 : section name
        echo -e "$(cprint -cl "$1")"
}

function print_command() {
        # Parameters :
        # $1 : command name
        # $2 : command discription
        echo -e "$(cprint -cl "$1")"
        echo -e "  $2"
}

function print_subcommand() {
        # Paramters
        # $1 : subcommand name
        # $2 : subcommand discription
        echo -e "$(cprint -gl $(symbol rarrow2)) $(cprint -gl "$1")\t$(cprint -w "$2")"
}

function print_option() {
        # Parameters
        # $1 : option name
        # $2 : option description
        echo -e "  $(cprint -y "$1")\t$(cprint -w "$2")"
}

function print_alias() {
        # Parameters :
        # $1 : alias
        # $2 : command
        echo -e "$(cprint -gl "$1")\t$(cprint -gl $(symbol rarrow3))\t$(cprint -w "$2")"
}

function edit_config_file() {
        nvim ~/.my_bash_config.sh
}

function reload_config_file() {
        case $SHELL in
                *bash*)
                        source ~/.bashrc
                        ;;
                *zsh*)
                        source ~/.zshrc
                        ;;
        esac
        echo -e "$(cprint -gl $(symbol done)) file reloaded"
}

####################################################################################
# ALIASES
####################################################################################

#TODO: fix first match not precessed correctly
#+ALIAS_SECTION GENERAL
alias v='nvim'
alias fm='xdg-open'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

#+ALIAS_SECTION PYTHON
alias py='python'
alias venv='python -m venv'
alias pyd='source ~/.python-envs/default/bin/activate'
alias pyls='ls -l ~/.python-envs/'

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

#+END_ALIASES

function _print_aliases() {
        clear
        cat ~/.my_bash_config.sh |
                awk ' /#\+ALIAS_SECTION/ { print "\n\033[36;1m ❯❯❯  " substr($0, 17) "\033[0m"} 
/^alias/ { FS = "="; print "\t\033[32;1m ⟶  " substr($1, 6) "\033[0m"  "\t\033[37m" $2 "\033[0m"}'

}

function _print_aliases_deprecated() {
        clear
        start_reading=false
        while read line; do
                if [[ $line == \#\+BEGIN_ALIASES* ]]; then
                        start_reading=true
                        continue
                elif [[ $line == \#\+END_ALIASES* ]]; then
                        break
                fi

                if [[ $start_reading == true ]]; then
                        if [[ $line == \#\#\#* ]]; then
                                draw_hline
                                echo -e $(cprint -cl $line)
                        else
                                echo -e $(cprint -gl $line)
                        fi
                fi

        done <~/.my_bash_config.sh
}

# define aliases at runtime
_list() {

        if [ -s ~/.aliasme/cmd ]; then
                while read name; do
                        read value
                        echo "$name : $value"
                done <~/.aliasme/cmd
        fi
}

_add() {
        #read name
        name=$1
        if [ -z $1 ]; then
                read -ep "Input name to add:" name
        fi

        #read path
        cmd="$2"
        if [ -z "$2" ]; then
                read -ep "Input cmd to add:" cmd
        fi

        echo $name >>~/.aliasme/cmd
        echo $cmd >>~/.aliasme/cmd
        echo "add: $name -> $cmd"

        _autocomplete
}

_remove() {
        #read name
        name=$1
        if [ -z $1 ]; then
                read -pr "Input name to remove:" name
        fi

        # read and replace file
        if [ -s ~/.aliasme/cmd ]; then
                touch ~/.aliasme/cmdtemp
                while read line; do
                        if [ "$line" = "$name" ]; then
                                read line #skip one more line
                                echo "remove $name"
                        else
                                echo $line >>~/.aliasme/cmdtemp
                        fi
                done <~/.aliasme/cmd
                mv ~/.aliasme/cmdtemp ~/.aliasme/cmd
        fi
        _autocomplete
}

_excute() {
        if [ -s ~/.aliasme/cmd ]; then
                while read -u9 line; do
                        if [ "$1" = "$line" ]; then
                                read -u9 line
                                eval $line
                                return 0
                        fi
                done 9<~/.aliasme/cmd
        fi
        return 1
}

_bashauto() {
        local cur opts
        COMPREPLY=()
        cur="${COMP_WORDS[COMP_CWORD]}"

        opts=""
        if [ -s ~/.aliasme/cmd ]; then
                while read line; do
                        opts+=" $line"
                        read line
                done <~/.aliasme/cmd
        fi
        COMPREPLY=($(compgen -W "${opts}" ${cur}))
        return 0
}

_autocomplete() {
        if [ $ZSH_VERSION ]; then
                # zsh
                opts=""
                if [ -s ~/.aliasme/cmd ]; then
                        while read line; do
                                opts+="$line "
                                read line
                        done <~/.aliasme/cmd
                fi
                compctl -k "($opts)" al
        else
                # bash
                complete -F _bashauto al
        fi
}

#TODO: configure auto completion
#_autocomplete

#TODO: add al as subcommand to my command
al() {
        if [ ! -z $1 ]; then
                if [ $1 = "ls" ]; then
                        _list
                elif [ $1 = "add" ]; then
                        _add $2 "$3"
                elif [ $1 = "rm" ]; then
                        _remove $2
                elif [ $1 = "-h" ]; then
                        echo "Usage:"
                        echo "al add [name] [command]      # add alias command with name"
                        echo "al rm [name]                 # remove alias by name"
                        echo "al ls                        # alias list"
                        echo "al [name]                    # execute alias associate with [name]"
                        echo "al -v                        # version information"
                        echo "al -h                        # help"
                elif [ $1 = "-v" ]; then
                        echo "aliasme 3.0.0"
                        echo "visit https://github.com/Jintin/aliasme for more information"
                else
                        if ! _excute $1; then
                                echo "not found"
                        fi
                fi
        fi
}

##########################################################
# Entry Point
##########################################################
function my() {

        case $1 in
                ## options
                -h | --help)
                        my_help
                        ;;
                -a | --alias)
                        _print_aliases
                        ;;
                -r | --reload)
                        reload_config_file
                        ;;
                -e | --edit)
                        edit_config_file
                        ;;
                -t | --task)
                        _print_tasks
                        ;;
                        ## other cases
                *)
                        command_not_found
                        ;;
        esac

}

function my_help() {
        clear
        print_section "COMMAND"
        print_command "my SUBCOMMAND [OPTION]" \
                "my command description"
        draw_hline

        print_section "OPTIONS"
        print_option "-h, --help" "help"
        print_option "-r, --reload" "reload .bashrc"
        print_option '-a, --alias' 'list user defined aliases'
        print_option '-t, --task' 'list tasks'
}

function _print_tasks() {
        cat ~/.my_bash_config.sh | awk '/^# *TODO *:/ { FS = ":"; print "\033[32;1m - TODO:  \033[0m"  "\033[37m" substr($0, 8) "\033[0m"}'

}
