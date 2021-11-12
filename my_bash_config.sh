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

# constants (  )
ALIASES_FILE="/home/moamenhraden/new-git/dotfiles/aliases.sh"


### customizations
set -o vi
EDITOR=nvim

####################################################################################
# Helper Functions
####################################################################################

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

function command_not_found() {
        echo -e "$(cprint r $(symbol x)) sorry entered subcommand or option can not be found "
        echo -e "$(symbol rfinger)  use -h or --help to get help"
}

function option_not_found() {
        echo -e "$(cprint r $(symbol x)) sorry option can be not found "
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
        echo -e "$(cprint lc "$1")"
}

function print_command() {
        # Parameters :
        # $1 : command name
        # $2 : command discription
        echo -e "$(cprint lc "$1")"
        echo -e "  $2"
}

function print_subcommand() {
        # Paramters
        # $1 : subcommand name
        # $2 : subcommand discription
        echo -e "$(cprint lg $(symbol rarrow2)) $(cprint -gl "$1")\t$(cprint w "$2")"
}

function print_option() {
        # Parameters
        # $1 : option name
        # $2 : option description
        echo -e "  $(cprint y "$1")\t$(cprint w "$2")"
}

function print_alias() {
        # Parameters :
        # $1 : alias
        # $2 : command
        echo -e "$(cprint lg "$1")\t$(cprint -gl $(symbol rarrow3))\t$(cprint w "$2")"
}

function edit_config_file() {
        nvim ~/.my_bash_config.sh
}

function reload_config_file() {
        # case $SHELL in
        #         *bash*) source ~/.bashrc ;;
        #         *zsh*) source ~/.zshrc ;;
        # esac
        source ~/.zshrc
        echo -e "$(cprint lg $(symbol done)) file reloaded"
}

####################################################################################
# ALIASES MANAGER
####################################################################################
source $ALIASES_FILE 

function _aliasmgr_add_alias(){
        echo "alias $1='${@:2}'\n" >> $ALIASES_FILE
        alias $1=${@:2}
}

function _aliasmgr_remove_alias(){
	awk -i inplace -v pattern="$1" '$0 !~ pattern'  $ALIASES_FILE
        unalias $1
}

function _aliasmgr_clear_aliases(){
        eval "$(awk 'BEGIN {start=0} /MANAGED/ {start=1; next}  /alias/ { FS = "="; if (start == 1) print "un"$1";" }' aliases.sh)"
        awk -i inplace '/MANAGED/ {print; exit} {print}' $ALIASES_FILE
}

function _aliasmgr_list() {
        clear
        cat $ALIASES_FILE |
                awk ' /#\+ALIAS_SECTION/ { print "\n\033[36;1m ❯❯❯  " substr($0, 17) "\033[0m"} 
/^alias/ { FS = "="; print "\t\033[32;1m ⟶  " substr($1, 6) "\033[0m"  "\t\033[37m" $2 "\033[0m"}'

}

function _aliasmgr_help(){
        print_section   "Usage:"
        print_option    "als -a, --add [name] [command]"        "add alias command with name"
        print_option    "als -d, --remove  [name]"              "remove alias by name"
        print_option    "als -c, --clear"                       "remove managed aliases"
        print_option    "als -l, --list"                        "alias list"
        print_option    "als -h, --help"                        "get help"        
}



function _aliasmgr() {
        case $1 in 
                -a | --add)             _aliasmgr_add_alias "${@:2}"    ;;
                -d | --remove)          _aliasmgr_remove_alias $2 	;;
                -c | --clear)           _aliasmgr_clear_aliases         ;;
                -l | --list)            _aliasmgr_list                  ;;
                -h | --help)            _aliasmgr_help                  ;;
                ######## other cases 
                *)                      command_not_found               ;;
        esac
}

##########################################################
# Entry Point
##########################################################
function _print_tasks() {
        cat ~/.my_bash_config.sh | awk '/^# *TODO *:/ { FS = ":"; print "\033[32;1m - TODO:  \033[0m"  "\033[37m" substr($0, 8) "\033[0m"}'

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

function my() {

        case $1 in
                ######## options
                -h | --help)            my_help                         ;;
                -r | --reload)          reload_config_file              ;;
                -e | --edit)            edit_config_file                ;;
                -t | --task)            _print_tasks                    ;;

                ######## subcommands 
                als | aliasmgr)         _aliasmgr "${@:2}"              ;;

                ######## other cases 
                *)                      command_not_found               ;;
        esac

}
