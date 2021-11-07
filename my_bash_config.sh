#!/bin/bash

################################################
#..............................................#
#.....Author      :Moamen Hredeen..............#
#.....Description :bash configuration..........# 
#..............................................#
################################################

# TODO : 
# - function : CRUD aliases
# - function : alias to open file explorer

### customizations 
set -o vi
EDITOR=nvim


##########################################################
# Helper Functions
##########################################################

function symbol(){
    
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

function cprint(){
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


function command_not_found(){
    echo -e "$(cprint -r $(symbol x)) sorry entered subcommand or option can not be found "
    echo -e "$(symbol rfinger)  use -h or --help to get help"
}

function option_not_found(){
    echo -e "$(cprint -r $(symbol x)) sorry option can be not found "
}

function draw_hline(){
    temp=""
    for (( i=0; i<50; i++ ));
    do
        temp="$temp$(symbol hline)" 
    done
    echo -e "$temp"
}

function print_section(){
    # Parameters : 
    # $1 : section name 
    echo -e "$(cprint -cl "$1")"
}

function print_command(){
    # Parameters : 
    # $1 : command name 
    # $2 : command discription
    echo -e "$(cprint -cl "$1")"
    echo -e "  $2"
}

function print_subcommand(){
    # Paramters 
    # $1 : subcommand name 
    # $2 : subcommand discription
    echo -e "$(cprint -gl $(symbol rarrow2)) $(cprint -gl "$1")\t$(cprint -w "$2")"
}

function print_option(){
    # Parameters 
    # $1 : option name 
    # $2 : option description 
    echo -e "  $(cprint -y "$1")\t$(cprint -w "$2")"
}

function print_alias(){
    # Parameters : 
    # $1 : alias 
    # $2 : command 
    echo -e "$(cprint -gl "$1")\t$(cprint -gl $(symbol rarrow3))\t$(cprint -w "$2")"
}


function edit_config_file(){
    nvim ~/.my_bash_config.sh
}

function reload_config_file(){
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

##########################################################
# ALIASES
##########################################################
# default aliases 
alias v='nvim'
alias vim='nvim'
alias fm='xdg-open'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

# python 
alias py='python'
alias venv='python -m venv'
alias pyd='source ~/.python-envs/default/bin/activate'
alias pyls='ls -l ~/.python-envs/'

# windows style 
alias cls='clear'


function aliases_help(){
    clear
    print_section "NAVIGATION"
    print_alias "v" "nvim"
    print_aulias "fm" "xdg-open"
    print_alias ".." "cd ../"
    print_alias "..." "cd ../../"
    print_alias "...." "cd ../../../"
    draw_hline
    print_section "PYTHON" 
    print_alias "py" "python"
    print_alias "venv" "python -m venv"
    print_alias "pyd" "source ~/.python-envs/default/bin/activate # activate default python environment"
    print_alias "pyls" "ls ~/.python-envs/ # list python environments"
    draw_hline
    print_section "WINDOWS-STYLE"
    print_alias "cls" "clear # clear console"
}

# define aliases at runtime 
_list() {

	if [ -s ~/.aliasme/cmd ];then
		while read name
		do
			read value
			echo "$name : $value"
		done < ~/.aliasme/cmd
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

	echo $name >> ~/.aliasme/cmd
	echo $cmd >> ~/.aliasme/cmd
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
    if [ -s ~/.aliasme/cmd ];then
        touch ~/.aliasme/cmdtemp
    	while read line
    	do
    		if [ "$line" = "$name" ]; then
    			read line #skip one more line
                echo "remove $name"
    		else
    			echo $line >> ~/.aliasme/cmdtemp
    		fi
    	done < ~/.aliasme/cmd
    	mv ~/.aliasme/cmdtemp ~/.aliasme/cmd
    fi
	_autocomplete
}

_excute() {
    if [ -s ~/.aliasme/cmd ];then
        while read -u9 line; do
            if [ "$1" = "$line" ]; then
                read -u9 line
    			eval $line
    			return 0
            fi
        done 9< ~/.aliasme/cmd
    fi
	return 1
}

_bashauto()
{
	local cur opts
	COMPREPLY=()
	cur="${COMP_WORDS[COMP_CWORD]}"

	opts=""
    if [ -s ~/.aliasme/cmd ];then
    	while read line
    	do
    		opts+=" $line"
    		read line
    	done < ~/.aliasme/cmd
    fi
	COMPREPLY=( $(compgen -W "${opts}" ${cur}) )
	return 0
}

_autocomplete()
{
	if [ $ZSH_VERSION ]; then
		# zsh
		opts=""
        if [ -s ~/.aliasme/cmd ];then
    		while read line
    		do
    			opts+="$line "
    			read line
    		done < ~/.aliasme/cmd
        fi
		compctl -k "($opts)" al
	else
		# bash
		complete -F _bashauto al
	fi
}

# TODO : configure auto completion
#_autocomplete

# TODO : add al as subcommand to my command
al(){
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
			if ! _excute $1 ; then
				echo "not found"
			fi
		fi
	fi
}




##########################################################
# maven
##########################################################

function my_maven_help(){
    print_subcommand "mvn | maven OPTION" "create maven projects"
    print_option "-q | --quick" "create maven project using maven-archetype-quickstart"
}


function my_maven(){
    case $1 in
        -q | --quick)
            echo "maven-archetype-quickstart was selected"
            echo "create maven project ..."
            mvn \
                archetype:generate \
                -DarchetypeGroupId="org.apache.maven.archetypes" \
                -DarchetypeArtifactId="maven-archetype-quickstart" \
                -DarchetypeVersion="1.4"
            ;;
        -h | --help)
            my_maven_help
            ;;
        *)
            option_not_found
            echo -e "\n"
            my_maven_help
        ;;
    esac
    
}


##########################################################
# Entry Point
##########################################################
function my(){

    case $1 in
    ## options 
        -h | --help) 
            my_help
            ;;
        -a | --alias) 
            aliases_help
            ;;
        -r | --reload) 
            reload_config_file
            ;;
        -e | --edit)
            edit_config_file 
            ;;
    ## subcommands
        mvn | maven)
            my_maven $2
            ;;
    ## other cases 
        *) 
            command_not_found
            ;;
    esac
    
}


function my_help(){
    clear 
    print_section "COMMAND"
    print_command "my SUBCOMMAND [OPTION]" \
        "my command description"
    draw_hline

    print_section "OPTIONS"
    print_option "-h, --help"       "help"
    print_option "-r, --reload"     "reload .bashrc"
    print_option '-a, -aliases'     'list user defined aliases'
    draw_hline

    print_section "SUBCOMMANDS"
    my_maven_help
}

##########################################################
# tests
##########################################################
# TODO : implement function to parse and format user defined alises
function get_argument(){
	while read line; 
	do 
		if [[ $line == alias* ]]
		then
			echo $line 
		fi
	done < ~/.my_bash_config.sh
}


