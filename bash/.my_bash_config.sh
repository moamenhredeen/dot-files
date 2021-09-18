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


################### formated output functions #############################

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


################### utility functions #############################
function edit_config_file(){
    nvim ~/.my_bash_config.sh
}

function reload_config_file(){
    . ~/.bashrc
    echo -e "$(cprint -gl $(symbol done)) file reloaded"
}

####################### aliases ##################################
### navigation 
alias v='nvim'
alias fm='xdg-open'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

### python 
alias py='python'
alias venv='python -m venv'
alias pyd='source ~/.python-envs/default/bin/activate'
alias pyls='ls -l ~/.python-envs/'

### windows style 
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



####################### maven  ##################################
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


####################### my  ##################################
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
###################################################################
