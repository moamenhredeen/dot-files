#!/bin/bash

################################################
#..............................................#
#.....Author      :Moamen Hredeen..............#
#.....Description :bash configuration..........# 
#.....Last Update :25.08.2021..................#
#..............................................#
################################################


############### colors ##################
prefix='\033['
sufix='m'
black="${prefix}0;30${sufix}"
white="${prefix}1;37${sufix}"
red="${prefix}0;31${sufix}"
green="${prefix}0;32${sufix}"
blue="${prefix}0;34${sufix}"
cyan="${prefix}0;36${sufix}"
yellow="${prefix}1;33${sufix}"
lred="${prefix}1;31${sufix}"
lgreen="${prefix}1;32${sufix}"
lblue="${prefix}1;34${sufix}"
lcyan="${prefix}1;36${sufix}"
nocolor="${prefix}0${sufix}"

################ customizations ###############
# enable vim mode in bash 
set -o vi
EDITOR=vim





################# aliases ######################
# navigation 
alias ..='cd ..'
alias ...='cd ../../'
alias ....='cd ../../../'
alias ex='xdg-open'

# python 
alias py='python'
alias venv='python -m venv'
alias pyd='source ~/.python-envs/default/bin/activate'
alias pyls='ls -l ~/.python-envs/'

# windows style 
alias cls='clear'


function print_aliases(){

# print aliases 
echo -e "${cyan}Navigation${nocolor}"
echo -e "${lgreen}\tex\t->\t${white}open file manager${nocolor}"
echo -e "${lgreen}\t..\t->\t${white}cd ..${nocolor}"
echo -e "${lgreen}\t...\t->\t${white}cd ../..${nocolor}"
echo -e "${lgreen}\t....\t->\t${white}cd ../../..${nocolor}"

echo -e "${cyan}python${nocolor}"
echo -e "${lgreen}\tpy\t->\t${white}python${nocolor}"
echo -e "${lgreen}\tvenv\t->\t${white}python -m venv${nocolor}"
echo -e "${lgreen}\tpyd\t->\t${white}activate python default environment${nocolor}"
echo -e "${lgreen}\tpyls\t->\t${white}list environment varaibles${nocolor}"

echo -e "${cyan}windows-style${nocolor}"
echo -e "${lgreen}\tcls\t->\t${white}clear${nocolor}"
}

################# maven ######################
function create_maven_project(){
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
        *)
            echo "template does not exist"
        ;;
    esac
    
}


################# help ######################
function my_help(){
    
    # print header 
    echo -e '\n'
    echo -e "${yellow}===========================================${nocolor}"
    echo -e "${yellow}.......my bash configuration...............${nocolor}"
    echo -e "${yellow}===========================================${nocolor}"
    echo -e '\n'

    echo -e "${cyan}Options: "
    echo -e "\t${lgreen}-h, --help\t\t${nocolor}help"
    echo -e "\t${lgreen}-r, --reload\t\t${nocolor}reload .bashrc"
    echo -e "\t${lgreen}-a, --alias\t\t${nocolor}aliases"
    echo -e '\n'

    echo -e "${cyan}Subcommands: "
    echo -e "${yellow}mvn, maven\t\t${nocolor}help"
    echo -e "\t${lgreen}-q, --quick\t\t${nocolor}create maven project using maven-archetype-quickstart"
}

################# my ######################
function my(){

    case $1 in
    ## options 
        -h | --help) 
            my_help
            ;;
        -a | --alias) 
            print_aliases
            ;;
        -r | --reload) 
            echo "reload .bashrc file ..."
            source ~/.bashrc
            ;;
        -e | --edit)
            code ~/new-git/configurations/bash/.my_bash_config.sh
            ;;
    ## subcommands
        mvn | maven)
            create_maven_project $2
            ;;
    ## other cases 
        *) 
            echo "command not found"
            echo "help >> my -h or my --help"
            ;;
    esac
    
}
