# Sample .bashrc for SuSE Linux
# Copyright (c) SuSE GmbH Nuernberg

# There are 3 different types of shells in bash: the login shell, normal shell
# and interactive shell. Login shells read ~/.profile and interactive shells
# read ~/.bashrc; in our setup, /etc/profile sources ~/.bashrc - thus all
# settings made here will also take effect in a login shell.
#
# NOTE: It is recommended to make language settings in ~/.profile rather than
# here, since multilingual X sessions would not work properly if LANG is over-
# ridden in every subshell.

# Some applications read the EDITOR variable to determine your favourite text
# editor. So uncomment the line below and enter the editor of your choice :-)

export EDITOR=ec

# For some news readers it makes sense to specify the NEWSSERVER variable here

#export NEWSSERVER=your.news.server

test -s ~/.alias && . ~/.alias || true

export PYTHONUSERBASE=~/pythonbase/

export JAVA_HOME=~/opt/jdk1.8.0_101/

export M2_HOME=~/opt/apache-maven-3.3.9/

export PATH=${HOME}/bin:${JAVA_HOME}/bin:${M2_HOME}/bin:${PATH}

function find_in_cpp {
    find -name \*.cpp -exec grep -H --color=auto ${1} {} \;
}

export less="--raw-control-chars"

[[ -f ~/.less_termcap ]] && . ~/.less_termcap
