#!/bin/bash
# start ssh-agent and load CECI key




# starts ssh-agent
eval $(ssh-agent)
 
PATH1=/f/Dropbox/Bin/ceci.rsa.2020
if [ -f "$PATH1" ]; then
    echo "loading $PATH1"
    ssh-add $PATH1
fi

PATH2=~/.ssh/ceci.rsa.2020
if [ -f "$PATH2" ]; then
    echo "loading $PATH2"
    ssh-add $PATH2
fi


# ne marche pas... (devrait etre ok si "source")