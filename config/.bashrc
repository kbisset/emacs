if [ -f .bashrc.local ]; then
    source .bashrc.local
fi

host=`hostname -s`
# 39 Default foreground color
# 30 Black
# 31 Red
# 32 Green
# 33 Yellow
# 34 Blue
# 35 Magenta
# 36 Cyan
# 37 Light gray
# 90 Dark gray
# 91 Light red
# 92 Light green
# 93 Light yellow
# 94 Light blue
# 95 Light magenta
# 96 Light cyan
# 97 White

# 49 Default background color
# 40 Black
# 41 Red
# 42 Green
# 43 Yellow
# 44 Blue
# 45 Magenta
# 46 Cyan
# 47 Light gray
# 100 Dark gray
# 101 Light red
# 102 Light green
# 103 Light yellow
# 104 Light blue
# 105 Light magenta
# 106 Light cyan
# 107 White


if [ $host = "stormtrooper" ]; then
    promptColor="01;36"
elif [ $host = "vagrant-ubuntu-trusty-64" ]; then
    promptColor="01;32"
elif [ $host = "katrina" ]; then
    promptColor="01;31"
elif [ $host = "cloud" ]; then
    promptColor="01;34"
elif [ $host = "Keiths-MacBook-Pro" ]; then
    promptColor="01;35"
else
    promptColor="01;33"
fi

PS1='\n${debian_chroot:+($debian_chroot)}\[\033['$promptColor'm\]\u@\h\[\033[00m\]:\[\033['$promptColor'm\]\W\[\033[00m\]\$ '

setTabTitle() {
    echo -ne "\033]1;$*\007"   
}

setWindoeTitle() {
    echo -ne "\033]2;$*\007"   
}
alias ls="ls -F"

PS1='\n\[\033[01;35m\]\u@\h\[\033[00m\]:\[\033[01;35m\]\W\[\033[00m\]\$ '
PROMPT_COMMAND='echo -ne "\033]1;${USER}@${host}\007\033]2;${host}\007"'

#export ROS_PARALLEL_JOBS=-jn
export IGNOREEOF=2

stty erase 

