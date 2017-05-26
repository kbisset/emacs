if test -f $HOME/.bashrc.local ; then
    source $HOME/.bashrc.local
fi

setTabTitle() {
    echo -ne "\033]1;$*\007"   
}

setWindoeTitle() {
    echo -ne "\033]2;$*\007"   
}

history_sync() { 
    history -a; history -c; history -r; 
}         

history_write() { 
    history -a; 
}         

shopt -s histappend 

PS1='\n\u@\h:\W\$ history_write '



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

# shell options
shopt -s checkjobs cmdhist histappend no_empty_cmd_completion nocaseglob

PS1='\n\[\033['$promptColor'm\]\u@\h\[\033[00m\]:\[\033['$promptColor'm\]\W\[\033[00m\]\[$gitclrbeg\]$git_prompt\[$gitclrend\]\$ '

LS_COLORS='rs=0:di=01;36:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.lz=01;31:*.xz=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.axv=01;35:*.anx=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.axa=00;36:*.oga=00;36:*.spx=00;36:*.xspf=00;36:';
export LS_COLORS

alias ls="ls -F"

alias git="/usr/bin/git -c user.name=Keith -c user.email=keith@light.house"
#Ps1='\n\[\033[01;35m\]\u@\h\[\033[00m\]:\[\033[01;35m\]\W\[\033[00m\]\$ '
PROMPT_COMMAND='echo -ne "\033]1;${USER}@${host}\007\033]2;${host}\007"; history_write'

#export ROS_PARALLEL_JOBS=-jn
export IGNOREEOF=2

export VISUAL="emacs -nw -u keith"
export EDITOR="$VISUAL"
export GIT_EDITOR="$VISUAL"

if [ -f ~/.git-completion.bash ]
then
	source ~/.git-completion.bash
fi

stty erase 

unset command_not_found_handle

source /home/keith/emacs/config/standalone-prompt.sh

export HISTSIZE=INFINITE
export HISTFILESIZE=10000
export HISTFILE=$HOME/.bash_history-krb
export HISTCONTROL=ignoredups:ignoredups

export ROSCONSOLE_FORMAT='[${severity}] [${time}] [${thread}]: ${message}'

export LESS="-n-R-X-P--Less-- ?f%f:stdin. ?pB(%pB\%):."

if test -f /opt/ros/indigo/setup.bash ; then
    source /opt/ros/indigo/setup.bash
fi

if test -f /opt/ros/kinetic/setup.bash ; then
    source /opt/ros/kinetic/setup.bash
fi

PATH="/home/keith/bin:$PATH"

alias psjarvis="ps uaxww | grep -e 'jarvis2|tlgen|barista'"
alias emacs="/usr/bin/emacs -u keith"
alias ls="ls -H"
