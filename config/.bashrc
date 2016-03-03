
PS1='\n${debian_chroot:+($debian_chroot)}\[\033[01;35m\]\u@\h\[\033[00m\]:\[\033[01;35m\]\W\[\033[00m\]\$ '

# alias settab='echo -n "]1; "\!*""'
# settab $cwd:h:t/$cwd:t

#PS1="\n%{\033[0;36m%}%U%n@%m[%h]%u%{\033[0;37m%} "

#export ROS_PARALLEL_JOBS=-jn
export IGNOREEOF=2

stty erase 
