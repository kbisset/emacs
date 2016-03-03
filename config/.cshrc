#echo "In .cshrc.local shell is $SHELL"
#echo -n "Vars "
#setenv HOST `/bin/hostname`
# MacPorts setting on 2008-10-22 at 12:39:59: adding an appropriate PATH variable for use with MacPorts.
setenv PATH /opt/local/var/macports/software/emacs-app-devel/20090104_0/Applications/MacPorts/Emacs.app/Contents/MacOS/bin/:/opt/local/bin:/opt/local/sbin:$PATH
# Finished adapting your PATH environment variable for use with MacPorts.

# MacPorts setting on 2008-10-22 at 12:39:59: adding an appropriate MANPATH variable for use with MacPorts.
setenv /opt/local/share/man
# Finished adapting your MANPATH environment variable for use with MacPorts.

#source /sw/bin/init.csh

setenv EDITOR emacs
setenv PAGER less
setenv LESS "-R-X-P--Less-- ?f%f:stdin. ?pB(%pB\%):."
unsetenv LESSOPEN
setenv CVSROOT /usr/local/simsci/cvsroot
setenv svnroot http://svn.vbi.vt.edu/svn/simfrastructure
setenv svntran http://svn.vbi.vt.edu/svn/transims
setenv svnepi http://svn.vbi.vt.edu/svn/episims

setenv OS_VERSION `/usr/bin/defaults read "$3/System/Library/CoreServices/SystemVersion" ProductVersion`

#Oracle
setenv DYLD_LIBRARY_PATH "/opt/local/oracle/instantclient_10_2"
setenv SQLPATH "/opt/local/oracle/instantclient_10_2"
setenv TNS_ADMIN "/opt/local/oracle/network/admin"
#setenv NLS_LANG="AMERICAN_AMERICA.UTF8"
setenv PATH $PATH{}:$DYLD_LIBRARY_PATH

if (${?DISPLAY} == 0) then 
    if (${?REMOTEHOST} == 0) then
        setenv DISPLAY :0
    endif
endif
#setenv GZIP "-best -v"

#setenv CVSROOT /home/PRG/teller/Parallel.Simulation/cvsroot
#echo -n "Path "
set path=(\
    /home/simsci1/share/bin \
    /home/simsci1/share/install/mpich2/bin \
    ~/bin \
    ~/git/bin \
    ~/software/emacs/bin \
    /opt/local/bin /opt/local/sbin \
   $path \
  )

umask 002
limit coredumpsize 0
#limit coredumpsize unlimited
unset autologout
unset ignoreeof
set autolist = ambiguous
#set complete = enhance

set filec
set history = 1000
set histdup = prev
set savehist

set printexitvalue
set notify

#set prompt = "`whoami`@%m[%h] "
if ($?prompt) set prompt="\n%{\033[0;36m%}%U%n@%m[%h]%u%{\033[0;37m%} "

unalias e
unalias cd 
unalias dir 
alias more less
alias cl clear
alias ls ls -F
#alias duh "perl -e\'%h=map{/.\\s/;99**(ord\$&&7)-\$\`,\$_}\`du -h\`;die@h\{sort%h\}\'"

if ( ${?host} == 0 ) setenv HOST `hostname -s`
setenv USER `whoami`
if ( ${?term} != 0 ) then
    # tab window
    alias cwdcmd 'printf "]1;%s]2;%s" "$cwd:t" "$HOST `echo $cwd | sed s-$HOME-~-`" '
    # Set title/tab color
#    echo -n ']6;1;bg;red;brightness;150'
    alias settermr "echo -n ']6;1;bg;red;brightness;\!*'"
    alias settermg "echo -n ']6;1;bg;green;brightness;\!*'"
    alias settermb "echo -n ']6;1;bg;blue;brightness;\!*'"
    alias settabcolor "settermr 173;settermg 255;settermb 255"
    alias precmd settabcolor
    # Set title text
    alias settitle 'echo -n "]2; "\!*""'
    settitle `hostname -s`
    # Set tab text
    alias settab 'echo -n "]1; "\!*""'
    settab $cwd:h:t/$cwd:t
    cwdcmd
#alias cwdcmd 'echo -ne "\\033]0;foo${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\\007"'
#    if ( $term == "rxvt" ) then
#      if ( -f /vmunix ) then
#        alias cd '\!\! ;echo -n "]1;$host $cwd:t]2;$host $cwd:t"'
#      else
#        alias cd '\!\! ;printf "]1;%s]2;%s" "$host $cwd:t" "$host $cwd:t" '
#      endif
#      cd 
#    endif
endif

##
# Your previous /Users/kbisset/.cshrc file was backed up as /Users/kbisset/.cshrc.macports-saved_2012-06-12_at_09:43:36
##

# MacPorts Installer addition on 2012-06-12_at_09:43:36: adding an appropriate PATH variable for use with MacPorts.
setenv PATH /opt/local/bin:/opt/local/sbin:$PATH
# Finished adapting your PATH environment variable for use with MacPorts.

setenv FEDORA_HOME /Users/kbisset/fedora
setenv CATALINA_HOME $FEDORA_HOME/tomcat
setenv PATH $FEDORA_HOME/server/bin:$FEDORA_HOME/client/bin:/Users/kbisset/android/android-sdk-macosx/platform-tools:$PATH

##
# Your previous /Users/kbisset/.cshrc file was backed up as /Users/kbisset/.cshrc.macports-saved_2014-08-11_at_08:52:58
##

# MacPorts Installer addition on 2014-08-11_at_08:52:58: adding an appropriate PATH variable for use with MacPorts.
setenv PATH "/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.


##
# Your previous /Users/kbisset/.cshrc file was backed up as /Users/kbisset/.cshrc.macports-saved_2014-08-11_at_11:43:03
##

# MacPorts Installer addition on 2014-08-11_at_11:43:03: adding an appropriate PATH variable for use with MacPorts.
setenv PATH "/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.

