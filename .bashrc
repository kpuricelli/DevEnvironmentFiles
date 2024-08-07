# .bashrc

# Don't run the bashrc for non interactive commands (like scp)
if [ -z "$PS1" ]; then
    return
fi

#==============================================================================
# kptodo-s
#==============================================================================
#
# Change the "bash-n.n$" text to something more useful, maybe $dev4
#
# Search kptodo there's always new ideas being added in random places
#
#==============================================================================
# kpnotes on bash globbing w/ grep
#==============================================================================
#
# ps x | grep xfce
# >>> this will show the grep command itself as an output from grep (BAD!)
#
# instead, do:
#
# ps x | grep xfce | grep -v grep
# >>> grep -v inverts the matches, so the third pipe won't match 'grep'
#
#==============================================================================
# Common exports
#==============================================================================
hostname="`hostname | sed 's/\..*//'`"
export HOST=$hostname
export HOSTNAME=$hostname
export PLATFORM=`uname -s`
export ARCH=`uname -m`
export BUILD_VER=$PLATFORM.$ARCH

# Mac-specific
if [[ "$OSTYPE" == "darwin22" ]]
then
    # kptodo is there anything needed to export into $PATH?
    export WORK=/Users/kp/work

    # OSX-specific version since osx loves absolute paths
    alias e='emacsRetControlOSX'
    
# Otherwise, assume Linux
else
    export PATH=$PATH:/home/puricelli/bin
    export WORK=/home/puricelli/work
    export LD_LIBRARY_PATH=/opt/public/lib:/usr/dt/lib:/usr/lib64:/usr/lib/

    # Calls function which takes the filename and slaps & at the end
    alias e='emacsRetControl'
fi

# Current git repos
export REPO_DEVENV=DevEnvironmentFiles
export REPO_STOCKS=StockDataRetriever
export REPO_PARKOUR=Parkour

#
# kpnote
#
# This is specific to the stocks project and used in the Makefile to determine
# which compiler flags to pass to g++
#
# The options are:
# none: CC_DEBUG := $(DebugFlags); CC_OPTIMIZE :=
# both: CC_DEBUG := $(DebugFlags); CC_OPTIMIZE := $(OptFlags)
# all : CC_DEBUG :=              ; CC_OPTIMIZE := $(OptFlags)
export STX_OPTIMIZE=none
alias stxopt='echo $STX_OPTIMIZE'
alias stxoptoff='export STX_OPTIMIZE=none'
alias stxoptmax='export STX_OPTIMIZE=all'
alias stxoptboth='export STX_OPTIMIZE=both'

#==============================================================================
# Env setup
#==============================================================================

# kptodo
# Don't love having to add a new entry manually for each new repo

# Assert env var 'dev4' set to something valid; default to dev env otherwise
if [[ ("$dev4" == "" || \
           ("$dev4" != "$REPO_DEVENV" && "$dev4" != "$REPO_STOCKS") \
               && "$dev4" != "$REPO_PARKOUR") ]]
then
    echo ""
    echo "Environment variable 'dev4' not set, defaulting to $REPO_STOCKS"
    export dev4=$REPO_STOCKS
elif [ "$dev4" == "$REPO_PARKOUR" ]
then
    export dev4=$REPO_PARKOUR
elif [ "$dev4" == "$REPO_STOCKS" ]
then
    export dev4=$REPO_STOCKS
elif [ "$dev4" == "$REPO_DEVENV" ]
then
    export dev4=$REPO_DEVENV
fi

# Print the current path
cd $WORK/$dev4
echo
pwd
echo

#==============================================================================
# 12data api keys hidden in a non-public github file
#==============================================================================
alias usekey1='export USEAPIKEY1='yes' && export USEAPIKEY2='no''
alias usekey2='export USEAPIKEY1='no' && export USEAPIKEY2='yes''
source ~/.setdevenv.sh

#==============================================================================
# Common aliases
#==============================================================================
alias sbrc='source ~/.bashrc'
alias envdev='export dev4=$REPO_DEVENV && sbrc && cds'
alias envstx='export dev4=$REPO_STOCKS && sbrc && cds'
alias envpar='export dev4=$REPO_PARKOUR && sbrc && cds'

#
# kpnotes on find alises
#
# An alias for find which will hide the "Permission denied" error messages
# when trying to find from a high level directory (like "/")
#
# The idea behind these aliases is to be able to execute the find cmd with
# different args while being able to utilize the same function (fancyFind)
#
# Note the fancyFind function will never search in any build directories
#
# 'fn' does case-sensitive matching
alias fn='\
export FIND_TYPE="find . -name " && \
export FIND_ARGS=" 2>&1 | grep -iv "permission denied"" && \
fancyFind'

# 'fin' does case-insensitive matching
alias fin='\
export FIND_TYPE="find . -iname " && \
export FIND_ARGS=" 2>&1 | grep -iv "permission denied"" && \
fancyFind'

# Similar to 'fn' - but redirects output to $HOME/out.txt
alias fnf='\
export FIND_TYPE="find . -name " && \
export FIND_ARGS=" 2>&1 | grep -iv "permission denied"" && \
export FIND_REDIRECT=" > $HOME/out.txt" && \
fancyFind'

# Similar to 'fin' - but redirects output to $HOME/out.txt
alias finf='\
export FIND_TYPE="find . -iname " && \
export FIND_ARGS=" 2>&1 | grep -iv "permission denied"" && \
export FIND_REDIRECT=" > $HOME/out.txt" && \
fancyFind'

# ~Lazy~
alias wd='echo $dev4'
alias eb='e ~/.bashrc'
alias ee='e ~/.emacs'
alias ys='yum search'
alias yi='yum install'
alias yu='yum update'

# Since this is under version control now, i can't be bothered to move it
# from the repo to ~
alias cpbrc='cp $WORK/DevEnvironmentFiles/.bashrc ~'

# Killer(s)
alias k='kill'
alias k9='kill -9'
alias xk='xkill'

# Colors are pretty
alias ls='ls --color=auto'
alias ll='ls -l --color=auto'

# Gre~p (basic)
alias g='grep --color=auto'
alias gr='grep -r --color=auto'
alias gir='grep -ir --color=auto'

# Gre~p never search inside node or js stuff
CS_GREP_EXCLUDES=--exclude-dir={node_modules,build.js}

# Most common things
alias ge="gr --color=auto -I $CS_GREP_EXCLUDES \
--include=*.{[CHch],cpp,hpp,cxx,hxx,cc,java,ts,js,html}"
alias gie="gir --color=auto -I $CS_GREP_EXCLUDES \
--include=*.{[CHch],cpp,hpp,cxx,hxx,cc,java,ts,js,html}"

# Header files
alias ghr="gr -I $CS_GREP_EXCLUDES --include=*.{H,h,hpp,hxx}"
alias gih="gir -I $CS_GREP_EXCLUDES --include=*.{H,h,hpp,hxx}"

# Source files
alias gsrc="gr -I $CS_GREP_EXCLUDES --include=*.{C,c,cpp,cxx,cc}"
alias gis="gir -I $CS_GREP_EXCLUDES --include=*.{C,c,cpp,cxx,cc}"

# Makefiles
alias gm="gr -I $CS_GREP_EXCLUDES --include=Makefile*"
alias gim="g -I $CS_GREP_EXCLUDES --include=Makefile*"

# git
alias ga='git add'
alias gs='git status'
alias gc='git commit'
alias gd='git diff'
alias gc='git checkout'
alias gp='git pull'

# Lazy-boi alias to update all repos
alias spall='echo && \
cd $WORK/$REPO_STOCKS && echo Running: git pull $REPO_STOCKS && sp && echo && \
cd $WORK/$REPO_PARKOUR && echo Running: git pull $REPO_PARKOUR && sp && \
echo && cd $WORK && echo Done updating all trees! && echo'

# kptodo how many cores on the osx laptop?
# Make ('lscpu' reports 20 cpus on vsmooth; hyperthreading enabled)
alias m='make'
alias m20='make -j20'
alias mc='make clean'

# C~d
alias u='cd .. && ll'
alias uu='cd ../.. && ll'
alias cdw='cd $WORK'
alias cdn='cd $HOME/notes'

# Aliases depending on env var 'dev4'
alias cds='cd $WORK/$dev4'
alias cdsrc='cds && cd src'
alias cdt='cds && cd tests'

# kptodo might be nice to have this alias figure out where the current dir is,
# as to run the target from the current directory
alias cdb='cds && cd bin/$BUILD_VER'
alias rt='cdb && ./RunTests'
alias rp='cdb && ./StockDataRetriever'

#==============================================================================
# Print some emacs keybinds to the console
#==============================================================================
alias ek='echo && \
echo Custom emacs keybinds... && \
echo Open line above: ctrl + shift + return && \
echo goto-line: C-f && \
echo query-replace: C-x + C-r && \
echo ispell-word: M-s && \
echo Insert std::cout: M-c && \
echo Copy current line: C-j && \
echo Copy word under cursor: C-o && echo'

#==============================================================================
# Bash Functions
#==============================================================================

#==============================================================================
# Kill process with given name
# Usage: 'smash <appName>'
#==============================================================================
smash()
{
    local T_PROC=$1
    local T_PIDS=($(pgrep "$T_PROC"))
    if [[ "${#T_PIDS[@]}" -ge 1 ]]; then
        echo "Found the following processes:"
        for pid in "${T_PIDS[@]}"; do
            echo "$pid" "$(ps -p "$pid" -o comm= | awk -F'/' '{print $NF}')" |
                column -t
        done
        if (yorn.ask "rly?"); then
            for pid in "${T_PIDS[@]}"; do
                echo "Killing ${pid}..."
                (kill -15 "$pid") && continue
                sleep 2
                (kill -2 "$pid") && continue
                sleep 2
                (kill -1 "$pid") && continue
                echo "Unable to find pid:" >&2 && return 1
            done
        else
            echo "......;(."
            return 0
        fi
    else
        echo "No processes found for: $1" >&2 && return 1
    fi
}

#==============================================================================
# Enforce a y/n response before killing a process
#==============================================================================
yorn.ask()
{
    read -p "$@ [Y/n]: " RESP &&
        local YORN_RESP="$(echo "${RESP:0:1}" | grep -i "[YN]")"
    while [[ -z "$YORN_RESP" ]]; do
        echo "Please respond only with: y or n"
        read -p "$@ [Y/n]: " RESP &&
            local YORN_RESP="$(echo "${RESP:0:1}" | grep -i "[YN]")"
    done
    [[ "$YORN_RESP" == 'Y' || "$YORN_RESP" == 'y' ]] && return 0 || return 1
}

#==============================================================================
# Start emacs and return control to the shell
#==============================================================================
emacsRetControl()
{
    emacs $1 &
}

#==============================================================================
# Start emacs and return control to the shell
# OSX needs the full path
#==============================================================================
emacsRetControlOSX()
{
    /Applications/Emacs.app/Contents/MacOS/Emacs $1 &
}

#==============================================================================
# kptodo make this platform independent
#
# Function to hide all 'Permission denied' error messages from find,
# provide the option to find using case sensitive or case insensitive matching,
# and optionally redirect the output of find to $HOME/out.txt
#
# Note: always skips the build directories ('Linux.x86_64/*.d' and *.o matches)
#==============================================================================
fancyFind()
{
    echo
    echo "Running command:"
    echo "$FIND_TYPE" $1 "$FIND_ARGS" "$FIND_REDIRECT"'| grep -iv Linux'
    echo
    
    eval "$FIND_TYPE" $1 "$FIND_ARGS" "$FIND_REDIRECT" | grep -iv Linux

    # Reset so we don't try and redirect the output when using 'fn' or 'fin'
    export FIND_REDIRECT=
}
