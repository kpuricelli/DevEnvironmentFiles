# .bashrc

# Don't run the bashrc for non interactive commands (like scp)
if [ -z "$PS1" ]; then
    return
fi

# Quiet pls
xset b off

#==============================================================================
# kptodo-s
#==============================================================================
#
# Function to update all local trees from the repo (and (optionally) build them)
# svn update all trees part done, would it actually be useful to make all?
#
# Change the "bash-n.n$" text to something more useful, maybe $dev4
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
export PATH=$PATH:/home/puricelli/bin
export WORK=/home/puricelli/work/
export LD_LIBRARY_PATH=/opt/public/lib:/usr/dt/lib:/usr/lib64:/usr/lib/
export MANPATH=/usr/share/man:/opt/public/man:/usr/man:/usr/X11R6/man

export PLATFORM=`uname -m`
export BUILD_VER=Linux.$PLATFORM

#==============================================================================
# Env setup
#==============================================================================

# Assert env var 'dev4' set to something valid; default 2 stoxxx otherwise
if [[ ("$dev4" == "" || ("$dev4" != "stoxxx" && "$dev4" != "Parkour")) ]]
then
    echo ""
    echo "Environment variable 'dev4' not set, defaulting to stoxxx"
    export dev4=stoxxx
elif [ "$dev4" == "Parkour" ]
then
    export dev4=Parkour
fi

# setdevenv.sh requires $dev4 to be set so we know which env vars to export,
# which parts of the code we want to skip building, 
# and creates aliases for operating in the current tree
#
# This codebase isn't big enough for updating this to be worthwhile
# source ~/.setdevenv.sh
cd $WORK/$dev4
echo
pwd
echo

#==============================================================================
# Common aliases
#==============================================================================
alias sbrc='source ~/.bashrc'
alias envstx='export dev4=stoxxx && sbrc && cds'
alias envpar='export dev4=Parkour && sbrc && cds'

# Function which takes the filename and slaps & at the end
alias e='emacsRetControl'

# An alias for find which will hide any "Permission Denied" error messages
# If desired, 'fnf' will write the output to a file 'out.txt' in the $HOME
# directory (usually /home/uname/)
alias fn='fancyFind'
alias fin='fancyFindInsensitive'
alias fnf='fancyFindSave2File'
alias finf='fancyFindSave2FileInsensitive'

# ~Lazy~
alias wd='echo $dev4'
alias eb='e ~/.bashrc'
# alias es='e ~/.setdevenv.sh'

# Killer(s)
alias k='kill'
alias xk='xkill'

# Colors are pretty
alias ls='ls --color=auto'
alias ll='ls -l --color=auto'

# Gre~p (basic)
alias g='grep --color=auto'
alias gr='grep -r --color=auto'
alias gir='grep -ir --color=auto'

# Gre~p (w/ excludes)
CS_GREP_EXCLUDES=--exclude-dir={node_modules,build.js}

# Search most useful file extensions
alias ge="grep -r --color=auto -I $CS_GREP_EXCLUDES \
--include=*.{[CHch],cpp,hpp,cxx,hxx,cc,java,ts,js,html}"
alias gie="grep -ir --color=auto -I $CS_GREP_EXCLUDES \
--include=*.{[CHch],cpp,hpp,cxx,hxx,cc,java,ts,js,html}"

# Header files
alias gh="grep -r -I $CS_GREP_EXCLUDES --include=*.{H,h,hpp,hxx}"
alias gih="grep -ir -I $CS_GREP_EXCLUDES --include=*.{H,h,hpp,hxx}"

# Source files
alias gs="grep -r -I $CS_GREP_EXCLUDES --include=*.{C,c,cpp,cxx,cc}"
alias gis="grep -ir -I $CS_GREP_EXCLUDES --include=*.{C,c,cpp,cxx,cc}"

# Makefiles
alias gm="grep -r -I $CS_GREP_EXCLUDES --include=Makefile*"
alias gim="grep -ir -I $CS_GREP_EXCLUDES --include=Makefile*"

# *RC files
alias grc="grep -r -I $CS_GREP_EXCLUDES --include=*RC*"
alias girc="grep -ir -I $CS_GREP_EXCLUDES --include=*RC*"

# Load webcam driver (requires root)
alias loadwebcam='modprobe -v stkwebcam'

# what can i say...i use svn at work and git at home (insert shrug emoji)
# git
alias sst='git status'
alias sc='git commit'
alias sd='git diff'
alias sr='git checkout'
alias sp='git pull'

# Lazy-boi alias to update all repos
# kptodo will need to manually add any newly cloned repos here
alias spall='echo && \
cd $WORK/stoxxx && echo Running: git pull stoxxx && sp && echo && \
cd $WORK/Parkour && echo Running: git pull Parkour && sp && echo && \
cd $WORK && echo Done updating all trees! && echo'

# Make ('lscpu' reports 4 cpus; 1 thread / core on this vm)
alias m='make'
alias mb='make brief'
alias m4='make -j4'
alias mc='make clean'

# C~d
alias u='cd .. && ll'
alias uu='cd ../.. && ll'
alias cdw='cd $WORK'
alias cdn='cd $HOME/notes'

# Aliases depending on env var 'dev4'
alias cds='cd $WORK/$dev4'

# kptodo for when i eventually move everything in a build directory
# alias cdb='cd /home/puricelli/$dev4/csl/bin/Linux.x86_64'

#==============================================================================
# Print the emacs keybinds to the console
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
# Start emacs and add '&' at the end
#==============================================================================
emacsRetControl()
{
    emacs $1 &
}

#==============================================================================
# Function to hide all 'Permission denied' error messages from find
#==============================================================================
fancyFind()
{
    find . -name $1 2>&1 | grep -iv "permission denied"
}

#==============================================================================
# Function to hide all 'Permission denied' error messages from find
# This is the case-insensitive version
#==============================================================================
fancyFindInsensitive()
{
    find . -iname $1 2>&1 | grep -iv "permission denied"
}

#==============================================================================
# Function to hide all 'Permission denied' error messages from find
# This will save all of the output to a file at $HOME/out.txt
#==============================================================================
fancyFindSave2File()
{
    find . -name $1 2>&1 | grep -iv "permission denied" > $HOME/out.txt
}

#==============================================================================
# Function to hide all 'Permission denied' error messages from find
# This is the case-insensitive version
# This will save all of the output to a file at $HOME/out.txt
#==============================================================================
fancyFindSave2FileInsensitive()
{
    find . -iname $1 2>&1 | grep -iv "permission denied" > $HOME/out.txt
}
