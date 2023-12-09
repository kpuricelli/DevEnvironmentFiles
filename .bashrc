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

# kptodo
# Work in progress but i tend to avoid hardcoding strings when possible
export REPO_DEVENV=DevEnvironmentFiles
export REPO_STOCKS=StockDataRetriever
export REPO_PARKOUR=Parkour

#==============================================================================
# Env setup
#==============================================================================

# kptodo don't love having to add a new entry manually for each new repo
# Assert env var 'dev4' set to something valid; default to dev env otherwise
if [[ ("$dev4" == "" || \
           ("$dev4" != "$REPO_DEVENV" && "$dev4" != "$REPO_STOCKS") \
               && "$dev4" != "$REPO_PARKOUR") ]]
then
    echo ""
    echo "Environment variable 'dev4' not set, defaulting to $REPO_DEVENV"
    export dev4=$REPO_DEVENV
elif [ "$dev4" == "$REPO_PARKOUR" ]
then
    export dev4=$REPO_PARKOUR
elif [ "$dev4" == "$REPO_STOCKS" ]
then
    export dev4=$REPO_STOCKS
fi

#
# setdevenv.sh requires $dev4 to be set so we know which env vars to export,
# which parts of the code we want to skip building, 
# and creates aliases for operating in the current tree
#
# These codebases arent big enough yet for updating this to be worthwhile
# source ~/.setdevenv.sh
#
cd $WORK/$dev4
echo
pwd
echo

#==============================================================================
# Common aliases
#==============================================================================
alias sbrc='source ~/.bashrc'
alias envde='export dev4=$REPO_DEVENV && sbrc && cds'
alias envstx='export dev4=$REPO_STOCKS && sbrc && cds'
alias envpar='export dev4=$REPO_PARKOUR && sbrc && cds'

# Calls function which takes the filename and slaps & at the end
alias e='emacsRetControl'

#
# kpnotes on find alises
#
# An alias for find which will hide any "Permission denied" error messages
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

# kptodo unnecessary at the moment
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

# Most common things
alias ge="gr --color=auto -I $CS_GREP_EXCLUDES \
--include=*.{[CHch],cpp,hpp,cxx,hxx,cc,java,ts,js,html}"
alias gie="gir --color=auto -I $CS_GREP_EXCLUDES \
--include=*.{[CHch],cpp,hpp,cxx,hxx,cc,java,ts,js,html}"

# Header files
alias ghr="gr -I $CS_GREP_EXCLUDES --include=*.{H,h,hpp,hxx}"
alias gih="gir -I $CS_GREP_EXCLUDES --include=*.{H,h,hpp,hxx}"

# Source files
alias gs="gr -I $CS_GREP_EXCLUDES --include=*.{C,c,cpp,cxx,cc}"
alias gis="gir -I $CS_GREP_EXCLUDES --include=*.{C,c,cpp,cxx,cc}"

# Makefiles
alias gm="gr -I $CS_GREP_EXCLUDES --include=Makefile*"
alias gim="g -I $CS_GREP_EXCLUDES --include=Makefile*"

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
cd $WORK/$REPO_STOCKS && echo Running: git pull $REPO_STOCKS && sp && echo && \
cd $WORK/$REPO_PARKOUR && echo Running: git pull $REPO_PARKOUR && sp && \
echo && cd $WORK && echo Done updating all trees! && echo'

# Make ('lscpu' reports 4 cpus; hyperthreading disabled in host bios)
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

# kptodo - make un-specific to stock repo
alias cdsrc='cds && cd src'
alias cdt='cds && cd tests'
alias cdb='cds && cd bin/Linux.x86_64'
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
# Function to hide all 'Permission denied' error messages from find,
# provide the option to find using case sensitive or case insensitive matching,
# and optionally redirect the output of find to $HOME/out.txt
#
# Note: always skips the build direcories ('Linux.x86_64/*.d' and *.o matches)
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
