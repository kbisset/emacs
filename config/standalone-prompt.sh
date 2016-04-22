# Source this file from your ~/.bashrc or ~/.bash_profile
# for color coded git branch information in your shell prompt.
#
# The last line ("PS1=...") should be commented out to allow
# your own customized prompt to be set in your ~/.bashrc,
# but it serves as an example of how to use the git prompting
# information and is the actual prompt that I use personally.
#
# If you find this code helpful I'd like hearing about it!
# You can email me at <mikeweilgart (at) gmail (dot) com>
#
# --Mike Weilgart

# Color codes thanks to https://github.com/jimeh/git-aware-prompt/blob/master/colors.sh
# Regular
txtblk="$(tput setaf 0 2>/dev/null || echo '\e[0;30m')"  # Black
txtred="$(tput setaf 1 2>/dev/null || echo '\e[0;31m')"  # Red
txtgrn="$(tput setaf 2 2>/dev/null || echo '\e[0;32m')"  # Green
txtylw="$(tput setaf 3 2>/dev/null || echo '\e[0;33m')"  # Yellow
txtblu="$(tput setaf 4 2>/dev/null || echo '\e[0;34m')"  # Blue
txtpur="$(tput setaf 5 2>/dev/null || echo '\e[0;35m')"  # Purple
txtcyn="$(tput setaf 6 2>/dev/null || echo '\e[0;36m')"  # Cyan
txtwht="$(tput setaf 7 2>/dev/null || echo '\e[0;37m')"  # White

# Bold
bldblk="$(tput setaf 0 2>/dev/null)$(tput bold 2>/dev/null || echo '\e[1;30m')"  # Black
bldred="$(tput setaf 1 2>/dev/null)$(tput bold 2>/dev/null || echo '\e[1;31m')"  # Red
bldgrn="$(tput setaf 2 2>/dev/null)$(tput bold 2>/dev/null || echo '\e[1;32m')"  # Green
bldylw="$(tput setaf 3 2>/dev/null)$(tput bold 2>/dev/null || echo '\e[1;33m')"  # Yellow
bldblu="$(tput setaf 4 2>/dev/null)$(tput bold 2>/dev/null || echo '\e[1;34m')"  # Blue
bldpur="$(tput setaf 5 2>/dev/null)$(tput bold 2>/dev/null || echo '\e[1;35m')"  # Purple
bldcyn="$(tput setaf 6 2>/dev/null)$(tput bold 2>/dev/null || echo '\e[1;36m')"  # Cyan
bldwht="$(tput setaf 7 2>/dev/null)$(tput bold 2>/dev/null || echo '\e[1;37m')"  # White

# Underline
undblk="$(tput setaf 0 2>/dev/null)$(tput smul 2>/dev/null || echo '\e[4;30m')"  # Black
undred="$(tput setaf 1 2>/dev/null)$(tput smul 2>/dev/null || echo '\e[4;31m')"  # Red
undgrn="$(tput setaf 2 2>/dev/null)$(tput smul 2>/dev/null || echo '\e[4;32m')"  # Green
undylw="$(tput setaf 3 2>/dev/null)$(tput smul 2>/dev/null || echo '\e[4;33m')"  # Yellow
undblu="$(tput setaf 4 2>/dev/null)$(tput smul 2>/dev/null || echo '\e[4;34m')"  # Blue
undpur="$(tput setaf 5 2>/dev/null)$(tput smul 2>/dev/null || echo '\e[4;35m')"  # Purple
undcyn="$(tput setaf 6 2>/dev/null)$(tput smul 2>/dev/null || echo '\e[4;36m')"  # Cyan
undwht="$(tput setaf 7 2>/dev/null)$(tput smul 2>/dev/null || echo '\e[4;37m')"  # White

# Background
bakblk="$(tput setab 0 2>/dev/null || echo '\e[40m')"  # Black
bakred="$(tput setab 1 2>/dev/null || echo '\e[41m')"  # Red
bakgrn="$(tput setab 2 2>/dev/null || echo '\e[42m')"  # Green
bakylw="$(tput setab 3 2>/dev/null || echo '\e[43m')"  # Yellow
bakblu="$(tput setab 4 2>/dev/null || echo '\e[44m')"  # Blue
bakpur="$(tput setab 5 2>/dev/null || echo '\e[45m')"  # Purple
bakcyn="$(tput setab 6 2>/dev/null || echo '\e[46m')"  # Cyan
bakwht="$(tput setab 7 2>/dev/null || echo '\e[47m')"  # White

# Reset
txtrst="$(tput sgr 0 2>/dev/null || echo '\e[0m')"  # Text Reset

# Code below here is original by Mike Weilgart
get_git_status_for_prompt() {
  local status
  if status="$(git status --porcelain 2>/dev/null)"; then
    # We are in a git repository
    local branch
    branch="$(git rev-parse --abbrev-ref HEAD 2>/dev/null)"
    [ "HEAD" = "$branch" ] && branch="-detached-"
    if [ "" = "$status" ]; then
      # Working directory and index fully clean, no further logic needed
      gitclrbeg="$txtcyn"
      git_prompt=" ($branch)"
      gitclrend="$txtrst"
    else
      local workdir_status index_status
      workdir_status="$(printf %s\\n "$status" | cut -c 2)"
      index_status="$(printf %s\\n "$status" | cut -c 1)"

      local abbrev_status
      if [[ "$workdir_status" =~ '?' ]]; then
        abbrev_status="1"
      else
        abbrev_status="0"
      fi

      case "$workdir_status" in
        (*[DM]*)
          abbrev_status+="1" ;;
        (*)
          abbrev_status+="0" ;;
      esac

      case "$index_status" in
        (*[MADRC]*)
          abbrev_status+="1" ;;
        (*)
          abbrev_status+="0" ;;
      esac

      case "$abbrev_status" in
        (000) # Clean - should have been caught.  Odd.
          gitclrbeg="$bldwht"
          git_prompt=" ($branch WTF)"
          gitclrend="$txtrst"
          ;;
        (100) # Untracked files otherwise clean
          gitclrbeg="$txtcyn"
          git_prompt=" ($branch)*"
          gitclrend="$txtrst"
          ;;
        (010) # Unadded tracked changes
          gitclrbeg="$txtred"
          git_prompt=" ($branch)"
          gitclrend="$txtrst"
          ;;
        (110) # Same plus untracked files
          gitclrbeg="$txtred"
          git_prompt=" ($branch)*"
          gitclrend="$txtrst"
          ;;
        (001) # Staged changes
          gitclrbeg="$txtgrn"
          git_prompt=" ($branch)"
          gitclrend="$txtrst"
          ;;
        (101) # Plus untracked files
          gitclrbeg="$txtgrn"
          git_prompt=" ($branch)*"
          gitclrend="$txtrst"
          ;;
        (011) # Unstaged and staged changes
          gitclrbeg="$txtylw"
          git_prompt=" ($branch)"
          gitclrend="$txtrst"
          ;;
        (111) # Plus untracked files
          gitclrbeg="$txtylw"
          git_prompt=" ($branch)*"
          gitclrend="$txtrst"
          ;;
        (*) # Unexpected failure
          gitclrbeg="$bldwht"
          git_prompt=" ($branch WTFH)"
          gitclrend="$txtrst"
          ;;
      esac
      case "$workdir_status$index_status" in
        (*U*)
          # Merge is in progress
          gitclrbeg="$txtpur"
          git_prompt=" ($branch - MERGING)"
          gitclrend="$txtrst"

          # This logic could be improved, as evidently a merge
          # can be in progress (as shown by 'git status')
          # without being visible in the output of 'git status porcelain'
          # at all.  So not ALL "merge in progress" will be shown in
          # purple, only some of them.
          ;;
      esac
    fi
  else
    # We are not in a git repository
    gitclrbeg=""
    git_prompt=""
    gitclrend=""
  fi
}

PROMPT_COMMAND="get_git_status_for_prompt; $PROMPT_COMMAND"

#PS1='\t [\u \W]\[$gitclrbeg\]$git_prompt\[$gitclrend\] \! \$ '
