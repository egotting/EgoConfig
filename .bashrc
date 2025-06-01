#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls -la --color=auto'
alias grep='grep --color=auto'
alias sudo='sudo -E -s'
alias repo='cd ~/Documentos/repo/github.com/egotting/'
alias isdk='asdf'
source ~/Documentos/git/ble.sh/out/ble.sh

parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

short_pwd() {
  local dir=$(pwd)
  if [[ "$dir" == "$HOME" ]]; then
    echo "~"
  elif [[ "$dir" == "$HOME/"* ]]; then
    echo "~/${dir#$HOME/}" | awk -F/ '{print $NF}'
  else
    basename "$dir"
  fi
}

export PS1="\u@\h\[\033[0;32m\]:\[\033[0m\]\$(short_pwd)\$(parse_git_branch) ~ % "

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
. "$HOME/.asdf/asdf.sh"
