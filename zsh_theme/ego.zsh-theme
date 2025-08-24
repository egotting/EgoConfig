setopt PROMPT_SUBST

ll=$(last -1 -R $USER | awk 'NR==1 {print $4, $3, $5, strftime("%T"), "on", $2}')
echo "Last login: [$ll]"

RED="%F{red}"
WHITE="%f"
HOSTNAME_DISPLAY="${HOST//_/-}"
last2_dirs_dots() {
  if [[ "$PWD" == "$HOME" ]]; then
    echo "~"
  else
    local dir="${PWD/#$HOME/~}"
    local parts
    IFS='/' read -r -A parts <<< "$dir"
    local len=${#parts[@]}
    if (( len >= 2 )); then
      echo "${parts[len-2]}.${parts[len-1]}"
    else
      echo "${parts[len-1]}"
    fi
  fi
}

PROMPT="${RED}${HOSTNAME_DISPLAY}${WHITE}[\$(last2_dirs_dots)] %% "



