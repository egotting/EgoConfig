ll=$(last -1 -R $USER | awk 'NR==1 {print $4, $3, $5, strftime("%T"), "on", $2}')
echo "Last login: $ll"
export PS1="Last login: [$ll]"'\n\h:\W\$ '

%1

# Cores
RED="%F{red}"
WHITE="%f"
HOSTNAME_DISPLAY="${HOST//_/-}"  # substitui _ por -
PROMPT="${RED}${HOSTNAME_DISPLAY}${WHITE}[%2~] %% "
