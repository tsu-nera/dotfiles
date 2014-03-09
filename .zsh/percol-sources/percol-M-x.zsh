function percol_M-x() {
  local tac_cmd
  which gtac &> /dev/null && tac_cmd=gtac || tac_cmd=tac
  BUFFER=$($tac_cmd ~/.zsh-history | sed 's/^: [0-9]*:[0-9]*;//' \
    | percol --match-method regex --query "$LBUFFER")
  CURSOR=$#BUFFER         # move cursor
  zle -R -c               # refresh
}

zle -N percol-M-x

