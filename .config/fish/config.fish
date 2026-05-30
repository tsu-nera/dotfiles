source /usr/share/cachyos-fish-config/cachyos-config.fish

source ~/.config/fish/aliases.fish
source ~/.config/fish/env.fish

set ALIASES_PRIVATE_FILE ~/.config/fish/aliases_private.fish
if test -f $ALIASES_PRIVATE_FILE
    source $ALIASES_PRIVATE_FILE
end
