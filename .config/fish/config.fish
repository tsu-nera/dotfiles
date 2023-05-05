# ###############
# # fish config
# ###############
# alias & function 読み込み
. ~/.config/fish/aliases.fish

set ALIASES_PRIVATE_FILE ~/.config/fish/aliases_private.fish
if test -f $ALIASES_PRIVATE_FILE
    . $ALIASES_PRIVATE_FILE
end

# 環境変数読み込み
. ~/.config/fish/env.fish

# conda
# source (conda info --root)/etc/fish/conf.d/conda.fish

# Ensure fisherman and plugins are installed
if not test -f $HOME/.config/fish/functions/fisher.fish
    echo "==> Fisherman not found.  Installing."
    curl -sLo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
    fisher
end

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
eval /home/tsu-nera/miniconda3/bin/conda "shell.fish" hook $argv | source
# <<< conda initialize <<<

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/tsu-nera/google-cloud-sdk/path.fish.inc' ]
    . '/home/tsu-nera/google-cloud-sdk/path.fish.inc'
end
