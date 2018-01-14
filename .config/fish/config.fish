# ###############
# # fish config
# ###############
# alias & function 読み込み
. ~/.config/fish/aliases.fish

# 環境変数読み込み
. ~/.config/fish/env.fish

# conda
source (conda info --root)/etc/fish/conf.d/conda.fish

# Ensure fisherman and plugins are installed
if not test -f $HOME/.config/fish/functions/fisher.fish
  echo "==> Fisherman not found.  Installing."
  curl -sLo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
  fisher
end
