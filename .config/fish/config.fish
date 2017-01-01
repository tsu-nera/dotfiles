# ###############
# # fish config
# ###############
# alias & function 読み込み
. ~/.config/fish/aliases.fish

# 環境変数読み込み
if status --is-login
    . ~/.config/fish/env.fish
end

# rbenv
rbenv init - | source
