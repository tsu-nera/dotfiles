# CachyOS 固有設定（Arch Linux + CachyOS の場合のみ読む）
if test -f /usr/share/cachyos-fish-config/cachyos-config.fish
    source /usr/share/cachyos-fish-config/cachyos-config.fish
end

# asdf version manager (go版 0.16+: shims を PATH に通す)
if test -d $HOME/.asdf/shims
    fish_add_path $HOME/.asdf/shims
end

source ~/.config/fish/aliases.fish
source ~/.config/fish/env.fish

set ALIASES_PRIVATE_FILE ~/.config/fish/aliases_private.fish
if test -f $ALIASES_PRIVATE_FILE
    source $ALIASES_PRIVATE_FILE
end
