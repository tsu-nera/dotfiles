# 共通設定の入口（dotfiles 管理、全マシン共通）
# 読み込み順: conf.d/*.fish → この config.fish → local.fish
# - 共通: dotfiles から symlink したファイル
# - マシン固有: ~/.config/fish/local.fish（symlink しない実ファイル、最後に読むので上書き可）

source ~/.config/fish/env.fish
source ~/.config/fish/aliases.fish

if test -f ~/.config/fish/local.fish
    source ~/.config/fish/local.fish
end
