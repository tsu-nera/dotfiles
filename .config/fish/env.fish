set -x LANG ja_JP.UTF-8
set -x MANROFFOPT "-c"
set -x MANPAGER "sh -c 'col -bx | bat -l man -p'"

set -x EDITOR "emacs -nw"
set -x VISUAL "emacs -nw"

# MSYS2 (Windows) 固有: Windows 実体の開発ツールを PATH 参照
# - claude: C:\Users\fox10\.local\bin\claude.exe
# - gh:     C:\Program Files\GitHub CLI\gh.exe（認証は Windows keyring 共有）
if string match -q '*MSYS*' "$MSYSTEM"; or string match -q '*MINGW*' "$MSYSTEM"
    # MSYS2 native git (/usr/bin/git) を MINGW64 の git より優先する
    # MINGW64 環境では /mingw64/bin が先に来て GfW の git が勝ってしまうため明示的に前置
    fish_add_path --prepend /usr/bin
    # Windows 実体の開発ツール（claude, gh）を追加
    fish_add_path /c/Users/fox10/.local/bin
    fish_add_path "/c/Program Files/GitHub CLI"
    # native symlink を作るための設定（make_lnlink 等で使用）
    set -x MSYS winsymlinks:nativestrict
end
