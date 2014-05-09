# ログイン時、.zshrcの後に一度のみ呼ばれる。
# functionの定義などを書く

# ------------------------------------------------------------------------
# Name     : auto-suggestion
# History  : 2014/01/25 add
# Install  : git clone git://github.com/tarruda/zsh-autosuggestions
# Function : フィッシュ形式でオススメコマンドを推薦してくれる
#            source .zshrcをするとエラーがでるため、
#            起動の設定は .zloginに記入
# ------------------------------------------------------------------------
if [ -d ${HOME}/.zsh/autosuggestions  ] ; then
source ~/.zsh/autosuggestions/autosuggestions.zsh
zle-line-init() {
    zle autosuggest-start
}
zle -N zle-line-init
fi
