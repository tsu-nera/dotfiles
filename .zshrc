###############
# zsh Setting 
###############

###############
# ヒストリ関連
###############
# 履歴の保存先
HISTFILE=$HOME/.zsh-history
## メモリに展開する履歴の数
HISTSIZE=1000
## 保存する履歴の数
SAVEHIST=10000

## コマンドラインの先頭がスペースで始まる場合ヒストリに追加しない
setopt hist_ignore_space
## history (fc -l) コマンドをヒストリリストから取り除く。
setopt hist_no_store
## 直前と同じコマンドをヒストリに追加しない
setopt hist_ignore_dups
## ヒストリを呼び出してから実行する間に一旦編集
setopt hist_verify
## zsh の開始, 終了時刻をヒストリファイルに書き込む
setopt extended_history
## 余分な空白は詰めて記録
setopt hist_reduce_blanks  
## 古いコマンドと同じものは無視 
setopt hist_save_no_dups
## ヒストリに追加されるコマンド行が古いものと同じなら古いものを削除
setopt hist_ignore_all_dups
## 補完時にヒストリを自動的に展開         
setopt hist_expand

## Screenでのコマンド共有用
## シェルを横断して.zshhistoryに記録
setopt inc_append_history
## ヒストリを共有
setopt share_history

###################
# ディレクトリ変更
###################
## ディレクトリ名だけで cd
setopt auto_cd
## cd 時に自動で push
setopt auto_pushd
## 同じディレクトリを pushd しない
setopt pushd_ignore_dups

############
# 補間関連
############
## 補完機能の有効
autoload -U compinit
compinit

## TAB で順に補完候補を切り替える
setopt auto_menu
## サスペンド中のプロセスと同じコマンド名を実行した場合はリジューム
setopt auto_resume
## 補完候補を一覧表示
setopt auto_list
## カッコの対応などを自動的に補完
setopt auto_param_keys
## ディレクトリ名の補完で末尾の / を自動的に付加し、次の補完に備える
setopt auto_param_slash
## 補完候補のカーソル選択を有効に
zstyle ':completion:*:default' menu select=1
## 補完候補の色づけ
eval `dircolors`
export ZLS_COLORS=$LS_COLORS
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
## 補完候補一覧でファイルの種別をマーク表示
setopt list_types
## 補完候補を詰めて表示
setopt list_packed

#########
# グロブ
#########
## 拡張グロブを使用
## ファイル名で #, ~, ^ の 3 文字を正規表現として扱う
setopt extended_glob
## =command を command のパス名に展開する
setopt equals
## --prefix=/usr などの = 以降も補完
setopt magic_equal_subst
## ファイル名の展開で辞書順ではなく数値的にソート
setopt numeric_glob_sort

##########
# 入出力
##########
## 出力時8ビットを通す
setopt print_eight_bit
## スペルチェック。間違うと訂正してくれる
setopt correct
## 出力の文字列末尾に改行コードが無い場合でも表示
unsetopt promptcr
## {a-c} を a b c に展開する機能を使えるようにする
setopt brace_ccl
## Ctrl+S/Ctrl+Q によるフロー制御を使わないようにする
setopt NO_flow_control
## コマンドラインでも # 以降をコメントと見なす
setopt interactive_comments
## ファイル名の展開でディレクトリにマッチした場合末尾に / を付加する
setopt mark_dirs
## 最後のスラッシュを自動的に削除しない
setopt noautoremoveslash

###############
# キーバインド
###############
## Emacsライクキーバインド設定
bindkey -e

##################
# プロンプト関連
##################
# 色有効
autoload -U colors
colors
## 色を使う
setopt prompt_subst

# 色を定義
local GREEN=$'%{\e[1;32m%}'
local BLUE=$'%{\e[1;34m%}'
local DEFAULT=$'%{\e[1;m%}'

# 通常のプロンプト
#PROMPT=$BLUE'[%n@%m] %(!.#.$) '$WHITE
PROMPT=$BLUE'[%n]%# '$WHITE
# 右側のプロンプト。ここでカレントディレクトリを出す。

if [ "$EMACS" ];then
    # Emacs の ansi-term では右プロンプトを表示しない
    RPROMPT=""
else
    RPROMPT=$DEFAULT'[%~]'$WHITE
    setopt transient_rprompt
fi
##############
# ジョブ制御
##############
## 内部コマンド jobs の出力をデフォルトで jobs -l にする
setopt long_list_jobs


#################
# その他・未分類
#################
## コアダンプサイズを制限
# limit coredumpsize 102400

## ビープを鳴らさない
setopt nobeep

## zawを読み込む
## git://github.com/zsh-users/zaw.git
# source $HOME/dotfiles/.zsh/zaw/zaw.zsh
if [ -d ${HOME}/.zsh/zaw/zaw.zsh ] ; then
    source $HOME/dotfiles/.zsh/zaw/zaw.zsh
fi

#########
# Alias 
#########
alias ls='ls -F --show-control-chars --color=aut'
alias ll='ls -ltr'
alias la='ls -a'
alias lal='ls -al'

alias r='ruby'
alias l='less'
alias t='task'
alias o='xdg-open'
alias lock='gnome-screensaver-command --lock'
alias forced_git_local_destroy='git fetch origin;git reset --hard origin/master'

#alias cd=cdls
alias cdp='cd ../'
alias cdpp='cd ../../'

# short function
alias enable_bashrc="source ~/.bashrc"
alias enable_zshrc="source ~/.bashrc"

# screen実行中にemacsで保存が効かない場合の対策
stty ixany
stty ixoff -ixon

#######################
# グローバルエイリアス
#######################
alias -g L='| less'
alias -g H='| head'
alias -g T='| tail'
alias -g G='| grep'
alias -g GI='| grep -i'
alias -g P='| percol'
alias -g PM='| percol --match-method migemo'

###############
# Environment 
###############
export LANG=ja_JP.UTF-8
export LD_LIBRARY_PATH=/usr/local/lib
export LIBRARY_PATH=/usr/local/lib
export CPATH=/usr/local/include

# rvenv設定 .rbenvがあるときだけ設定させる
# http://mukaer.com/archives/2012/03/12/rubyrbenv/
# rbenv
if [ -d ${HOME}/.rbenv  ] ; then
  export PATH="${HOME}/.rbenv/bin:${HOME}/.rbenv/shims:${PATH}"
  eval "$(rbenv init - zsh)"
fi

# z ディレクトリ移動を簡単に
# https://github.com/rupa/z
# if [ -d ${HOME}/bin/z  ] ; then
#     source ${HOME}/bin/z/z.sh
#     function precmd () {
# 	z --add "$(pwd -P)"
#     }
# fi

# ------------------------------------------------------------------------
# Functions
# ------------------------------------------------------------------------
# cd して ls する
function cdls() {
    # cdがaliasでループするので\をつける
    \cd $1;
    ls;
}

# ------------------------------------------------------------------------
# Name     : isemacs
# History  : 2014/04/20
# Function : emacsか判定
# Refs     :
# http://sakito.jp/emacs/emacsshell.html#term
# ------------------------------------------------------------------------
function isemacs(){
   [[ "$EMACS" != "" ]] && return 0
   echo "emacs"
   return 1
}

# ------------------------------------------------------------------------
# Name     : percol
# History  : 2014/01/25
# Install  : pip install percol
# Function : consoleで anythingライクなインタフェースを提供する
# ------------------------------------------------------------------------
if [ -d ${HOME}/.zsh  ] ; then
    source ~/.zsh/percol.zsh
    bindkey '^[x' percol-M-x
fi

# ------------------------------------------------------------------------
# Name     : show_buffer_stack()
# History  : 2014/04/20
# Function : コマンドラインスタック
# Refs:
# http://d.hatena.ne.jp/kei_q/20110308/1299594629
# http://qiita.com/items/1f2c7793944b1f6cc346
# ------------------------------------------------------------------------
show_buffer_stack() {
    POSTDISPLAY="
stack: $LBUFFER"
    zle push-line-or-edit
}
zle -N show_buffer_stack
setopt noflowcontrol
bindkey '^Q' show_buffer_stack  # Ctrl + q

# クリップボードコピー
xsel-buffer(){
    print -rn $BUFFER | xsel -i
    zle -M "xsel -i ${BUFFER}"
}

zle -N xsel-buffer
bindkey '^x^p' xsel-buffer # C-x C-p

# Template
# ------------------------------------------------------------------------
# Name     : tmux 
# Function : tmuxをログイン時に自動起動
# http://yonchu.hatenablog.com/entry/20120514/1337026014
# ------------------------------------------------------------------------
if [ -z "$TMUX" -a -z "$STY" ]; then
    if type tmux >/dev/null 2>&1; then
	tmux
    elif type tmux >/dev/null 2>&1; then
	if tmux has-session && tmux list-sessions | /usr/bin/grep -qE '.*]$'; then
	    tmux attach && echo "tmux attached session "
	else
	    tmux new-session && echo "tmux created new session"
	fi
    elif type screen >/dev/null 2>&1; then
	screen -rx || screen -D -RR
    fi
fi

# ------------------------------------------------------------------------
# For Emacs
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# Name     : dired
# History  : 2014/04/20
# Function : コマンドラインからEmacs Dired
# Refs:
## http://masutaka.net/chalow/2011-09-28-1.html
# ------------------------------------------------------------------------
#########
# Emacs
#########
# EmacsClient起動
if [ "$EMACS" ];then
else
    alias m='emacsclient -nw'
    alias kill_emacs="emacsclient -e \"(kill-emacs)\""
    alias boot_emacs="emacs --daemon"
    alias reboot_emacs="emacsclient -e \"(kill-emacs)\";emacs --daemon"

    export TERM=xterm-256color
    export EDITOR='emacsclient -nw'
    export VISUAL="emacsclient"
    # zsh起動時にemacs --daemon起動
    # この機能は封印 cygwin上だとemacsの起動時間が遅いので。
    # if pgrep emacs >/dev/null 2>&1; then
    #     # echo "Emacs server is already running..."
    #   else
    #     `emacs --daemon`
    # fi
fi

# screen実行中にEmacs保存ができない
# stty ixany
# stty ixoff -ixon
# screen -xR
# export LANG=ja_JP.utf8

## Invoke the ``dired'' of current working directory in Emacs buffer.
function dired () {
emacsclient -e "(dired \"${1:a}\")"
}
 
# ## Chdir to the ``default-directory'' of currently opened in Emacs buffer.
function cde () {
EMACS_CWD=`emacsclient -e "
(expand-file-name
(with-current-buffer
(if (featurep 'elscreen)
(let* ((frame-confs (elscreen-get-frame-confs (selected-frame)))
(num (nth 1 (assoc 'screen-history frame-confs)))
(cur-window-conf (cadr (assoc num (assoc 'screen-property frame-confs))))
(marker (nth 2 cur-window-conf)))
(marker-buffer marker))
(nth 1
(assoc 'buffer-list
(nth 1 (nth 1 (current-frame-configuration))))))
default-directory))" | sed 's/^"\(.*\)"$/\1/'`
 
echo "chdir to $EMACS_CWD"
cd "$EMACS_CWD"
}
