###############
# zsh Setting 
###############

###############
# ヒストリ関連
###############
# 履歴ファイルの保存先
export HISTFILE=${HOME}/.zsh-history
# メモリに保存される履歴の件数
export HISTSIZE=1000
# 履歴ファイルに保存される履歴の件数
export SAVEHIST=100000

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
# autoload -U compinit
# compinit

# cygwinでのエラー回避
# zsh compinit: insecure directories, run compaudit for list.
# http://d.hatena.ne.jp/ywatase/20071103
autoload -Uz compinit
compinit -u

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
## 色を使う
setopt prompt_subst

# 色を定義
local GREEN=$'%{\e[1;32m%}'
local ORANGE=$'%{\e[1;33m%}'
local DEFAULT=$'%{\e[1;m%}'

if [ "$EMACS" ];then
    PROMPT='[%n]%# '
    # Emacs の shell では右プロンプトを表示しない
    RPROMPT=""
else
    PROMPT=$ORANGE'[%n]%# '$WHITE

    # 右側のプロンプト。ここでカレントディレクトリを出す。
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
if [ "$EMACS" ];then
    alias ls='ls -F --show-control-chars'
else
    alias ls='ls -F --show-control-chars --color=aut'
fi
alias ll='ls -ltr'
alias la='ls -a'
alias lal='ls -al'

alias r='ruby'
alias l='less'
alias t='task'
alias o='xdg-open'
alias forced_git_local_destroy='git fetch origin;git reset --hard origin/master'

#alias cd=cdls
alias cdp='cd ../'
alias cdpp='cd ../../'

# short function
alias enable_bashrc="source ~/.bashrc"
alias enable_zshrc="source ~/.bashrc"

if [ -x "`which docker`" ]; then
    alias doc='docker'
fi

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

if [ -d ${HOME}/script  ] ; then
  export PATH="${HOME}/script/sh:${PATH}"
  export PATH="${HOME}/script/ruby:${PATH}"
fi

# z ディレクトリ移動を簡単に
# https://github.com/rupa/z
# if [ -d ${HOME}/bin/z  ] ; then
#     source ${HOME}/bin/z/z.sh
#     function precmd () {
# 	z --add "$(pwd -P)"
#     }
# fi

# for go lang
if [ -x "`which go`" ]; then
  export GOPATH=$HOME/go
  export PATH=$PATH:$GOROOT/bin:$GOPATH/bin
fi

# for ruby
if [ -x "`which ruby`" ]; then
  export PATH=$HOME/.gem/ruby/2.1.0/bin:$PATH
fi

# ------------------------------------------------------------------------
# OSによる場合分け
# ------------------------------------------------------------------------
case $OSTYPE in
  cygwin*)
  alias executer="cygstart"
  ;;
  linux*)
  alias executer="xdg-open"
  ;;
esac

# ------------------------------------------------------------------------
# Functions
# ------------------------------------------------------------------------
# cd して ls する
function cdls() {
    # cdがaliasでループするので\をつける
    \cd $1;
    ls;
}

# history 全表示
function history-all { history -E 1 }

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
if [ -x "`which xsel`" ]; then
xsel-buffer(){
    print -rn $BUFFER | xsel -i
    zle -M "xsel -i ${BUFFER}"
}
fi

zle -N xsel-buffer
bindkey '^x^p' xsel-buffer # C-x C-p

# ------------------------------------------------------------------------
# Name     : tmux 
# Function : tmuxをログイン時に自動起動
# http://yonchu.hatenablog.com/entry/20120514/1337026014
# ------------------------------------------------------------------------
# if [ -z "$TMUX" -a -z "$STY" ]; then
#     if type tmux >/dev/null 2>&1; then
# 	tmux
#     elif type tmux >/dev/null 2>&1; then
# 	if tmux has-session && tmux list-sessions | /usr/bin/grep -qE '.*]$'; then
# 	    tmux attach && echo "tmux attached session "
# 	else
# 	    tmux new-session && echo "tmux created new session"
# 	fi
#     elif type screen >/dev/null 2>&1; then
# 	screen -rx || screen -D -RR
#     fi
# fi

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
export TERM=xterm-256color
alias mc='emacsclient -nc'

if [ "$EMACS" ];then
else
    alias m='emacsclient -nw'
    alias kill_emacs="emacsclient -e \"(kill-emacs)\""
    alias boot_emacs="emacs --daemon"
    alias boot_my_world="emacs --daemon"
    alias reboot_emacs="emacsclient -e \"(kill-emacs)\";emacs --daemon"

    export EDITOR="emacsclient -nw"
    export VISUAL="emacsclient -nw"
    # zsh起動時にemacs --daemon起動
    # この機能は封印 cygwin上だとemacsの起動時間が遅いので。
    # if pgrep emacs >/dev/null 2>&1; then
    #     # echo "Emacs server is already running..."
    #   else
    #     `emacs --daemon`
    # fi
    # いろなし
fi

## Invoke the ``dired'' of current working directory in Emacs buffer.
function dired () {
emacsclient -e "(dired \"${1:a}\")"
}
 
# ------------------------------------------------------------------------
# Name     : cde
#   http://d.hatena.ne.jp/syohex/20111026/1319606395
# ------------------------------------------------------------------------
# ## Chdir to the ``default-directory'' of currently opened in Emacs buffer.
function cde () {
    EMACS_CWD=`emacsclient -e "
      (if (featurep 'elscreen)
          (elscreen-current-directory)
        (non-elscreen-current-directory))" | sed 's/^"\(.*\)"$/\1/'`

    echo "chdir to $EMACS_CWD"
    cd "$EMACS_CWD"
}

# ------------------------------------------------------------------------
# Name     : cdr
# Function : 最近訪れたフォルダへ移動
# ------------------------------------------------------------------------
autoload -Uz is-at-least
if is-at-least 4.3.11
then
  autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
  add-zsh-hook chpwd chpwd_recent_dirs
  zstyle ':chpwd:*' recent-dirs-max 5000
  zstyle ':chpwd:*' recent-dirs-default yes
  zstyle ':completion:*' recent-dirs-insert both
fi

# ------------------------------------------------------------------------
# Name     : percol
# History  : 2014/01/25
# Install  : pip install percol
# Function : consoleで anythingライクなインタフェースを提供する
# ------------------------------------------------------------------------
# if [ -d ${HOME}/.zsh  ] ; then
#     source ~/.zsh/percol.zsh
#     bindkey '^[x' percol-M-x
# fi

# ------------------------------------------------------------------------
# Name     : peco
# Function : consoleで helmライクなインタフェースを提供する
# ------------------------------------------------------------------------
if [ -x "`which peco`" ]; then
alias -g P='| peco'
alias llp='ls -la | peco'
alias lsp='lsv| peco'
alias tp='top | peco'
alias pp='ps aux | peco'

if [ -x "`which anamnesis`" ]; then
    alias ap='anamnesis -l 200 | peco'
fi

# ------------------------------------------------------------------------
# history filter
# http://qiita.com/uchiko/items/f6b1528d7362c9310da0
# ------------------------------------------------------------------------
function peco-select-history() {
    local tac
    if which tac > /dev/null; then
        tac="tac"
    else
        tac="tail -r"
    fi
    BUFFER=$(\history -n 1 | \
        eval $tac | \
        peco --query "$LBUFFER")
    CURSOR=$#BUFFER
    zle clear-screen
}
zle -N peco-select-history

# ------------------------------------------------------------------------
# peco-recentd
# 最近訪れたディレクトリに移動
# ------------------------------------------------------------------------
function peco-recentd () {
    local selected_dir=$(cdr -l | awk '{ print $2 }' | peco)
    if [ -n "$selected_dir" ]; then
        BUFFER="cd ${selected_dir}"
        zle accept-line
    fi
    zle clear-screen
}
zle -N peco-recentd

# ------------------------------------------------------------------------
# peco-kill-process
# プロセスキル
# ------------------------------------------------------------------------
function peco-kill-process () {
    ps -ef | peco | awk '{ print $2 }' | xargs kill
    zle clear-screen
}
zle -N peco-kill-process

#------------------------------------------------------------------------
# peco-find-file
# emacsでカレントディレクトリのファイルを開く
#------------------------------------------------------------------------
function peco-find-file () {
    ls | peco | xargs emacsclient -nw
    zle clear-screen
}
zle -N peco-find-file

#------------------------------------------------------------------------
# peco-open-app
# カレントディレクトリのファイルを開く
#------------------------------------------------------------------------
function peco-open-app () {
    case $OSTYPE in
	cygwin*)
	    ls | peco | xargs cygstart
	    ;;
	linux*)
            ls | peco | xargs xdg-open
	    ;;
    esac
#    zle clear-screen
}
zle -N peco-open-app

#------------------------------------------------------------------------
# peco-ag
# agで検索して、emacsで開く
#------------------------------------------------------------------------
if [ -x "`which ag`" ]; then
function peco-ag () {
    ag $@ | peco --query "$LBUFFER" | awk -F : '{print "+" $2 " " $1}' | xargs emacsclient -nw
}
fi

#------------------------------------------------------------------------
# peco-M-x
# コマンド一覧を表示して実行
#------------------------------------------------------------------------
function peco-M-x () {
    local cmd=$(peco-M-x.rb | peco)
    eval $cmd &
}
zle -N peco-M-x

#------------------------------------------------------------------------
# peco-docker
# http://qiita.com/sonodar/items/d2de15b98581108b5de8
#------------------------------------------------------------------------
# pecoで選択したコンテナに対して操作を行う

if [ -x "`which docker`" ]; then
docker_peco_containers() {
  if [ $# -lt 1 ]; then
      echo "Usage: dpc [OPTIONS] COMMAND [args]" >&2
      return 1
  fi
  
  docker ps -a | peco | while read CONTAINER
  do
      docker $@ `echo $CONTAINER | awk '{print $1}'`
  done
}

# pecoで選択したイメージに対して操作を行う
docker_peco_images() {
    if [ $# -lt 1 ]; then
	echo "Usage: dpi [OPTIONS] COMMAND [args]" >&2
	return 1
    fi

    unset DOCKER_OPTS
    [[ -z $ENVS ]] || \
	for e in $ENVS; do DOCKER_OPTS="$DOCKER_OPTS -e $e"; done
    [[ -z $VOLUMES ]] || \
	for v in $VOLUMES; do DOCKER_OPTS="$DOCKER_OPTS -v $v"; done
    [[ -z $PORTS ]] || \
	for p in $PORTS; do DOCKER_OPTS="$DOCKER_OPTS -p $p"; done
    docker images | peco | while read IMAGE
    do
	docker $DOCKER_OPTS $@ `echo $IMAGE | awk '{print $3}'`
    done
}

# エイリアスはお好みで
alias dpc='docker_peco_containers'
alias dpi='docker_peco_images'
fi

################
# key bindings
################
bindkey '^xb'  peco-select-history # C-x b
bindkey '^[x'  peco-M-x            # M-x
bindkey '^x^r' peco-recentd        # C-x C-r
bindkey '^xk'  peco-kill-process   # C-x k
bindkey '^x^f' peco-find-file      # C-x C-f
bindkey '^xo'  peco-open-app       # C-x o
fi
