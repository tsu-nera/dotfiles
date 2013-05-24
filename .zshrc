###############
# zsh Setting #
###############

# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

export LANG=ja_JP.UTF-8

## 履歴の保存先
HISTFILE=$HOME/.zsh-history
## メモリに展開する履歴の数
# HISTSIZE=10000
## 保存する履歴の数
SAVEHIST=10000

## 補完機能の強化
autoload -U compinit
compinit

## コアダンプサイズを制限
# limit coredumpsize 102400
## 出力の文字列末尾に改行コードが無い場合でも表示
unsetopt promptcr
## Emacsライクキーバインド設定
bindkey -e

## 色を使う
setopt prompt_subst
## ビープを鳴らさない
setopt nobeep
## 内部コマンド jobs の出力をデフォルトで jobs -l にする
setopt long_list_jobs
## 補完候補一覧でファイルの種別をマーク表示
setopt list_types
## サスペンド中のプロセスと同じコマンド名を実行した場合はリジューム
setopt auto_resume
## 補完候補を一覧表示
setopt auto_list
## 直前と同じコマンドをヒストリに追加しない
setopt hist_ignore_dups
## cd 時に自動で push
setopt auto_pushd
## 同じディレクトリを pushd しない
setopt pushd_ignore_dups
## ファイル名で #, ~, ^ の 3 文字を正規表現として扱う
setopt extended_glob
## TAB で順に補完候補を切り替える
setopt auto_menu
## zsh の開始, 終了時刻をヒストリファイルに書き込む
setopt extended_history
## =command を command のパス名に展開する
setopt equals
## --prefix=/usr などの = 以降も補完
setopt magic_equal_subst
## ヒストリを呼び出してから実行する間に一旦編集
setopt hist_verify
## ファイル名の展開で辞書順ではなく数値的にソート
setopt numeric_glob_sort
## 出力時8ビットを通す
setopt print_eight_bit
## ヒストリを共有
setopt share_history
## 補完候補のカーソル選択を有効に
zstyle ':completion:*:default' menu select=1
## 補完候補の色づけ
eval `dircolors`
export ZLS_COLORS=$LS_COLORS
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
## ディレクトリ名だけで cd
setopt auto_cd
## カッコの対応などを自動的に補完
setopt auto_param_keys
## ディレクトリ名の補完で末尾の / を自動的に付加し、次の補完に備える
setopt auto_param_slash
## スペルチェック
setopt correct
## {a-c} を a b c に展開する機能を使えるようにする
setopt brace_ccl
## Ctrl+S/Ctrl+Q によるフロー制御を使わないようにする
setopt NO_flow_control
## コマンドラインの先頭がスペースで始まる場合ヒストリに追加しない
setopt hist_ignore_space
## コマンドラインでも # 以降をコメントと見なす
setopt interactive_comments
## ファイル名の展開でディレクトリにマッチした場合末尾に / を付加する
setopt mark_dirs
## history (fc -l) コマンドをヒストリリストから取り除く。
setopt hist_no_store
## 補完候補を詰めて表示
setopt list_packed
## 最後のスラッシュを自動的に削除しない
setopt noautoremoveslash

#########
# Alias #
#########
alias ls='ls -F --show-control-chars --color=aut'
alias ll='ls -ltr'
alias la='ls -a'
alias lal='ls -al'

alias c='cygstart'
alias r='ruby'
alias l='less'
alias m='emacsclient -nw'

#alias cd=cdls
alias cdp='cd ../'
alias cdpp='cd ../../'

# short function
alias enable_bashrc="source ~/.bashrc"
alias enable_emacs="source ~/.bashrc"
alias reboot_emacs="emacsclient -e \'\(kill-emacs\)\';emacs --daemon"

# screen実行中にemacsで保存が効かない場合の対策
stty ixany
stty ixoff -ixon

############
# Function #
############
# cd して ls する
function cdls() {
    # cdがaliasでループするので\をつける
    \cd $1;
    ls;
}

###############
# Environment #
###############
export LD_LIBRARY_PATH=/usr/local/lib
export LIBRARY_PATH=/usr/local/lib
export CPATH=/usr/local/include

# screen実行中にemacsで閉じた場合の対策
stty ixany
stty ixoff -ixon

#initial exec
#screen -xR
#export LANG=ja_JP.utf8


# less vのときにemacs起動
# export EDITOR=emacs
# EmacsClient起動
export EDITOR='emacsclient -nw'
