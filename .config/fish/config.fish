# ###############
# # fish config
# ###############
# alias & function 読み込み
. ~/.config/fish/aliases.fish

# 環境変数読み込み
if status --is-login
    . ~/.config/fish/env.fish
end



# for go lang
# if [ -x "`which go`" ]; then
#   export GOPATH=$HOME/go
#   export PATH=$PATH:$GOROOT/bin:$GOPATH/bin
# fi

# for ruby
# if [ -x "`which ruby`" ]; then
#   export PATH=$HOME/.gem/ruby/2.1.0/bin:$PATH
# fi

# ------------------------------------------------------------------------
# OSによる場合分け
# ------------------------------------------------------------------------
# case $OSTYPE in
#   cygwin*)
#   alias executer="cygstart"
#   ;;
#   linux*)
#   alias executer="xdg-open"
#   ;;
# esac

# ------------------------------------------------------------------------
# Functions
# ------------------------------------------------------------------------
# cd して ls する
# function cdls() {
#     # cdがaliasでループするので\をつける
#     \cd $1;
#     ls;
# }

# # history 全表示
# function history-all { history -E 1 }

# ------------------------------------------------------------------------
# Name     : isemacs
# History  : 2014/04/20
# Function : emacsか判定
# Refs     :
# http://sakito.jp/emacs/emacsshell.html#term
# ------------------------------------------------------------------------
# function isemacs(){
#    [[ "$EMACS" != "" ]] && return 0
#    echo "emacs"
#    return 1
# }

# ------------------------------------------------------------------------
# Name     : show_buffer_stack()
# History  : 2014/04/20
# Function : コマンドラインスタック
# Refs:
# http://d.hatena.ne.jp/kei_q/20110308/1299594629
# http://qiita.com/items/1f2c7793944b1f6cc346
# ------------------------------------------------------------------------
# show_buffer_stack() {
#     POSTDISPLAY="
# stack: $LBUFFER"
#     zle push-line-or-edit
# }
# zle -N show_buffer_stack
# setopt noflowcontrol
# bindkey '^Q' show_buffer_stack  # Ctrl + q

# # クリップボードコピー
# if [ -x "`which xsel`" ]; then
# xsel-buffer(){
#     print -rn $BUFFER | xsel -i
#     zle -M "xsel -i ${BUFFER}"
# }
# fi

# zle -N xsel-buffer
# bindkey '^x^p' xsel-buffer # C-x C-p

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
# export TERM=xterm-256color
# alias mc='emacsclient -nc'

# if [ "$EMACS" ];then
# else
#     alias m='emacsclient -nw'
#     alias kill_emacs="emacsclient -e \"(kill-emacs)\""
#     alias boot_emacs="LC_CTYPE=ja_JP.UTF-8 emacs --daemon"
#     alias reboot_emacs="kill_emacs;boot_emacs"

#     export EDITOR="emacsclient -nw"
#     export VISUAL="emacsclient -nw"
#     # zsh起動時にemacs --daemon起動
#     # この機能は封印 cygwin上だとemacsの起動時間が遅いので。
#     # if pgrep emacs >/dev/null 2>&1; then
#     #     # echo "Emacs server is already running..."
#     #   else
#     #     `emacs --daemon`
#     # fi
#     # いろなし
# fi

# ## Invoke the ``dired'' of current working directory in Emacs buffer.
# function dired () {
# emacsclient -e "(dired \"${1:a}\")"
# }
 
# ------------------------------------------------------------------------
# Name     : cde
#   http://d.hatena.ne.jp/syohex/20111026/1319606395
# ------------------------------------------------------------------------
# ## Chdir to the ``default-directory'' of currently opened in Emacs buffer.
# function cde () {
#     EMACS_CWD=`emacsclient -e "
#       (if (featurep 'elscreen)
#           (elscreen-current-directory)
#         (non-elscreen-current-directory))" | sed 's/^"\(.*\)"$/\1/'`

#     echo "chdir to $EMACS_CWD"
#     cd "$EMACS_CWD"
# }

# ------------------------------------------------------------------------
# Name     : cdr
# Function : 最近訪れたフォルダへ移動
# ------------------------------------------------------------------------
# autoload -Uz is-at-least
# if is-at-least 4.3.11
# then
#   autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
#   add-zsh-hook chpwd chpwd_recent_dirs
#   zstyle ':chpwd:*' recent-dirs-max 5000
#   zstyle ':chpwd:*' recent-dirs-default yes
#   zstyle ':completion:*' recent-dirs-insert both
# fi

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
# if [ -x "`which peco`" ]; then
# alias -g P='| peco'
# alias llp='ls -la | peco'
# alias lsp='lsv| peco'
# alias tp='top | peco'
# alias pp='ps aux | peco'

# if [ -x "`which anamnesis`" ]; then
#     alias ap='anamnesis -l 200 | peco'
# fi

# ------------------------------------------------------------------------
# history filter
# http://qiita.com/uchiko/items/f6b1528d7362c9310da0
# ------------------------------------------------------------------------
# function peco-select-history() {
#     local tac
#     if which tac > /dev/null; then
#         tac="tac"
#     else
#         tac="tail -r"
#     fi
#     BUFFER=$(\history -n 1 | \
#         eval $tac | \
#         peco --query "$LBUFFER")
#     CURSOR=$#BUFFER
#     zle clear-screen
# }
# zle -N peco-select-history

# ------------------------------------------------------------------------
# peco-recentd
# 最近訪れたディレクトリに移動
# ------------------------------------------------------------------------
# function peco-recentd () {
#     local selected_dir=$(cdr -l | awk '{ print $2 }' | peco)
#     if [ -n "$selected_dir" ]; then
#         BUFFER="cd ${selected_dir}"
#         zle accept-line
#     fi
#     zle clear-screen
# }
# zle -N peco-recentd

# ------------------------------------------------------------------------
# peco-kill-process
# プロセスキル
# ------------------------------------------------------------------------
# function peco-kill-process () {
#     ps -ef | peco | awk '{ print $2 }' | xargs kill
#     zle clear-screen
# }
# zle -N peco-kill-process

#------------------------------------------------------------------------
# peco-find-file
# emacsでカレントディレクトリのファイルを開く
#------------------------------------------------------------------------
# function peco-find-file () {
#     ls | peco | xargs emacsclient -nw
#     zle clear-screen
# }
# zle -N peco-find-file

#------------------------------------------------------------------------
# peco-open-app
# カレントディレクトリのファイルを開く
#------------------------------------------------------------------------
# function peco-open-app () {
#     case $OSTYPE in
# 	cygwin*)
# 	    ls | peco | xargs cygstart
# 	    ;;
# 	linux*)
#             ls | peco | xargs xdg-open
# 	    ;;
#     esac
# #    zle clear-screen
# }
# zle -N peco-open-app

#------------------------------------------------------------------------
# peco-ag
# agで検索して、emacsで開く
#------------------------------------------------------------------------
# if [ -x "`which ag`" ]; then
# function peco-ag () {
#     ag $@ | peco --query "$LBUFFER" | awk -F : '{print "+" $2 " " $1}' | xargs emacsclient -nw
# }
# fi

#------------------------------------------------------------------------
# peco-M-x
# コマンド一覧を表示して実行
#------------------------------------------------------------------------
# function peco-M-x () {
#     local cmd=$(peco-M-x.rb | peco)
#     eval $cmd &
# }
# zle -N peco-M-x

#------------------------------------------------------------------------
# peco-docker
# http://qiita.com/sonodar/items/d2de15b98581108b5de8
#------------------------------------------------------------------------
# pecoで選択したコンテナに対して操作を行う

# if [ -x "`which docker`" ]; then
# docker_peco_containers() {
#   if [ $# -lt 1 ]; then
#       echo "Usage: dpc [OPTIONS] COMMAND [args]" >&2
#       return 1
#   fi
  
#   docker ps -a | peco | while read CONTAINER
#   do
#       docker $@ `echo $CONTAINER | awk '{print $1}'`
#   done
# }

# # pecoで選択したイメージに対して操作を行う
# docker_peco_images() {
#     if [ $# -lt 1 ]; then
# 	echo "Usage: dpi [OPTIONS] COMMAND [args]" >&2
# 	return 1
#     fi

#     unset DOCKER_OPTS
#     [[ -z $ENVS ]] || \
# 	for e in $ENVS; do DOCKER_OPTS="$DOCKER_OPTS -e $e"; done
#     [[ -z $VOLUMES ]] || \
# 	for v in $VOLUMES; do DOCKER_OPTS="$DOCKER_OPTS -v $v"; done
#     [[ -z $PORTS ]] || \
# 	for p in $PORTS; do DOCKER_OPTS="$DOCKER_OPTS -p $p"; done
#     docker images | peco | while read IMAGE
#     do
# 	docker $DOCKER_OPTS $@ `echo $IMAGE | awk '{print $3}'`
#     done
# }

# # エイリアスはお好みで
# alias dpc='docker_peco_containers'
# alias dpi='docker_peco_images'
# fi

########
# git
########
# function git-ignore() {
#     curl -s https://www.gitignore.io/api/$@ ;
# }

# function git-ignore-list() {
#     git-ignore `git-ignore list | ruby -ne 'puts $_.split(",")' | peco`
# }

################
# key bindings
################
# bindkey '^xb'  peco-select-history # C-x b
# bindkey '^[x'  peco-M-x            # M-x
# bindkey '^x^r' peco-recentd        # C-x C-r
# bindkey '^xk'  peco-kill-process   # C-x k
# bindkey '^x^f' peco-find-file      # C-x C-f
# bindkey '^xo'  peco-open-app       # C-x o
# fi

#######################################################
# highlight
# https://github.com/zsh-users/zsh-syntax-highlighting
#######################################################
# if [ -d ${HOME}/.zsh/zsh-syntax-highlighting  ] ; then
#     source ${HOME}/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
# fi

# source ~/.zshenv


#######################################################
# multi-display
#######################################################
# # set dual monitors
# dual () {
#     xrandr --output eDP1 --primary --left-of HDMI1 --output HDMI1 --mode 1280x720
# }

# dual2 () {
#     xrandr --output eDP1 --primary --left-of HDMI1 --output HDMI1 --auto
# }

# # set single monitor
# single () {
#     xrandr --output HDMI1 --off
# }
