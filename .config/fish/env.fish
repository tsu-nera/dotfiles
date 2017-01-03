export LANG=ja_JP.UTF-8
export LD_LIBRARY_PATH=/usr/local/lib
export LIBRARY_PATH=/usr/local/lib
export CPATH=/usr/local/include

set fish_user_paths ~/script/sh $fish_user_paths
set fish_user_paths ~/script/ruby $fish_user_paths
set fish_user_paths ~/script/scala $fish_user_paths        

set -x EDITOR emacs
set -x VISUAL "emacsclient -nw"

# rbenv
set fish_user_paths $HOME/.rbenv/bin $fish_user_paths

# for go
set -x GOPATH $HOME/go
set fish_user_paths $GOROOT/bin $fish_user_paths 
set fish_user_paths $GOPATH/bin $fish_user_paths 

# my bin
set fish_user_paths $HOME/bin $fish_user_paths

# cask
set fish_user_paths $HOME/.cask/bin $fish_user_paths

# notmuch
set -x XAPIAN_CJK_NGRAM 1
