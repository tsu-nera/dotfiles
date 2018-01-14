set -x LANG ja_JP.UTF-8
set -x LD_LIBRARY_PATH /usr/local/lib
set -x LIBRARY_PATH /usr/local/lib
set -x CPATH /usr/local/include

set fish_user_paths ~/script/sh $fish_user_paths
set fish_user_paths ~/script/ruby $fish_user_paths
set fish_user_paths ~/script/scala $fish_user_paths        

set -x EDITOR "emacs -nw"
set -x VISUAL "emacs -nw"

# my bin
set fish_user_paths $HOME/bin $fish_user_paths

# anaconda
set fish_user_paths $HOME/anaconda3/bin $fish_user_paths
