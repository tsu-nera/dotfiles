set -x LANG ja_JP.UTF-8
set -x LD_LIBRARY_PATH /usr/local/lib
set -x LIBRARY_PATH /usr/local/lib
set -x CPATH /usr/local/include

set -U fish_user_paths ~/script/sh
set -U fish_user_paths ~/script/ruby $fish_user_paths
set -U fish_user_paths ~/script/scala $fish_user_paths
set -U fish_user_paths ~/script/python $fish_user_paths

set -x EDITOR "emacs -nw"
set -x VISUAL "emacs -nw"

# my bin
set -U fish_user_paths $HOME/bin $fish_user_paths

# anaconda
set -U fish_user_paths $HOME/anaconda3/bin $fish_user_paths

# cuda
# set -U fish_user_paths /usr/local/cuda/bin $fish_user_paths
# set -x LD_LIBRARY_PATH /usr/local/cuda/lib64 $LD_LIBRARY_PATH
# set -x CUDA_HOME /usr/local/cuda

# go
# set -x GOPATH $HOME/go
# set -U fish_user_paths $GOPATH/bin $fish_user_paths
