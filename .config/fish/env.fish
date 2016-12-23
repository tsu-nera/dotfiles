export LANG=ja_JP.UTF-8
export LD_LIBRARY_PATH=/usr/local/lib
export LIBRARY_PATH=/usr/local/lib
export CPATH=/usr/local/include

if test -d ~/script
        set -U fish_user_paths ~/script/sh $fish_user_paths
        set -U fish_user_paths ~/script/ruby $fish_user_paths
        set -U fish_user_paths ~/script/scala $fish_user_paths        
end

set -x EDITOR emacs

# for ruby
if test -x "`which ruby`"
        export fish_user_paths $HOME/.gem/ruby/2.1.0/bin fish_user_paths
end
