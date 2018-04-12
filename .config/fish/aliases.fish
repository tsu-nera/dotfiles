alias forced_git_local_destroy 'git fetch origin;git reset --hard origin/master'
alias xmap 'xmodmap ~/.Xmodmap'

# シャットダウン
alias fault 'sudo shutdown -P now'

alias .. 'cd ..'
alias ... 'cd ../..'
alias .... 'cd ../../..'
alias ..... 'cd ../../../..'

alias scrotclip 'scrot -s ~/foo.png; and xclip ~/foo.png; and rm ~/foo.png'

function conda-activate
    # anaconda
    set fish_user_paths $HOME/anaconda3/bin $fish_user_paths
end

alias diary 'emacs -nw /home/tsu-nera/Dropbox/diary/2018.txt'

alias labi 'lab issue'

######################################
## peco
######################################
set -x FILTER peco

function peco
  command peco --layout=bottom-up $argv
end

function ghq-remove
        ghq list --full-path | peco | xargs rm -r
end

function fish_user_key_bindings
        # ghqを選択
        # bind \cl peco_select_ghq_repository
        # gh-open
        bind \cx\cl peco_open_gh_repository
        # コマンド履歴を見る
        bind \cr peco_select_history
        # プロセスをキルする
        bind \cx\ck peco_kill
        # 最近見たディレクトリに移動
        bind \cx\cr peco_recentd

        # fzf
        bind \cx\cf '__fzf_find_file'
        bind \ctr '__fzf_reverse_isearch'
        bind \ex '__fzf_find_and_execute'
        bind \ed '__fzf_cd'
        bind \eD '__fzf_cd_with_hidden'
end

##################
# multi-display
##################
# set dual monitors
function dual
        xrandr --output DVI-I-1-1 --auto --right-of eDP1
end

function dual2
	xrandr --setprovideroutputsource 1 0 
        xrandr --output DVI-I-1-1 --auto --right-of HDMI-0 --mode 1440x900
end

function dual4
        xrandr --output eDP1 --primary --left-of HDMI1 --output HDMI1 --mode 1280x720
end

function dual3
        xrandr --output eDP1 --primary --left-of HDMI1 --output HDMI1 --auto
end

# set single monitor
function single
        xrandr --output DVI-I-1-1 --off
end

###########
## Emacs
###########
alias boot_emacs "emacs --daemon"
alias kill_emacs "emacsclient -e \"(kill-emacs)\""
alias m 'emacsclient -nw'

function reboot_emacs
	 kill_emacs;boot_emacs
end

function dired
        emacsclient -e "(dired \"$PWD\")"
end
## Chdir to the ``default-directory'' of currently opened in Emacs buffer.
function cde
        emacsclient -e "(return-current-working-directory-to-shell)" | sed 's/^"\(.*\)"$/\1/' | read EMACS_CWD
        echo "chdir to $EMACS_CWD"
        cd "$EMACS_CWD"
end

alias home 'ssh -p 10022 tsu-nera@fox10225fox.ddns.net'

alias touch_disable 'xinput --set-prop "SynPS/2 Synaptics TouchPad" "Device Enabled" 0'
alias touch_enable 'xinput --set-prop "SynPS/2 Synaptics TouchPad" "Device Enabled" 1'
