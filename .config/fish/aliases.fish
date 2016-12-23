alias o 'xdg-open'
alias c 'cygstart'
alias forced_git_local_destroy 'git fetch origin;git reset --hard origin/master'
alias xmap 'xmodmap ~/.Xmodmap'

# Emacs関連
alias m 'emacsclient -nw'
alias kill_emacs "emacsclient -e \"(kill-emacs)\""
alias boot_emacs "LC_CTYPE=ja_JP.UTF-8 emacs --daemon"
alias reboot_emacs "kill_emacs;boot_emacs"

# 画面ロック
alias lock 'gnome-screensaver-command -l'

# シャットダウン
alias fault 'sudo shutdown -P now'

# Power Line
# function fish_prompt
#     ~/powerline-shell.py $status --shell bare ^/dev/null
# end

#######################################################
# multi-display
#######################################################
# set dual monitors
function dual
        xrandr --output eDP1 --primary --left-of HDMI1 --output HDMI1 --mode 1280x720
end
        
function dual2
        xrandr --output eDP1 --primary --left-of HDMI1 --output HDMI1 --auto
end

# set single monitor
function single
    xrandr --output HDMI1 --off
end
