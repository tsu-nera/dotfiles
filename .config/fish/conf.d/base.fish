# CachyOS の cachyos-fish-config から使う部分だけ抜き出したもの（非 CachyOS でも同じ挙動にするため）
# 完了通知は同梱の done.fish（franciscolourenco/done）

if not status is-interactive
    exit
end

function fish_greeting
    command -q fastfetch; and fastfetch
end

set -g __done_min_cmd_duration 10000
set -g __done_notification_urgency_level low

# !! で直前のコマンド、!$ で直前の引数
function __history_previous_command
    switch (commandline -t)
        case "!"
            commandline -t $history[1]; commandline -f repaint
        case "*"
            commandline -i !
    end
end

function __history_previous_command_arguments
    switch (commandline -t)
        case "!"
            commandline -t ""
            commandline -f history-token-search-backward
        case "*"
            commandline -i '$'
    end
end

if test "$fish_key_bindings" = fish_vi_key_bindings
    bind -Minsert ! __history_previous_command
    bind -Minsert '$' __history_previous_command_arguments
else
    bind ! __history_previous_command
    bind '$' __history_previous_command_arguments
end

function history
    builtin history --show-time='%F %T ' $argv
end

function backup --argument filename
    cp $filename $filename.bak
end

alias fixpacman="sudo rm /var/lib/pacman/db.lck"
alias tarnow='tar -acf '
