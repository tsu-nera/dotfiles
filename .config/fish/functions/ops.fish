function ops --description 'vaio の常駐 Claude セッション（Discord DM と同じ文脈）に入る。抜けるのは C-o d'
    # vaio 上では ssh せずにそのまま入る
    if tmux -L discord-ops has-session -t discord-ops 2>/dev/null
        tmux -L discord-ops attach -t discord-ops
    else
        ssh -t vaio tmux -L discord-ops attach -t discord-ops
    end
end
