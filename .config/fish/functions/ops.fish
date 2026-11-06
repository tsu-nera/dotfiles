function ops --description 'vaio の常駐 Claude セッション（Discord の DM と同じ文脈）に入る。ops = クリプト、ops life = 生活。抜けるのは C-o d'
    set -l name discord-ops
    if test (count $argv) -gt 0
        set name $argv[1]
    end
    # vaio 上では ssh せずにそのまま入る
    if tmux -L $name has-session -t $name 2>/dev/null
        tmux -L $name attach -t $name
    else
        ssh -t vaio tmux -L $name attach -t $name
    end
end
