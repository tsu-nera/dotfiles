function peco_recentd
        z -l | peco | awk '{ print $2 }' | read recentd
        cd $recentd
end
