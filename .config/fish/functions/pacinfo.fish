function pacinfo --description '現在の pacman 速度制限設定を表示（「何にしたっけ」確認用）'
    echo "XferCommand       : "(pacman-conf XferCommand)
    echo "ParallelDownloads : "(pacman-conf ParallelDownloads)
    echo "(コワーキング自制: --limit-rate 250k × 2 = 約4Mbps / 全開は pacfast / 戻すは sudo cp /etc/pacman.conf.bak /etc/pacman.conf)"
end
