function pacfast --description 'pacman -Syu を全開で実行（コワーキングの自制設定を一時無視。混雑してない時用）'
    # /etc/pacman.conf の速度制限(XferCommand)と並列数を一時的に外したコピーで実行する。
    # /etc 本体は変更しないので、次回の通常 pacman は自動でまた ~4Mbps 制限に戻る。
    echo "⚡ 全開モードで pacman 実行（XferCommand無効 / ParallelDownloads=10）"
    sudo pacman -Syu --disable-download-timeout \
        --config (sed -e 's|^XferCommand.*||' -e 's|^ParallelDownloads = 2|ParallelDownloads = 10|' /etc/pacman.conf | psub) $argv
end
