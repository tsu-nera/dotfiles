# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# Touch Padデフォルト無効
xinput --set-prop "SynPS/2 Synaptics TouchPad" "Device Enabled" 0 &

# 背景の設定
feh --bg-scale ~/Pictures/wallpaper/moonlight.png &

# tmux
tmux-session restore

# trayer タスクトレイ
trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --width 10 --transparent true --tint 0x000000 --height 14 &

# ネットワーク
nm-applet &

# サウンド
cinnamon-sound-applet &
