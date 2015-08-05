#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Touch Padデフォルト無効
xinput --set-prop "SynPS/2 Synaptics TouchPad" "Device Enabled" 0 &

##########
# xmonad
##########
# set the cursor 矢印にする
xsetroot -cursor_name left_ptr
# set Japanese keyboard layout
setxkbmap -layout jp

# xmodmap
# 起動時には、すぐにxmodmapが反映されない。そのため、5秒のdelayを入れる。
if [ -s ~/.Xmodmap ]; then
    /bin/bash -c "sleep 5;/usr/bin/xmodmap /home/$USER/.Xmodmap"
fi

# Boot Emacs
emacs --daemon &

# Emacs Speedup
xset r rate 200 25

#export GTK_IM_MODULE=fcitx
#export QT_IM_MODULE=fcitx
#XCexport GTK_IM_MODULE=xim
#export QT_IM_MODULE=xim
#export XMODIFIERS="@im=fcitx"
#fcitx -d

# xmonad
exec xmonad

# emacs
#exec emacs
