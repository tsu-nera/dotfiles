#!/bin/sh
# fcitx5 の dbus 名(org.fcitx.Fcitx5)の所有者が変わったら oyainput を繋ぎ直す。
# oyainput は IM=fcitx で fcitx5 への dbus 接続を保持しており、fcitx5 が
# session 途中で再起動/置換されると接続が切れて変換が無言で止まるため、
# 新所有者の出現を検知して oyainput.service を自動再起動する。
#
# gdbus monitor の出力形式:
#   /org/freedesktop/DBus: org.freedesktop.DBus.NameOwnerChanged ('NAME', 'OLD', 'NEW')

gdbus monitor --session --dest org.freedesktop.DBus 2>/dev/null | while read -r line; do
    case "$line" in
        *"NameOwnerChanged ('org.fcitx.Fcitx5'"*)
            # 3番目(NEW = 新所有者)を取り出す。空 = fcitx5 が消えただけなので無視。
            new=$(printf '%s\n' "$line" | sed -n "s/.*'org.fcitx.Fcitx5', '[^']*', '\([^']*\)').*/\1/p")
            [ -n "$new" ] && systemctl --user restart oyainput.service
            ;;
    esac
done
