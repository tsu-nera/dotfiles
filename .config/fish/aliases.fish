# eza (ls replacement)
alias ls='eza -al --color=always --group-directories-first --icons'
alias la='eza -a --color=always --group-directories-first --icons'
alias ll='eza -l --color=always --group-directories-first --icons'
alias lt='eza -aT --color=always --group-directories-first --icons'
alias l.="eza -a | grep -e '^\.'"

# Navigation
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

# Git
alias g git

# System
alias fault='sudo shutdown -P now'
alias update='sudo pacman -Syu'
alias cleanup='sudo pacman -Rns (pacman -Qtdq)'
alias jctl='journalctl -p 3 -xb'

# SSH
alias home='ssh -p 10022 tsu-nera@fox10225fox.ddns.net'

# Emacs
alias boot_emacs='emacs --daemon'
alias kill_emacs='emacsclient -e "(kill-emacs)"'
alias m='emacsclient -nw'

function reboot_emacs
    kill_emacs
    boot_emacs
end

function dired
    emacsclient -e "(dired \"$PWD\")"
end

function cde
    emacsclient -e "(return-current-working-directory-to-shell)" | sed 's/^"\(.*\)"$/\1/' | read EMACS_CWD
    echo "chdir to $EMACS_CWD"
    cd "$EMACS_CWD"
end
