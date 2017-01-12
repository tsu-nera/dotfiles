balias o 'xdg-open'
balias c 'cygstart'
balias forced_git_local_destroy 'git fetch origin;git reset --hard origin/master'
balias xmap 'xmodmap ~/.Xmodmap'

# Emacs関連
balias boot_emacs "emacs --daemon"
balias kill_emacs "emacsclient -e \"(kill-emacs)\""
balias m 'emacsclient -nw'

function reboot_emacs
	 kill_emacs;boot_emacs
end

# 画面ロック
balias lock 'gnome-screensaver-command -l'

# シャットダウン
balias fault 'sudo shutdown -P now'

balias td 'todoist --color'

#######################################
## peco
######################################
function peco
  command peco --layout=bottom-up $argv
end

function ghq-remove
        ghq list --full-path | peco | xargs rm -r -i
end

function fish_user_key_bindings
        # ghqを選択
        bind \cl peco_select_ghq_repository
        # gh-open
        bind \cx\cl peco_open_gh_repository
        # コマンド履歴を見る
        bind \cr peco_select_history
        # プロセスをキルする
        bind \cx\ck peco_kill
        # 最近見たディレクトリに移動
        bind \cx\cr peco_recentd

        # fzf
        bind \cx\cf '__fzf_find_file'
        # bind \ct '__fzf_reverse_isearch'
        bind \ex '__fzf_find_and_execute'
        bind \ed '__fzf_cd'
        bind \eD '__fzf_cd_with_hidden'

        # todoist
        bind \ctt peco_todoist_item
        bind \ctp peco_todoist_project
        bind \ctl peco_todoist_labels
        bind \ctc peco_todoist_close
        bind \ctd peco_todoist_delete
end
#######################################################
# multi-display
#######################################################
# set dual monitors
function dual
        xrandr --output eDP1 --primary --left-of HDMI1 --output HDMI1 --mode 1280x720
end
        
function dual2
        xrandr --output eDP1 --primary --left-of HDMI1 --output HDMI1 --auto
end

# set single monitor
function single
        xrandr --output HDMI1 --off
end

############
## Emacs
###########
function dired 
        emacsclient -e "(dired \"$PWD\")"
end

## Chdir to the ``default-directory'' of currently opened in Emacs buffer.
function cde
        emacsclient -e "(return-current-working-directory-to-shell)" | sed 's/^"\(.*\)"$/\1/' | read EMACS_CWD
        echo "chdir to $EMACS_CWD"
        cd "$EMACS_CWD"        
end
