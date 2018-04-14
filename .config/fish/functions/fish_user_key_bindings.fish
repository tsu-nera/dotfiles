end
set -q FZF_LEGACY_KEYBINDINGS
or set -l FZF_LEGACY_KEYBINDINGS 1
if test "$FZF_LEGACY_KEYBINDINGS" -eq 1
    bind \ct '__fzf_find_file'
    bind \cr '__fzf_reverse_isearch'
    bind \cx '__fzf_find_and_execute'
    bind \ec '__fzf_cd'
    bind \eC '__fzf_cd_with_hidden'
    if bind -M insert >/dev/null ^/dev/null
        bind -M insert \ct '__fzf_find_file'
        bind -M insert \cr '__fzf_reverse_isearch'
        bind -M insert \cx '__fzf_find_and_execute'
        bind -M insert \ec '__fzf_cd'
        bind -M insert \eC '__fzf_cd_with_hidden'
    end
else
    bind \cf '__fzf_find_file'
    bind \cr '__fzf_reverse_isearch'
    bind \ex '__fzf_find_and_execute'
    bind \eo '__fzf_cd'
    bind \eO '__fzf_cd_with_hidden'
< < < < < < < HEAD
if bind -M insert >/dev/null ^/dev/null
    =======
    if bind -M insert >/dev/null ^/dev/null
>> >> >> > f537b7b82a1c62734d06680f98927bd1b879052a
bind -M insert \cf '__fzf_find_file'
bind -M insert \cr '__fzf_reverse_isearch'
bind -M insert \ex '__fzf_find_and_execute'
bind -M insert \eo '__fzf_cd'
bind -M insert \eO '__fzf_cd_with_hidden'
### expand ###
bind --sets-mode expand \t expand:execute
bind --mode expand --sets-mode default --key backspace expand:revert
bind --mode expand \t expand:choose-next
bind --mode expand --sets-mode default '' ''
### expand ###
end
function fish_user_key_bindings
    ### fzf ###
    if test "$FZF_LEGACY_KEYBINDINGS" -eq 1
        bind \ct '__fzf_find_file'
        bind \cr '__fzf_reverse_isearch'
        bind \ec '__fzf_cd'
        bind \eC '__fzf_cd_with_hidden'
        if bind -M insert >/dev/null ^/dev/null
            bind -M insert \ct '__fzf_find_file'
            bind -M insert \cr '__fzf_reverse_isearch'
            bind -M insert \ec '__fzf_cd'
            bind -M insert \eC '__fzf_cd --hidden'
        end
    else
        bind \cf '__fzf_find_file'
        bind \cr '__fzf_reverse_isearch'
        bind \eo '__fzf_cd'
        bind \eO '__fzf_cd --hidden'
        if bind -M insert >/dev/null ^/dev/null
            bind -M insert \cf '__fzf_find_file'
            bind -M insert \cr '__fzf_reverse_isearch'
            bind -M insert \eo '__fzf_cd'
            bind -M insert \eO '__fzf_cd --hidden'
        end
    end
    set -q FZF_COMPLETE
    and bind \t '__fzf_complete'
    ### fzf ###
end
