set -l cache $XDG_CACHE_HOME

if test -z "$cache"
    set cache ~/.cache
end

if test -s "$cache/gitignore_templates"
    read -laz t < "$cache/gitignore_templates"
    complete -c gitignore -a "$t" -f
end
