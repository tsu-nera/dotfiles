function peco_open_gh_repository
  set -l query (commandline)

  if test -n $query
    set peco_flags --query "$query"
  end

  ghq list -p | peco $peco_flags | read line

  if [ $line ]
    gh-open $line
    commandline -f repaint
  end
end
