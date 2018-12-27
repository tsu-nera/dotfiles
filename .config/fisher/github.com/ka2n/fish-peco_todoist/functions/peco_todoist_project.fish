function peco_todoist_project
  todoist projects | peco | head -n1 | cut -d ' ' -f 1 | read ret
  if [ $ret ]
    set buf (commandline | sed -e 's/[ \t]*$//')
    commandline "$buf -P $ret"
  end
end
