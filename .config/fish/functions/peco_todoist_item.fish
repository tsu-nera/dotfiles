function peco_todoist_item
  todoist list | peco | cut -d ' ' -f 1 | tr '\n' ' ' | read ret
  if [ $ret ]
    set buf (commandline | sed -e 's/[ \t]*$//')
    commandline "$buf $ret"
  end
end
