function peco_todoist_delete
  todoist list | peco | cut -d ' ' -f 1 | tr '\n' ' ' | sed -e 's/[ \t]*$//' | read ret 
  if [ $ret ]
    todoist delete $ret
    commandline -f repaint
  end
end
