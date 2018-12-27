function peco_todoist_close
  todoist list | peco | cut -d ' ' -f 1 | tr '\n' ' ' | sed -e 's/[ \t]*$//' | read ret 
  if [ $ret ]
    todoist close $ret
    commandline -f repaint
  end
end
