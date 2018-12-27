function expand:revert
  # Since we can't conditionally switch key bind modes, we need to pass through command completion requests again even
  # though we are already in expand mode.
  if not set -q __expand_replacements[1]
    commandline -f backward-delete-char
    return
  end

  # Replace the entire buffer with the original contents before any expansion was performed.
  commandline -r $__expand_last_string
end
