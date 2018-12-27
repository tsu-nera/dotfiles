function argu -d "Parse command options"
  # Iterate over each input argument to parse the option configuration.
  for arg in $argv
    set -e argv[1]
    switch "$arg"
      # We've reached the end of the option definitions.
      case --
        break
      # Short option.
      case '?::' '?:' '?'
        set options $options -$arg
      # Long option.
      case '*'
        set options $options --$arg
    end
  end

  # Now that we've parsed the option definitions, we can parse the given options
  # themselves.
  while set -q argv[1]
    switch "$argv[1]"
      # End all option parsing; everything else should be treated as values.
      case --
        set -e argv[1]
        break

      # Match a long option with a value.
      case '--*=*'
        set -l parts (echo $argv[1] | tr = '\n')

        if contains -- $parts[1] $options
          echo "Flag `$parts[1]' does not accept a value." >&2
          return 1
        end

        contains -- $parts[1]: $options
          or contains -- $parts[1]:: $options
          and echo -E "$parts"
          or echo "Unknown option `$parts[1]'." >&2

      # Match a short or long single option.
      case '--*' '-?'
        if contains -- $argv[1] $options
          echo -E "$argv[1]"
        else if contains -- $argv[1]: $options
          # Option must have a value.
          if begin; not set -q argv[2]; or expr "$argv[2]" : '-.*' > /dev/null; end
            echo "Option `$argv[1]' requires a value." >&2
            return 1
          end

          echo -E "$argv[1..2]"
          set -e argv[2]
        else if contains -- $argv[1]:: $options
          # Value is not required.
          if begin; not set -q argv[2]; or expr "$argv[2]" : '-.*' > /dev/null; end
            echo -E "$argv[1]"
          else
            echo -E "$argv[1..2]"
            set -e argv[2]
          end
        else
          echo "Unknown option `$argv[1]'." >&2
        end

      # Match a short option with a value, or a series of flags.
      case '-??*'
        set -l flag (echo $argv[1] | cut -c -2)

        # If first flag does not take a value, we assume all following characters
        # are also flags.
        if contains -- $flag $options
          echo -E "$flag"
          # Parse combined flags as well.
          set -l flags (echo $argv[1] | cut -c 3- | fold -w1)
          for flag in $flags
            if begin; contains -- -$flag $options; or contains -- -$flag:: $options; end
              echo -E "-$flag"
            else if contains -- -$flag: $options
              echo "Option `-$flag' requires a value." >&2
              return 1
            else
              echo "Unknown option `-$flag'." >&2
            end
          end
        else
          # We must have a single short option with a value.
          set -l value (echo $argv[1] | cut -c 3-)
          contains -- $flag: $options
            or contains -- $flag:: $options
            and echo -E "$flag $value"
            or echo "Unknown option `$flag'." >&2
        end

      # Match an unassoicated value.
      case '*'
        echo "_ $argv[1]"
    end

    set -e argv[1]
  end

  # Print out the remeaining values that do not belong to any option.
  for arg in $argv
    echo "_ $arg"
  end

  return 0
end
