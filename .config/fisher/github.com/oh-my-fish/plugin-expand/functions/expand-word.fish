function expand-word -d 'Define a word expansion'
  set -l help "Define a word expansion.

Usage
  expand-word -p PATTERN -e EXPANSION
  expand-word -c CONDITION -e EXPANSION

Options
  -c, --condition Function whose return value determines when the expansion should be run
  -e, --expander  Function or string to expand the matching word with
  -h, --help      Show this help message
  -p, --pattern   A regular expression condition the word must match
"

  if not set -q argv[1]
    echo "$help"
    return 0
  end

  argu h help p: pattern: c: condition: e: expander: -- $argv | while read -l option value
    switch $option
      case -h --help
        echo "$help"
        return 0

      case -c --condition
        set condition "$value"

      case -p --pattern
        set condition "expand:match '$value'"

      case -e --expander
        set expander "$value"

      case _
        echo "Unexpected argument: $value" >&2
        return 1
    end
  end

  if not set -q condition
    echo "Expansions must have a condition" >&2
    return 1
  end

  if not set -q expander
    echo "You must specify an expander" >&2
    return 1
  end

  set -g __expand_expanders $__expand_expanders "$condition
$expander"
end
