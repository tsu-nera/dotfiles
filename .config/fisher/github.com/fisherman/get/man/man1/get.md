get(1) -- Press Any Key to Continue
===================================

## SYNOPSIS

`get` [[`--prompt`=]*msg*] [`--rule`=*regex*] [`--error`=*msg*] [`--silent`[=*num*]] <br>
`get` [`--quiet`] [`--no--case`] [`--no-cursor`] [`--help`]<br>

## DESCRIPTION

**Get** is an interactive prompt reader for [fish]. Use with [read(1)] to validate and store user input such as passwords and other data into variables.

## Options

* -p, --prompt=*msg*:
    Set the prompt message.

* -r, --rule=*regex*:
    Use the Regular Expression *regex* to validate the input. This creates an infinite read loop until the input matches *regex*.

* -e, --error=*msg*:
    Set the error message. *msg* is a [printf(1)] format string. Use the first positional %s to display the read input.

* -s, --silent[=*number*]:
    Hide the user input as it is being typed. Use this option to handle passwords or other sensitive data. Optionally, indicate a *number* to read up to a certain number of characters instead of a new line by default.

* -d, --default=*str*:
    Use default value if none selected.

* -q, --quiet:
    Suppress standard output.

* --no-case:
    Ignore case during validation.

* --no-cursor:
    Hide cursor.

* -h, --help:
    Show usage help.

## Usage

**Get** prints the user input to standard output by default.

```fish
get
```


Use --prompt=*msg* to display a prompt.

```fish
get --prompt "What's your name?\n-> "
```

Use --rule=*regex* to validate the user input.

```fish
get --prompt="Enter a number:" --rule="[0-9]"
```

Use --error=*msg* to display an error message when --rule=*regex* fails. The error message can be  [printf(1)] format string and the user input can be displayed using *%s*.

```fish
get --prompt="Enter a number:" --rule="[0-9]" --error="'%s' is not a valid number."
```

Use --silent to hide the user input and read a password into a variable.

```fish
get --prompt="Enter your password:" --silent
```

Using --silent in combination with --rule and --error will hide the input from the error message using a star character `*`.

```fish
get --prompt="Enter your password:" --rule="[0-9]" --error="Invalid password '%s'" --silent
```

Use --no-cursor to hide the cursor and --silent=*number* to read up to *number* of characters too.

Use --quiet to suppress writing the read input to standard output.

```fish
get --prompt="Press any key to continue..." --no-cursor --silent=1
```

## SEE ALSO

* read(1)

[printf(1)]: http://linux.die.net/man/1/printf
[read(1)]: http://fishshell.com/docs/current/commands.html#read
[fish]: https://github.com/fish-shell/fish-shell
