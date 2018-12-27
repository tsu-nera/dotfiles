[![Build Status][travis-badge]][travis-link]
[![Slack Room][slack-badge]][slack-link]

# Get

**Get** is an interactive prompt reader for [fish]. Use with [read(1)] to validate and store user input such as passwords and other data into variables.

## Install

With [fisherman]:

```fish
fisher get
```

## Options

* -p, --prompt=*msg*: Set the prompt message.

* -r, --rule=*regex*: Use the Regular Expression *regex* to validate the input. This creates an infinite read loop until the input matches *regex*.

* -e, --error=*msg*: Set the error message. *msg* is a [printf(1)] format string. Use the first positional %s to display the read input.

* -s, --silent[=*number*]: Hide the user input as it is being typed. Use this option to handle passwords or other sensitive data. Optionally, indicate a *number* to read up to a certain number of characters instead of a new line by default.

* -d, --default=*str*: Use default value if none selected.

* -q, --quiet: Suppress standard output.

* --no-case: Ignore case during validation.

* --no-cursor: Hide cursor.

* -h, --help: Show usage help.

## Usage

**Get** prints the user input to standard output by default.

```fish
get
```

![get](https://cloud.githubusercontent.com/assets/8317250/12975202/18dcee84-d0fd-11e5-9f4c-00348f3c7ede.gif)

Use --prompt=*msg* to display a prompt.

```fish
get --prompt "What's your name?\n-> "
```

![get](https://cloud.githubusercontent.com/assets/8317250/12975420/d32db93e-d0fe-11e5-813e-fb17c4935824.gif)

Use --rule=*regex* to validate the user input.

```fish
get --prompt="Enter a number:" --rule="[0-9]"
```

![get](https://cloud.githubusercontent.com/assets/8317250/12975525/d6f3ea60-d0ff-11e5-8180-2ec18cd228fa.gif)

Use --error=*msg* to display an error message when --rule=*regex* fails. The error message can be  [printf(1)] format string and the user input can be displayed using *%s*.

```fish
get --prompt="Enter a number:" --rule="[0-9]" --error="'%s' is not a valid number."
```

![get](https://cloud.githubusercontent.com/assets/8317250/12975598/50a6f9a6-d100-11e5-8d98-e7f28e125462.gif)

Use --silent to hide the user input and read a password into a variable.

```fish
get --prompt="Enter your password:" --silent
```

![get](https://cloud.githubusercontent.com/assets/8317250/12975660/e2514dac-d100-11e5-9464-0d926e65d7d5.gif)

Using --silent in combination with --rule and --error will hide the input from the error message using a star character `*`.

```fish
get --prompt="Enter your password:" --rule="[0-9]" --error="Invalid password '%s'" --silent
```

![get](https://cloud.githubusercontent.com/assets/8317250/12975740/7a158428-d101-11e5-9845-c514a0a4959e.gif)


Use --no-cursor to hide the cursor and --silent=*number* to read up to *number* of characters too.

> Use --quiet to suppress writing the read input to standard output.

```fish
get --prompt="Press any key to continue..." --no-cursor --silent=1
```

![get](https://cloud.githubusercontent.com/assets/8317250/12999807/870a0a42-d196-11e5-9f59-7d93411c2adf.gif)

[slack-link]: https://fisherman-wharf.herokuapp.com
[slack-badge]: https://fisherman-wharf.herokuapp.com/badge.svg

[travis-link]: https://travis-ci.org/fisherman/get
[travis-badge]: https://img.shields.io/travis/fisherman/get.svg

[fisherman]: https://github.com/fisherman/fisherman
[printf(1)]: http://linux.die.net/man/1/printf
[read(1)]: http://fishshell.com/docs/current/commands.html#read
[fish]: https://github.com/fish-shell/fish-shell
