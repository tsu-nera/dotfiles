[![Build Status][travis-badge]][travis-link]
[![Slack Room][slack-badge]][slack-link]

# Spin

Background job spinner for fish.

## Install

With [fisherman]

```
fisher spin
```

## Usage

```fish
spin "sleep 1"
```

Spin interprets any output to standard error as failure. Use --error=*file* to redirect the standard error output to *file*.

```fish
if not spin --style=pipe --error=debug.txt "curl -sS $URL"
    return 1
end
```

![Spin Errors](https://cloud.githubusercontent.com/assets/8317250/13031549/0d07befe-d314-11e5-8cd7-e3e1d111c554.gif)

## Options

* -s, --style=*style*|*string*: Use *string* to slice the spinner characters. If you don't want to display the spinners, use --style=*""*.

* -f, --format=*format*: Use *format* to customize the spinner display. The default format is `"  @\r"` where `@` represents the spinner token and `\r` a carriage return, used to refresh (erase) the line.

* --error=*file*: Write the standard error output to a given *file*.

* -h, --help: Show usage help.

### Customization

Replace the default spinner with your own string of characters. For example, --style=*12345* will display the numbers from 1 to 5, and --style=*.* --format=*@* an increasing sequence of dots.

[travis-link]: https://travis-ci.org/fisherman/spin
[travis-badge]: https://img.shields.io/travis/fisherman/spin.svg
[slack-link]: https://fisherman-wharf.herokuapp.com/
[slack-badge]: https://fisherman-wharf.herokuapp.com/badge.svg
[fisherman]: https://github.com/fisherman/fisherman
