# `argu`: ARGument Understander
Sane and easy to use argument parser for [Oh My Fish][omf].

[![MIT License](https://img.shields.io/badge/license-MIT-007EC7.svg?style=flat-square)](LICENSE)

> Tired of arguing over arguments? Fear not! The ARGument Understander is here to help you understand clearly the arguments you are given and live a life of happiness.

Focus on writing your function, not on argument parsing. `argu` does the hard work of parsing function arguments for you in a standard way.


## Usage

    argu <definition>... -- [ARGV...]

`argu` parses a series of arguments by matching each one against a list of option definitions. A definition is simply a special string that describes how that argument is used. For example, if we had an `--awesome` flag, we could match it like this:

```fish
argu awesome -- $argv
```

If `$argv` contained the `--awesome` flag, it would be printed out appropriately. If our utility has multiple flags and options, we can tell `argu` about all of them and have them parsed:

```fish
argu a awesome b: boring: -- $argv
```

This defines four different options. First, we have an `-a` short flag, and a long `--awesome` flag. These are simply boolean in nature and either are present or not. We also define `b:` and `boring:`; the colon (`:`) indicates that these are options that have _values_, which accept a string associated with the option when passed. This will match something like `--boring tutorial` on the command line, where the string `tutorial` is associated with the `boring` option.

When defined this way, our options require a value. If a value is not given, such as passing in `-b` by itself, we get an error message. If we want our option values themselves to be optional, add another colon to the end of the definition:

```fish
argu a awesome b:: boring:: -- $argv
```

With this definition, it is OK to use `-b` or `--boring` by themselves or with an associated value.

Now to use our definitions, we simply place the arguments we want to parse (usually `$argv`) after the `--` argument:

```fish
$ argu a awesome b: boring: -- --awesome -b tutorial
--awesome
-b tutorial
```

If we try to put some flags into `$argv` that don't match any of our definitions, `argu` will exit and display a message for us:

```fish
$ argu a awesome b: boring: -- --verbose
Unknown option `--verbose'.
```

If you simply want to pass in some extra values that aren't associated with an option, you can do that too. Every such value will be associated with the underscore (`_`) special option:

```fish
$ argu a awesome b: boring: -- --awesome -b tutorial extra values
--awesome
-b tutorial
_ extra
_ values
```

When called, `argu` will parse the given arguments and print out each matching argument name and value. This output can be easily combined with `read` to begin acting on these values:

```fish
$ argu a awesome b: boring: -- --awesome -b tutorial | while read -l opt value
  echo "Option: $opt"
  echo "Value: $value"
end

Option: --awesome
Value:
Option: -b
Value: tutorial
```


## Examples
An actual code example speaks a thousand words, so here's an example on how `argu` can be used to parse function arguments:

```fish
function my_utility
  argu l long x: o:: optional:: -- $argv | while read -l opt value
    switch $opt
      case -l --long
        echo handle `-l --long`
      case -x
        echo handle `-x` w/ argument `$value`
      case -o --optional
        echo handle `-o --optional` w/ optional argument `$value`
      case _
        echo operand: `$value`
    end
  end
end
```


## Syntax
`argu` obtains options and their arguments from a list of parameters that, as indicated by each `<definition>`, are single letters preceded by a `-` or words preceded by `--` and possibly followed by an argument value. Each definition has the following grammar:

    <definition>  ::= <letter> <value> | <word> <value>
    <value>       ::= "" | ":" | "::"

A definition beginning with `<letter>` defines a short option, while a definition beginning with `<word>` defines a long option. A short option will match `-<letter>`, while a long option will match `--<word>`.

If a `<letter>` or `<word>` is followed by a `:`, the option is expected to have an argument, which may be supplied separately or next to the option without spaces in the same string. To indicate optional arguments, use an additional `:` character after a `:` at the end of the definition.

Both required and optional values for arguments can be supplied either in the same string as the option, or in the string following the option. For short options, the value can be appended without spaces, e.g, `-<letter>value`. For long options, use a `=` character after the option, e.g, `--<word>=value`.

In general, `argu` parses arguments according to [The Open Group Utility Syntax Guidelines][utilconv], except that it is less restrictive. The following is a summary of the features:

- Short options; single letters preceded by `-`, and long options; words preceded by `--`, are both supported.

- Single letters may be grouped. `-abc` → `-a -b -c`

- Options required to take an argument can specify the argument either in the same string as the option or separated from the by a space. (1) `-a argument`, (2) `-aargument`

- Options that can take an argument optionally shall specify the argument in the same string as the option argument if in short option style: `-aargument`, or separated by a `=` if in long form: `--long-form=argument`. If a blank space is used, the following argument will be treated independently.

- Options can appear multiple times in the same argument list. `argu` will print every match sequentially on each call, and should default to the short form of the option if available.


## Relation to getopts
`argu` has been written from scratch with a new algorithm, to improve performance and provide more consistent parsing. It aims to replace [plugin-getopts], a separate plugin. Inspiration is drawn from this plugin, but the usage and implementation are **not** related nor compatible. See the `README` on the above plugin to compare the changes if you are migrating to `argu` from [plugin-getopts].


## Inspiration and related links
- [The Open Group Base Specifications: Utility Conventions][utilconv]
- [GNU getopt](http://man7.org/linux/man-pages/man1/getopt.1.html)
- [zparseopts](http://linux.die.net/man/1/zshmodules)
- Credit goes to https://github.com/fishery/getopts for the `while read` idea for reading options.


## License
[MIT][mit] © [Stephen Coakley][author]. See the [AUTHORS](AUTHORS) file for a generated list of all contributors, and the [LICENSE](LICENSE) file for license details.


[author]: https://github.com/coderstephen
[getopts]: http://en.wikipedia.org/wiki/Getopts
[mit]: http://opensource.org/licenses/MIT
[omf]: https://www.github.com/oh-my-fish
[plugin-getopts]: https://github.com/oh-my-fish/plugin-getopts
[utilconv]: http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap12.html#tag_12_02
