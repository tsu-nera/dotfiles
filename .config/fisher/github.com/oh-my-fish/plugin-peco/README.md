<img src="https://cdn.rawgit.com/oh-my-fish/oh-my-fish/e4f1c2e0219a17e2c748b824004c8d0b38055c16/docs/logo.svg" align="left" width="144px" height="144px"/>

# peco

[![MIT License](https://img.shields.io/badge/license-MIT-007EC7.svg?style=flat-square)](/LICENSE)
[![Fish Shell Version](https://img.shields.io/badge/fish-v2.2.0+-007EC7.svg?style=flat-square)](http://fishshell.com)
[![Oh My Fish Framework](https://img.shields.io/badge/Oh%20My%20Fish-Framework-007EC7.svg?style=flat-square)](https://www.github.com/oh-my-fish/oh-my-fish)

<br/>

Use [peco][peco] with [fish shell][fish-shell] managed by [Oh my
fish][omf-link] in a very simple way!

Based directly on the percol plugin.

## Installation

```fish
omf install peco
```

## Usage

Please bind `peco_select_history` to your favorite key.

If you'd like to bind Ctrl+r:

```fish
function fish_user_key_bindings
  bind \cr 'peco_select_history (commandline -b)'
end
```

# License

[MIT][mit] © [Oh My Fish! Authors][author] et [al][contributors]


[mit]:           http://opensource.org/licenses/MIT
[author]:        https://github.com/oh-my-fish/plugin-peco/blob/master/LICENSE
[contributors]:  https://github.com/oh-my-fish/plugin-peco/graphs/contributors
[omf-link]:      https://github.com/oh-my-fish/oh-my-fish
[fish-shell]:    https://fishshell.com
[peco]:          https://github.com/peco/peco

[license-badge]: https://img.shields.io/badge/license-MIT-007EC7.svg?style=flat-square
