![][license-badge]

<div align="center">
  <a href="http://github.com/oh-my-fish/oh-my-fish">
  <img width=90px  src="https://cloud.githubusercontent.com/assets/8317250/8510172/f006f0a4-230f-11e5-98b6-5c2e3c87088f.png">
  </a>
</div>
<br>

# pbcopy

OSX's `pbcopy` and `pbpaste` support for Linux.

## Install

```fish
$ omf install pbcopy
```

## Usage

Copy a list of files in your home directory to the clipboard:
```fish
$ ls ~ | pbcopy
```

Copy the contents of a file to the clipboard:
```fish
$ pbcopy < cookies.txt
```

Copy part of a file to the clipboard:
```fish
$ grep 'ip address' serverlist.txt | pbcopy
```

Paste from your clipboard to stdout:
```fish
$ pbpaste
```

Paste from your clipboard to a file:
```fish
$ pbpaste > clipboard.txt
```

Paste from your clipboard to a file in a remote host:
```fish
$ pbpaste | ssh username@host 'cat > ~/myclipboard.txt'
```

# License

[MIT][mit] © [Oh My Fish!][author] [contributors][contributors]


[mit]:            http://opensource.org/licenses/MIT
[author]:         http://github.com/oh-my-fish
[contributors]:   https://github.com/oh-my-fish/plugin-pbcopy/graphs/contributors
[omf-link]:       https://www.github.com/oh-my-fish/oh-my-fish

[license-badge]:  https://img.shields.io/badge/license-MIT-007EC7.svg?style=flat-square
