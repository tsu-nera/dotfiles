[![Slack Room][slack-badge]][slack-link]

# Menu

Create interactive prompt menus.

## Install

With [fisherman]

```
fisher menu
```

## Usage

```
menu 1 2 3
> 1  
  2
  3
```

## Options

### `menu_cursor_glyph`

Set cursor character.

Default: `>`.

### `menu_hover_item_style`

Set hover item style.

Default: None.

### `menu_multiple_choice`

Enable multiple choice mode.

Default: `false`.


### `menu_selected_item_style`

Set selected item style.

Default: None.

### `menu_checked_glyph`

Checked item glyph.

Default: `[x]`

### `menu_unchecked_glyph`

Unchecked item glyph.

Default: `[ ]`

### `menu_selected_index`

Use `menu_selected_index` to retrieve the selected item index from `$argv`.

### Example

```fish
set -l items "Batman" "Flash" "Superman" "Aquaman"

set -l menu_hover_item_style -o black -b yellow
set -l menu_cursor_glyph ▶
set -l menu_cursor_glyph_style -o

menu $items
```

[slack-link]: https://fisherman-wharf.herokuapp.com
[slack-badge]: https://fisherman-wharf.herokuapp.com/badge.svg
[fisherman]: https://github.com/fisherman/fisherman
