# Editor Support for Gloom

Syntax highlighting and editor support for the Gloom language.

## Vim/Neovim

### Quick Setup with Nix (Recommended)

If you use Nix, the easiest way to get Vim/Neovim with Gloom syntax highlighting is:

```bash
nix-shell
```

This provides an isolated development environment with Neovim configured to recognize `.gloom` files automatically. This is especially useful if you use a custom Neovim configuration (nvf, LazyVim, NvChad, etc.) that manages its own config.

### Installation

#### Quick Setup for Development

If you're working on the Gloom project and want syntax highlighting that updates automatically:

```bash
cd vim
./setup-local.sh
```

This creates symlinks instead of copying files, so any changes to `gloom.vim` are reflected immediately in your editor.

**Note:** This may not work with framework-managed Neovim configurations (nvf, LazyVim, etc.). Use nix-shell instead.

#### Manual Installation

1. Create the syntax directory if it doesn't exist:
```bash
mkdir -p ~/.vim/syntax
```

2. Copy the syntax file:
```bash
cp vim/gloom.vim ~/.vim/syntax/
```

3. Create a filetype detection file at `~/.vim/ftdetect/gloom.vim`:
```bash
mkdir -p ~/.vim/ftdetect
echo 'au BufRead,BufNewFile *.gloom set filetype=gloom' > ~/.vim/ftdetect/gloom.vim
```

#### For Neovim

Same as above, but use `~/.config/nvim/` instead of `~/.vim/`:

```bash
mkdir -p ~/.config/nvim/syntax
mkdir -p ~/.config/nvim/ftdetect
cp vim/gloom.vim ~/.config/nvim/syntax/
echo 'au BufRead,BufNewFile *.gloom set filetype=gloom' > ~/.config/nvim/ftdetect/gloom.vim
```

#### For Neovim with Custom Config (LazyVim, NvChad, AstroVim, etc.)

If you use a framework like LazyVim, NvChad, or nvf, add this to your Neovim config:

**Option 1: In your `init.lua` or similar:**
```lua
-- Gloom filetype detection
vim.filetype.add({
  extension = { gloom = 'gloom' },
})

vim.api.nvim_create_autocmd({"BufRead", "BufNewFile"}, {
  pattern = "*.gloom",
  callback = function()
    vim.bo.filetype = 'gloom'
  end,
})

-- Add syntax path
vim.opt.runtimepath:append('/path/to/bird/editor-support/vim')
```

**Option 2: Copy the Lua file:**
```bash
# Copy the filetype detection file to your config
cp vim/gloom-ft.lua ~/.config/nvim/lua/

# Then require it in your init.lua:
# require('gloom-ft')
```

**Option 3: Add to your lazy.nvim plugins (if using LazyVim):**
```lua
{
  dir = "/path/to/bird/editor-support/vim",
  ft = "gloom",
  config = function()
    vim.filetype.add({ extension = { gloom = 'gloom' } })
  end,
}
```

Replace `/path/to/bird` with the actual path to your Gloom project.

#### Using vim-plug (Vim/Neovim)

Add this to your `~/.vimrc` or `~/.config/nvim/init.vim`:

```vim
Plug 'JJoaoll/C4', {'rtp': 'editor-support/vim'}
```

Then run `:PlugInstall`

### Verification

Open a `.gloom` file and run:
```vim
:set filetype?
```

Should show: `filetype=gloom`

## Sublime Text

### Installation

1. Open Sublime Text
2. Go to: Preferences → Browse Packages
3. Copy the `sublime/Gloom.sublime-syntax` file to the Packages directory

### Verification

Open a `.gloom` file and check the syntax in the bottom-right corner. It should show "Gloom".

## Features

The syntax highlighting supports:

- Keywords: `fn`, `return`, `if`, `else`, `while`, `for`, `var`, `const`, `struct`
- Types: `Int`, `Float`, `String`, `Bool`
- Constants: `true`, `false`, `null`
- Comments: `//` and `/* */`
- Strings with escape sequences
- Numbers (integers and floats)
- Operators: arithmetic, comparison, logical
- Function names
- Custom types (capitalized identifiers)
- Array types: `[Type]`

## Contributing

To add support for other editors, create a new directory with the appropriate syntax files and update this README.
