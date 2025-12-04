{ pkgs ? import <nixpkgs> {} }:

let
  plugins = with pkgs.vimPlugins; [
    LazyVim
    bufferline-nvim
    cmp-buffer
    cmp-nvim-lsp
    cmp-path
    cmp_luasnip
    conform-nvim
    dashboard-nvim
    dressing-nvim
    flash-nvim
    friendly-snippets
    gitsigns-nvim
    indent-blankline-nvim
    lualine-nvim
    neo-tree-nvim
    neoconf-nvim
    neodev-nvim
    noice-nvim
    nui-nvim
    nvim-cmp
    nvim-lint
    nvim-lspconfig
    nvim-notify
    nvim-spectre
    nvim-treesitter
    nvim-treesitter-context
    nvim-treesitter-textobjects
    nvim-ts-autotag
    nvim-ts-context-commentstring
    nvim-web-devicons
    persistence-nvim
    plenary-nvim
    telescope-fzf-native-nvim
    telescope-nvim
    todo-comments-nvim
    tokyonight-nvim
    trouble-nvim
    vim-illuminate
    vim-startuptime
    which-key-nvim
    luasnip
    catppuccin-nvim
    gruvbox-nvim
    mini-nvim
  ];
  
  mkEntryFromDrv = drv:
    if pkgs.lib.isDerivation drv then
      { name = "${pkgs.lib.getName drv}"; path = drv; }
    else
      drv;
      
  lazyPath = pkgs.linkFarm "lazy-plugins" (builtins.map mkEntryFromDrv plugins);
  
  treesitterParsers = pkgs.symlinkJoin {
    name = "treesitter-parsers";
    paths = (pkgs.vimPlugins.nvim-treesitter.withPlugins (plugins: with plugins; [
      c
      lua
      vim
      vimdoc
      query
    ])).dependencies;
  };
  
  gloomNeovim = pkgs.neovim.override {
    configure = {
      packages.myPlugins = with pkgs.vimPlugins; {
        start = [ lazy-nvim ];
      };
      
      customRC = ''
        set runtimepath+=${toString ./editor-support/vim}
        set runtimepath+=${toString ./nvim-config}
        set runtimepath+=${treesitterParsers}
        
        lua << EOF
        -- Use nix-provided treesitter parsers
        vim.opt.runtimepath:append("${treesitterParsers}/parser")
        
        require("lazy").setup({
          defaults = {
            lazy = true,
          },
          dev = {
            path = "${lazyPath}",
            patterns = { "" },
            fallback = true,
          },
          spec = {
            { "LazyVim/LazyVim", import = "lazyvim.plugins" },
            { "nvim-telescope/telescope-fzf-native.nvim", enabled = true },
            { "williamboman/mason-lspconfig.nvim", enabled = false },
            { "williamboman/mason.nvim", enabled = false },
            { "nvim-treesitter/nvim-treesitter", opts = { ensure_installed = {} } },
            { import = "plugins" },
          },
        })
        EOF
      '';
    };
  };
in

pkgs.mkShell {
  buildInputs = with pkgs; [
    ghc
    cabal-install
    haskellPackages.haskell-language-server
    haskellPackages.ghcid
    gloomNeovim
    lua-language-server
    stylua
    ripgrep
    zlib
    pkg-config
  ];

  shellHook = ''
    echo "╔════════════════════════════════════════╗"
    echo "║   Gloom Development Environment        ║"
    echo "╚════════════════════════════════════════╝"
    echo ""
    echo "GHC version: $(ghc --version)"
    echo "Cabal version: $(cabal --version | head -1)"
    echo ""
    
    GLOOM_BINARY=$(find dist-newstyle -name "haskell" -type f -executable 2>/dev/null | head -1)
    
    if [ -n "$GLOOM_BINARY" ]; then
      alias gloom="$GLOOM_BINARY"
      export GLOOM_BINARY
      
      echo "✓ Gloom compiler ready!"
      echo ""
      echo "Gloom Commands:"
      echo "  gloom <file.gloom>          - Compile and run"
      echo "  gloom --check <file.gloom>  - Type check only"
      echo "  gloom --help                - Show help"
      echo ""
    else
      echo "⚠ Gloom binary not found. Run 'cabal build' first."
      echo ""
    fi
    
    echo "Editor:"
    echo "  nvim <file.gloom>  - Edit with LazyVim + Gloom support"
    echo ""
  '';

  NIX_GHC_LIBDIR = "${pkgs.ghc}/lib/ghc-${pkgs.ghc.version}";
}
