return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = {
      ensure_installed = {},
      auto_install = false,
      highlight = { enable = true },
      indent = { enable = true },
    },
    config = function(_, opts)
      -- Don't try to install parsers in nix store
      require("nvim-treesitter.configs").setup(opts)
    end,
  },
}
