-- Gloom filetype detection and syntax highlighting
-- Add this to your Neovim config or source it with: require('gloom-ft')

-- Detect .gloom files
vim.filetype.add({
  extension = {
    gloom = 'gloom',
  },
})

-- Ensure syntax highlighting is enabled
vim.api.nvim_create_autocmd({"BufRead", "BufNewFile"}, {
  pattern = "*.gloom",
  callback = function()
    vim.bo.filetype = 'gloom'
    vim.bo.syntax = 'gloom'
  end,
})
