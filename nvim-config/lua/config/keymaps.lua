-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua

local map = vim.keymap.set

-- Telescope with Ctrl+n to open, Space+n to close
map("n", "<C-n>", "<cmd>Telescope find_files<cr>", { desc = "Find Files" })
map("n", "<leader>n", "<cmd>lua vim.api.nvim_input('<Esc>')<cr>", { desc = "Close Telescope" })

-- Better escape
map("i", "jk", "<Esc>", { desc = "Exit insert mode" })
map("i", "kj", "<Esc>", { desc = "Exit insert mode" })

-- Save with Ctrl+S
map("n", "<C-s>", "<cmd>w<cr>", { desc = "Save file" })
map("i", "<C-s>", "<Esc><cmd>w<cr>", { desc = "Save file" })

-- Clear search highlight
map("n", "<leader>/", "<cmd>nohlsearch<cr>", { desc = "Clear search highlight" })
