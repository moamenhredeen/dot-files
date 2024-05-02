
local vscode = require('vscode-neovim')

vim.g.mapleader = " "
vim.keymap.set('n', '<Leader>f', function ()
    vscode.action('workbench.action.quickOpen')
end)

vim.keymap.set('n', '<Leader>b', function ()
    vscode.action('workbench.action.quickOpen')
end)