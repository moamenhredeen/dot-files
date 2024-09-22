
local vscode = require('vscode-neovim')

vim.g.mapleader = " "
vim.keymap.set('n', '<Leader>f', function ()
    vscode.action('workbench.action.quickOpen')
end)

vim.keymap.set('n', '<Leader>b', function ()
    vscode.action('workbench.action.quickOpen')
end)


vim.keymap.set('n', '<Leader>x', function ()
    vscode.action('workbench.action.showCommands')
end)


vim.keymap.set('n', '<Leader>e', function ()
    vscode.action('workbench.view.explorer')
end)


vim.keymap.set('n', '<Leader>o', function ()
    vscode.action('workbench.action.gotoSymbol')
end)


vim.keymap.set('n', 'gs', function ()
    vscode.action('workbench.action.gotoSymbol')
end)
