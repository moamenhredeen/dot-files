
vim.g.mapleader = ' '


-- define key mapping
local keymapping = {
	['vim'] = {
		['<leader>w'] = ':w',
		['<leader>x'] = ':x',
		['<leader>q'] = ':q',
	},

	['telescope.builtin'] = {
		['<leader>b'] 	= 'buffers',
		['<leader>f'] 	= 'git_files',
		['<leader>s'] 	= 'live_grep',
		['<leader>ab'] 	= 'git_branches',
		['<leader>ac'] 	= 'git_commits',
		['<leader>as'] 	= 'git_status',
		['<C-P>'] 			= 'commands',
		['<C-H>'] 			= 'help_tags',
		['<C-N>'] 			= 'man_pages',
	},
	
	['telescope-custom'] = {
		['<leader>ad'] 	= 'edit_dotfiles',
	},

	['utils'] = {
		['<leader>e'] 	= 'openNotesFile'
	}
}


-- apply defined keymapping
local map = vim.keymap.set
local options = {noremap = true}
for module, keymapping_table in pairs(keymapping) do
	for key, fn in pairs(keymapping_table) do
		if module == 'vim' then
			map('n', key, function() vim.cmd(fn) end, options)
		else
		 	map('n', key, function() require(module)[fn]() end, options)
		end
	end
end

