vim.g.mapleader = ' '

-- define key mapping
local keymapping = {
	['vim'] = {
		['<leader>w'] 	= {command = ':w', 							mode = {'n'}},
		['<leader>x'] 	= {command = ':x', 							mode = {'n'}},
		['<leader>q'] 	= {command = ':q!', 						mode = {'n'}},
		['<C-j>'] 			= {command = ':wincmd j', 			mode = {'n', 't'}},
		['<C-k>'] 			= {command = ':wincmd k', 			mode = {'n', 't'}},
		['<C-h>'] 			= {command = ':wincmd h', 			mode = {'n', 't'}},
		['<C-l>'] 			= {command = ':wincmd l', 			mode = {'n', 't'}},
		['<leader>e'] 	= {command = 'NvimTreeToggle', 	mode = {'n'}},
		['<leader>t'] 	= {command = 'ToggleTerm', 			mode =  {'n'}}
	},

	['telescope.builtin'] = {
		['<leader>b'] 	= {command = 'buffers', 				mode = {'n'}},
		['<leader>f'] 	= {command = 'git_files', 			mode = {'n'}},
		['<leader>s'] 	= {command = 'live_grep', 			mode = {'n'}},
		['<leader>ab'] 	= {command = 'git_branches', 		mode = {'n'}},
		['<leader>ac'] 	= {command = 'git_commits', 		mode = {'n'}},
		['<leader>as'] 	= {command = 'git_status', 			mode = {'n'}},
		['<leader>ap'] 	= {command = 'commands', 				mode = {'n'}},
		['<leader>am'] 	= {command = 'man_pages', 			mode = {'n'}},
		['<leader>ah'] 	= {command = 'help_tags', 			mode =  {'n'}},
		-- ['<C-P>'] 		= {command = 'commands', 				mode = {'n'}},
		-- ['<C-H>'] 		= 'help_tags',
		-- ['<C-N>'] 		= {command = 'man_pages', 			mode = {'n'}},
	},

	['configure-plugins.telescope-custom'] = {
		['<leader>ad'] 	= {command = 'edit_dotfiles', 	mode = {'n'}},
	},

	['utils'] = {
		['<leader>#'] 	= {command = 'openNotesFile', 	mode = {'n'}}
	},
}


-- apply defined keymapping
local map = vim.keymap.set
local options = {noremap = true}
for module, keymapping_table in pairs(keymapping) do
	for key, command in pairs(keymapping_table) do
		if module == 'vim' then
			for _, mode in ipairs(command['mode']) do
				map(mode, key, function() vim.cmd(command['command']) end, options)
			end
		else
			for _, mode  in ipairs(command['mode']) do
				map(mode, key, require(module)[command['command']], options)
			end
		end
	end
end
