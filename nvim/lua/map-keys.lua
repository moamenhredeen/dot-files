vim.g.mapleader = ' '


-- define key mapping
local keymapping = {
	['vim'] = {
		['<leader>w'] 	= ':w',
		['<leader>x'] 	= ':x',
		['<leader>q'] 	= ':q!',
		['<C-j>'] 			= ':wincmd j',
		['<C-k>'] 			= ':wincmd k',
		['<C-h>'] 			= ':wincmd h',
		['<C-l>'] 			= ':wincmd l',
	 	['<leader>e'] 	= 'NvimTreeToggle',
	},

	['telescope.builtin'] = {
		['<leader>b'] 	= 'buffers',
		['<leader>f'] 	= 'git_files',
		['<leader>s'] 	= 'live_grep',
		['<leader>ab'] 	= 'git_branches',
		['<leader>ac'] 	= 'git_commits',
		['<leader>as'] 	= 'git_status',
		['<C-P>'] 			= 'commands',
		--['<C-H>'] 			= 'help_tags',
		['<C-N>'] 			= 'man_pages',
	},

	['configure-plugins.telescope-custom'] = {
		['<leader>ad'] 	= 'edit_dotfiles',
	},

	['utils'] = {
		['<leader>#'] 	= 'openNotesFile'
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
		 	map('n', key, require(module)[fn], options)
		end
	end
end

-- TODO refactor this 
-- terminal key mapping (register only when open the terminal (autocmd) )
function _G.set_terminal_keymaps()
  local opts = {buffer = 0}
  vim.keymap.set('t', '<esc>', [[<C-\><C-n>]], opts)
  vim.keymap.set('t', '<C-h>', [[<Cmd>wincmd h<CR>]], opts)
  vim.keymap.set('t', '<C-j>', [[<Cmd>wincmd j<CR>]], opts)
  vim.keymap.set('t', '<C-k>', [[<Cmd>wincmd k<CR>]], opts)
  vim.keymap.set('t', '<C-l>', [[<Cmd>wincmd l<CR>]], opts)
end

-- if you only want these mappings for toggle term use term://*toggleterm#* instead
vim.cmd('autocmd! TermOpen term://* lua set_terminal_keymaps()')

