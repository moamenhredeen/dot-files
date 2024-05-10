-- ***********************************************************************
-- ***
-- *** My Person Neovim Config
-- *** using lazy package manager
-- ***
--
--  ███╗   ██╗███████╗ ██████╗ ██╗   ██╗██╗███╗   ███╗
--  ████╗  ██║██╔════╝██╔═══██╗██║   ██║██║████╗ ████║
--  ██╔██╗ ██║█████╗  ██║   ██║██║   ██║██║██╔████╔██║
--  ██║╚██╗██║██╔══╝  ██║   ██║╚██╗ ██╔╝██║██║╚██╔╝██║
--  ██║ ╚████║███████╗╚██████╔╝ ╚████╔╝ ██║██║ ╚═╝ ██║
--  ╚═╝  ╚═══╝╚══════╝ ╚═════╝   ╚═══╝  ╚═╝╚═╝     ╚═╝


-- ***********************************************************************
-- ***
-- *** better defaults
-- ***

-- better performance 
vim.loader.enable()

-- set font for gui neovim clients
vim.opt.guifont = 'FiraCode Nerd Font'

-- highlight current line
vim.o.cursorline = true

-- Set highlight on search
vim.o.hlsearch = false

-- Make line numbers default
vim.o.number = true

-- Enable mouse mode
vim.o.mouse = 'a'

-- Enable break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true

-- Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Decrease update time
vim.o.updatetime = 250
vim.wo.signcolumn = 'yes'

-- Set colorscheme
vim.o.termguicolors = true

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

vim.o.wrap = false
vim.o.splitbelow = true
vim.o.splitright = true
vim.opt.clipboard = 'unnamedplus'
vim.o.tabstop = 2
vim.o.shiftwidth = 2
-- vim.o.noexpandtab = true


-- change default shell
if vim.fn.has('linux') == 1 then
	vim.opt.shell='fish'
	vim.g.terminal_emulator='fish'
elseif vim.fn.has('win32') == 1 then
	vim.opt.shell='pwsh.exe -c '
	vim.g.terminal_emulator='pwsh.exe'
end


vim.g.mapleader = ' '
vim.g.maplocalleader = ','
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })


-- window navigation
vim.keymap.set('n', '<M-j>', '<C-w>j')
vim.keymap.set('n', '<M-k>', '<C-w>k')
vim.keymap.set('n', '<M-h>', '<C-w>h')
vim.keymap.set('n', '<M-l>', '<C-w>l')
vim.keymap.set('n', '<M-l>', '<C-w>l')
vim.keymap.set('n', '<M-q>', '<C-w>q')
vim.keymap.set('n', '<M-o>', '<C-w>o')
vim.keymap.set('n', '<M-s>', '<C-w>s')
vim.keymap.set('n', '<M-v>', '<C-w>v')
vim.keymap.set('n', '<M-r>', '<C-w>r')
vim.keymap.set('n', '<M-n>', ':windo wincmd H<CR>')
vim.keymap.set('n', '<M-m>', ':windo wincmd K<CR>')


-- *************************************************
-- bootstrap lazy.nvim
--
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)



-- *************************************************
-- configure theme
--
local configure_theme = function()
	vim.o.background = "dark"
	vim.cmd.colorscheme "vscode"
end



-- *************************************************
-- configure treesitter
--
local configure_treesitter = function()
	require('nvim-treesitter.configs').setup({
		ensure_installed = {
			"lua",
			"vim",
			"vimdoc",
			"go",
			"javascript",
			"typescript",
			"scss",
			"zig"
		},
		auto_intall = true,
		highlight = {
			enable = true,
			additional_vim_regex_highlight = false
		}
	})
end



-- *************************************************
-- telescope
-- See :help telescope and :help telescope.setup()

local configure_telescope = function()
	local telescope = require('telescope')
	telescope.setup {
		defaults = {
			layout_config = {
				bottom_pane = {
					height = 5,
					prompt_position = "top",
				},
			},
			mappings = {
				i = {
					['<C-u>'] = false,
					['<C-d>'] = false,
				},
			},
		},
	}

	local telescope_builtin = require('telescope.builtin')

	local fuzzy_find_buffer = function()
				telescope_builtin
					.current_buffer_fuzzy_find(require('telescope.themes')
					.get_dropdown { previewer = false })
	end

	local find_dot_file = function ()
		telescope_builtin.find_files({
			cwd = "~/git-repos/dot-files"
		})
	end

	-- Enable telescope fzf native, if installed
	pcall(telescope.load_extension, 'fzf')

	-- See `:help telescope.builtin`
	vim.keymap.set('n', '<Leader>f', telescope_builtin.find_files, { desc = 'open [F]ile' })
	vim.keymap.set('n', '<Leader>b', function ()
		 telescope_builtin.buffers({sort_lastused = true, only_cwd = true, ignore_current_buffer = true});
	end, { desc = 'open [B]uffer' })
	vim.keymap.set('n', '<Leader>x', telescope_builtin.commands, { desc = '[C]ommands' })
	vim.keymap.set('n', '<Leader>sh', telescope_builtin.help_tags, { desc = '[H]elp' })
	vim.keymap.set('n', '<Leader>sc', telescope_builtin.git_commits, { desc = '[C]ommands' })
	vim.keymap.set('n', '<Leader>sm', telescope_builtin.marks, { desc = '[S]earch [D]iagnostics' })
	vim.keymap.set('n', '<Leader>sq', telescope_builtin.quickfix, { desc = '[S]earch [D]iagnostics' })
	vim.keymap.set('n', '<Leader>sd', telescope_builtin.diagnostics, { desc = '[S]earch [D]iagnostics' })
	vim.keymap.set('n', '<Leader>sw', telescope_builtin.live_grep, { desc = '[S]earch by [G]rep' })

	vim.keymap.set('n', '<Leader>sb', fuzzy_find_buffer, { desc = '[/] Fuzzily search in current buffer]' })
	vim.keymap.set('n', '<Leader>d', find_dot_file, { desc = '[D]ot files' })
end


-- ***********************************************************************
-- ***
-- *** auto completion
-- ***

local configure_cmp = function()
	local cmp = require('cmp')
	local luasnip = require('luasnip')
	require("luasnip.loaders.from_vscode").lazy_load()

	cmp.setup({
		snippet = {
			expand = function(args)
				luasnip.lsp_expand(args.body)
			end,
		},
		mapping = cmp.mapping.preset.insert {
			['<C-d>'] = cmp.mapping.scroll_docs(-4),
			['<C-f>'] = cmp.mapping.scroll_docs(4),
			['<C-Space>'] = cmp.mapping.complete {},
			['<CR>'] = cmp.mapping.confirm {
				behavior = cmp.ConfirmBehavior.Replace,
				select = true,
			},
			['<Tab>'] = cmp.mapping(function(fallback)
				if cmp.visible() then
					cmp.select_next_item()
				elseif luasnip.expand_or_jumpable() then
					luasnip.expand_or_jump()
				else
					fallback()
				end
			end, { 'i', 's' }),
			['<S-Tab>'] = cmp.mapping(function(fallback)
				if cmp.visible() then
					cmp.select_prev_item()
				elseif luasnip.jumpable(-1) then
					luasnip.jump(-1)
				else
					fallback()
				end
			end, { 'i', 's' }),
		},
		sources = {
			{ name = 'nvim_lsp' },
			{ name = 'luasnip' },
			{ name = 'path' },
		},
	})
end


-- *************************************************
-- configure lsp config
--

local on_attach = function(_, bufnr)

	local nmap = function(keys, func, desc)
		if desc then
			desc = 'LSP: ' .. desc
		end
		vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
	end

	local telescope_built_ins = require('telescope.builtin')

	nmap('ga', vim.lsp.buf.code_action, '[A]ction')
	nmap('<Leader>a', vim.lsp.buf.code_action, '[A]ction')
	nmap('<Leader>rr', vim.lsp.buf.rename, '[R]efactor [R]ename')
	nmap('gd', vim.lsp.buf.definition, '[G]oto [D]efinition')
	nmap('gr', vim.lsp.buf.references, '[G]oto [R]eferences')
	nmap('gi', vim.lsp.buf.implementation, '[G]oto [I]mplementation')
	nmap('gs', telescope_built_ins.lsp_document_symbols, '[D]ocument [S]ymbols')
	nmap('gS', telescope_built_ins.lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')
	nmap('<Leader>t', telescope_built_ins.lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')
	nmap('K', vim.lsp.buf.hover, 'Hover Documentation')

	vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
		vim.lsp.buf.format()
	end, { desc = 'Format current buffer with LSP' })
end


local servers = {

	lua_ls = {
		Lua = {
			runtime = {
				-- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
				version = 'LuaJIT',
			},
			diagnostics = {
				-- Get the language server to recognize the `vim` global
				globals = { 'vim' },
			},
			workspace = {
				-- Make the server aware of Neovim runtime files
				library = vim.api.nvim_get_runtime_file("", true),
			},
			-- Do not send telemetry data containing a randomized but unique identifier
			telemetry = {
				enable = false,
			},
		},
	},
}


local capabilities = function ()
	local capabilities = vim.lsp.protocol.make_client_capabilities()
	return require('cmp_nvim_lsp').default_capabilities(capabilities)
end


local configure_lspconfig = function()
	-- Setup mason so it can manage external tooling
	require('mason').setup()

	-- Ensure the servers above are installed
	local mason_lspconfig = require('mason-lspconfig')


	mason_lspconfig.setup {
		ensure_installed = vim.tbl_keys(servers),
	}

	mason_lspconfig.setup_handlers {
		function(server_name)
			require('lspconfig')[server_name].setup {
				capabilities = capabilities(),
				on_attach = on_attach,
				settings = servers[server_name],
			}
		end,
	}
end;




-- *************************************************
-- configure gitsigns
--
local configure_gitsigns = function()
	require('gitsigns').setup {
		signs = {
			add          = { hl = 'GitSignsAdd', text = '█', numhl = 'GitSignsAddNr', linehl = 'GitSignsAddLn' },
			change       = { hl = 'GitSignsChange', text = '█', numhl = 'GitSignsChangeNr', linehl = 'GitSignsChangeLn' },
			delete       = { hl = 'GitSignsDelete', text = '█', numhl = 'GitSignsDeleteNr', linehl = 'GitSignsDeleteLn' },
			topdelete    = { hl = 'GitSignsDelete', text = '█', numhl = 'GitSignsDeleteNr', linehl = 'GitSignsDeleteLn' },
			changedelete = { hl = 'GitSignsChange', text = '█', numhl = 'GitSignsChangeNr', linehl = 'GitSignsChangeLn' },
			untracked    = { hl = 'GitSignsAdd', text = '█', numhl = 'GitSignsAddNr', linehl = 'GitSignsAddLn' },
		},
	}
end



-- ***********************************************************************
-- nvim tree
--
local configure_nvimtree = function()
	-- disable netrw at the very start of your init.lua (strongly advised)
	vim.g.loaded_netrw = 1
	vim.g.loaded_netrwPlugin = 1
	local api = require('nvim-tree.api')

	local my_on_attach = function(bufnr)
		api.config.mappings.default_on_attach(bufnr)
	end

	require("nvim-tree").setup {
		on_attach = my_on_attach,
		sort_by = "case_sensitive",
		view = {
			adaptive_size = true,
		},
		renderer = {
			group_empty = true,
			icons = {
				show = {
					file = true,
					folder = true
				}
			}
		},
		-- filters = {
		-- 	dotfiles = true,
		-- },
	}

	vim.keymap.set('n', '<Leader>e', function ()
		api.tree.open({
			find_file = true
		})
	end)
end




-- ***********************************************************************
-- configure comment
--

local configure_comment = function()
	require('Comment').setup {
		padding = true,
		sticky = true,
		ignore = '^$',
		toggler = {
			line = 'gcc',
			block = 'gbc'
		},
		extra = {
			---Add comment on the line above
			above = 'gck',
			---Add comment on the line below
			below = 'gcj',
			---Add comment at the end of line
			eol = 'gca',
		},
		---Enable keybindings
		--NOTE: If given `false` then the plugin won't create any mappings
		mappings = {
			---Operator-pending mapping; `gcc` `gbc` `gc[count]{motion}` `gb[count]{motion}`
			basic = true,
			---Extra mapping; `gco`, `gcO`, `gcA`
			extra = true,
		},
		---Function to call before (un)comment
		-- pre_hook = nil,
		---Function to call after (un)comment
		-- post_hook = nil
	}
end


-- ***********************************************************************
-- configure comment
--
local configure_overseer = function ()
	--TODO: configure shortcuts
	require("overseer").setup()
end



-- ***********************************************************************
-- lua line config
--
local configure_lualine = function ()
	require("lualine").setup()
end



-- ***********************************************************************
-- flutter tools config
--
local configure_flutter_tools = function ()
require("flutter-tools").setup {
  ui = {
		-- 'native' or 'plugin'
    notification_style = 'native'
  },
  flutter_path = "/home/moamen/tools/flutter/bin/flutter",
  root_patterns = { ".git", "pubspec.yaml" },
	widget_guides = {
    enabled = true,
  },
  closing_tags = {
    prefix = "//",
    enabled = true
  },
  lsp = {
		color = {
			enabled = true,
			background = true,
		},
    on_attach = on_attach,
    capabilities = capabilities(),
    settings = {
      showTodos = true,
      completeFunctionCalls = true,
      enableSnippets = true,
      updateImportsOnRename = true,
    }
  }
}
end


-- *************************************************
-- install plugisn and apply configuratio
--
require("lazy").setup({
	{ "Mofiqul/vscode.nvim", priority = 1000, config = true, opts = configure_theme },
	{ "nvim-telescope/telescope.nvim", config = configure_telescope, },
	{ "nvim-treesitter/nvim-treesitter", config = configure_treesitter, },
	{ "lewis6991/gitsigns.nvim", config = configure_gitsigns, },
	{ "numToStr/Comment.nvim", config = configure_comment, },
	{ "windwp/nvim-autopairs", config = true, },
	{ "j-hui/fidget.nvim", tag = "legacy", event = "LspAttach", config = true },
	{ 'stevearc/overseer.nvim', opts = {}, config = configure_overseer },
	{ "folke/neodev.nvim", config = true, event = "BufEnter init.lua" },
	{ "sindrets/diffview.nvim", config = true},
	{ "NeogitOrg/neogit", config = true,
		dependencies = { "nvim-lua/plenary.nvim", "sindrets/diffview.nvim" } },
	{ 'hrsh7th/nvim-cmp', config = configure_cmp,
		dependencies = { 'hrsh7th/cmp-path', 'hrsh7th/cmp-nvim-lsp', 'saadparwaiz1/cmp_luasnip',
		{'L3MON4D3/LuaSnip', dependencies= { "rafamadriz/friendly-snippets" }}} },
	{ "neovim/nvim-lspconfig", config = configure_lspconfig,
		dependencies = { "williamboman/mason.nvim", "williamboman/mason-lspconfig.nvim" } },
	{ 'nvim-tree/nvim-tree.lua', config = configure_nvimtree,
		dependencies = { 'nvim-tree/nvim-web-devicons' } },
	{ 'nvim-lualine/lualine.nvim', opts = {}, config = configure_lualine,
		dependencies = { 'nvim-tree/nvim-web-devicons' }},
	{ "folke/todo-comments.nvim", config = true,
		dependencies = { "nvim-lua/plenary.nvim" }},
	{ 'akinsho/flutter-tools.nvim', lazy = false, config = configure_flutter_tools,
			dependencies = { 'nvim-lua/plenary.nvim', 'stevearc/dressing.nvim'}},
})
