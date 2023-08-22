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


-- highlight current line
vim.o.cursorline = true

-- Set highlight on search
vim.o.hlsearch = false

-- Make line numbers default
vim.wo.number = true

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
-- vim.cmd [[colorscheme onedark]]

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

vim.o.wrap = false

vim.o.splitbelow = true
vim.o.splitright = true

vim.opt.clipboard = 'unnamedplus'

-- disable netrw at the very start of your init.lua (strongly advised)
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1


vim.g.mapleader = ' '
vim.g.maplocalleader = ' '



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
-- configure treesitter
--
local configure_treesitter = function() 
require('nvim-treesitter.configs').setup({
	ensure_installed = { "lua", "vim", "vimdoc" },
	auto_intall = true, 
	highlight =  {
		enable = true,
		additional_vim_regex_highlight = false
	}
})
end



-- ***********************************************************************
-- ***
-- *** auto completion
-- ***

local configure_cmp = function()
local cmp = require('cmp')
local luasnip = require('luasnip')

cmp.setup {
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
}

end


-- *************************************************
-- configure lsp config
--

local on_attach = function (_, bufnr)   
  -- NOTE: Remember that lua is a real programming language, and as such it is possible
  -- to define small helper and utility functions so you don't have to repeat yourself
  -- many times.
  --
  -- In this case, we create a function that lets us more easily define mappings specific
  -- for LSP related items. It sets the mode, buffer and description for us each time.
  local nmap = function(keys, func, desc)
    if desc then
      desc = 'LSP: ' .. desc
    end

    vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
  end

  nmap('<leader>a', vim.lsp.buf.code_action, '[A]ction')
	  nmap('<leader>rr', vim.lsp.buf.rename, '[R]efactor [R]ename')

	  nmap('gd', vim.lsp.buf.definition, '[G]oto [D]efinition')
	  -- nmap('gr', require('telescope.builtin').lsp_references, '[G]oto [R]eferences')
	  nmap('gi', vim.lsp.buf.implementation, '[G]oto [I]mplementation')

	  nmap('<leader>D', vim.lsp.buf.type_definition, 'Type [D]efinition')
	  --nmap('<leader>ds', require('telescope.builtin').lsp_document_symbols, '[D]ocument [S]ymbols')
	  --nmap('<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')

	  -- See `:help K` for why this keymap
	  nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
	  -- nmap('<C-k>', vim.lsp.buf.signature_help, 'Signature Documentation')

	  -- Lesser used LSP functionality
	  nmap('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
	  nmap('<leader>wa', vim.lsp.buf.add_workspace_folder, '[W]orkspace [A]dd Folder')
	  nmap('<leader>wr', vim.lsp.buf.remove_workspace_folder, '[W]orkspace [R]emove Folder')
	  nmap('<leader>wl', function()
	    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
	  end, '[W]orkspace [L]ist Folders')


	  -- Create a command `:Format` local to the LSP buffer
	  vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
	    vim.lsp.buf.format()
	  end, { desc = 'Format current buffer with LSP' })
end


local servers = {
  gopls = {},
  pyright = {},
  rust_analyzer = {},
  tsserver = {},

  lua_ls = {
    Lua = {
      runtime = {
        -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
        version = 'LuaJIT',
      },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = {'vim'},
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


local configure_lspconfig = function()
	-- Setup mason so it can manage external tooling
	require('mason').setup()

	-- Ensure the servers above are installed
	local mason_lspconfig = require('mason-lspconfig')

	local capabilities = vim.lsp.protocol.make_client_capabilities()
	capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

	mason_lspconfig.setup {
	  ensure_installed = vim.tbl_keys(servers),
	}

	mason_lspconfig.setup_handlers {
	  function(server_name)
	    require('lspconfig')[server_name].setup {
	      capabilities = capabilities,
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
	require("nvim-tree").setup {
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
	  filters = {
	    dotfiles = true,
	  },
	}

	vim.keymap.set('n', '<leader>e', require('nvim-tree.api').tree.find_file)
end


-- *************************************************
-- install plugisn and apply configuratio
--
require("lazy").setup({

	{
		"ellisonleao/gruvbox.nvim",
		priority = 1000,
		config = function ()
			vim.o.background = "dark"
			vim.cmd([[colorscheme gruvbox]])
		end
	},

	-- tree sitter plugin
	{
		"nvim-treesitter/nvim-treesitter",
		lazy = false,
		config = configure_treesitter
	},


	-- telescope plugin
	{
	    "nvim-telescope/telescope.nvim"
    	},


	-- auto completion
	{
	    'hrsh7th/nvim-cmp',
	    config = configure_cmp,
	    dependencies = {
	      'hrsh7th/cmp-path',
	      'hrsh7th/cmp-nvim-lsp',
	      'L3MON4D3/LuaSnip',
	      'saadparwaiz1/cmp_luasnip'
	    },
	},

	-- lsp-config plugin
	{
		"neovim/nvim-lspconfig", 
		dependencies = {
			"williamboman/mason.nvim",
			"williamboman/mason-lspconfig.nvim"
		},
		lazy = false,
		config = configure_lspconfig
	},

	-- gitsigns plugin
	{
		'lewis6991/gitsigns.nvim',
		lazy = false,
		config = configure_gitsigns
	},

	{
		"NeogitOrg/neogit",
		lazy = false,
		dependencies = {
		"nvim-lua/plenary.nvim",
		"sindrets/diffview.nvim",
		},
		config = true
	},

	{
		 'numToStr/Comment.nvim',
		 lazy = false
	},

	-- nvim tree
	{
		'nvim-tree/nvim-tree.lua',
		config = configure_nvimtree,
		lazy = true,
		dependencies = {
			'nvim-tree/nvim-web-devicons'
		}
	},

	-- which key plugin
	{
		"folke/which-key.nvim",
		lazy = true
	},
})



