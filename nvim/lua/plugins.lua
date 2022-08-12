-- This file can be loaded by calling `lua require('plugins')` from your init.vim

return require('packer').startup(function(use)
  	-- Packer can manage itself
	use 'wbthomason/packer.nvim'
	use 'neovim/nvim-lspconfig' -- Configurations for Nvim LSP
	use { 
		'nvim-telescope/telescope.nvim',
		tag = '0.1.0',
		requires = { 'nvim-lua/plenary.nvim' }
	}
	use 'L3MON4D3/LuaSnip'
	use 'onsails/lspkind.nvim'

	-- auto completionn plugins
	use 'hrsh7th/nvim-cmp'
	use 'hrsh7th/cmp-path'
	use 'hrsh7th/cmp-buffer'
	use 'hrsh7th/cmp-nvim-lua'
	use 'hrsh7th/cmp-nvim-lsp'
	use 'saadparwaiz1/cmp_luasnip'
	use 'kdheepak/cmp-latex-symbols'

	use {
    'numToStr/Comment.nvim',
    config = function()
        require('Comment').setup()
    end
	}

	use {
		"windwp/nvim-autopairs",
    config = function() require("nvim-autopairs").setup {} end
	}

	use {
		'nvim-lualine/lualine.nvim',
		requires = { 'kyazdani42/nvim-web-devicons', opt = true }
	}
end)