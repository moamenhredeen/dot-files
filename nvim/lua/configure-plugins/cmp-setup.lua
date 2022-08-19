local luasnip = require("luasnip")
local lspkind = require('lspkind')
local cmp = require('cmp')


local has_words_before = function()
	local line, col = unpack(vim.api.nvim_win_get_cursor(0))
	return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

cmp.setup({
	sources = {
		-- you can specifiy keyword_length
		{ name = 'nvim_lsp' },
		{ name = 'path' },
		{ name = 'buffer' },
		{ name = 'luasnip' },
		{ name = 'latex_symbols'},
		{ name = 'nvim_lua' },
	},

	snippet = {
		expand = function(args)
			require 'luasnip'.lsp_expand(args.body)
		end
	},

	view = {
		entries = 'custom'
	},


	mapping = {
		["<Tab>"] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_next_item()
			elseif luasnip.expand_or_jumpable() then
				luasnip.expand_or_jump()
			elseif has_words_before() then
				cmp.complete()
			else
				fallback()
			end
		end, { "i", "s" }),
		["<S-Tab>"] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_prev_item()
			elseif luasnip.jumpable(-1) then
				luasnip.jump(-1)
			else
				fallback()
			end
		end, { "i", "s" }),
		['<C-b>'] = cmp.mapping.scroll_docs(-4),
		['<C-f>'] = cmp.mapping.scroll_docs(4),
		['<C-Space>'] = cmp.mapping.complete(),
		['<C-e>'] = cmp.mapping.abort(),
		-- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
		['<CR>'] = cmp.mapping.confirm({ select = true }),
	},

	formatting = {
		format = lspkind.cmp_format({
			mode = "symbol_text",
			menu = ({
				buffer = "[Buffer]",
				nvim_lsp = "[LSP]",
				luasnip = "[LuaSnip]",
				nvim_lua = "[Lua]",
				latex_symbols = "[Latex]",
			})
		}),
	},
})


-- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline('/', {
	mapping = cmp.mapping.preset.cmdline(),
	view = {
		entries = { name = 'wildmenu', separator = '|' }
	},
	sources = {
		{ name = 'buffer' }
	}
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(':', {
	mapping = cmp.mapping.preset.cmdline(),
	sources = cmp.config.sources({
		{ name = 'path' }
	}, {
		{ name = 'cmdline' }
	})
})


-- Setup lspconfig.
local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
local lspconfig = require('lspconfig')
lspconfig['rust_analyzer'].setup {
	capabilities = capabilities
}


lspconfig['sumneko_lua'].setup {
	capabilities = capabilities
}
