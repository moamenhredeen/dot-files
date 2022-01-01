
call plug#begin('~/.local/share/nvim/plugged')

Plug 'morhetz/gruvbox'
Plug 'vim-airline/vim-airline'
Plug 'machakann/vim-highlightedyank'
"Plug 'neoclide/coc.nvim', {'branch': 'release'}
"Plug 'Shougo/denite.nvim', { 'do': ':UpdateRemotePlugins' }
"Plug 'sbdchd/neoformat'
"Plug 'tmhedberg/SimpylFold'
"Plug 'lervag/vimtex'
"Plug 'vim-airline/vim-airline-themes'
"Plug 'jiangmiao/auto-pairs'
"Plug 'preservim/nerdcommenter'
"Plug 'neomake/neomake'
"Plug 'terryma/vim-multiple-cursors'
"Plug 'Olical/conjure', {'tag': 'v4.23.0'}
"Plug 'godlygeek/tabular'
"Plug 'plasticboy/vim-markdown'
"Plug 'yamatsum/nvim-cursorline'
"Plug 'mcchrish/nnn.vim'
"Plug 'liuchengxu/vim-which-key'
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-cmdline'
Plug 'hrsh7th/nvim-cmp'
Plug 'saadparwaiz1/cmp_luasnip'
Plug 'L3MON4D3/LuaSnip'

"Dart/Flutte
"Plug 'dart-lang/dart-vim-plugin'
"Plug 'thosakwe/vim-flutter'
"Plug 'natebosch/vim-lsc'
"Plug 'natebosch/vim-lsc-dart'



call plug#end()



"***********************************************************
" General Settings

filetype plugin on
set number
set relativenumber
colorscheme gruvbox 
" cursorline
set cursorline
hi cursorline guibg=234 ctermbg=234
set cursorcolumn
hi cursorcolumn guibg=234 ctermbg=234



" clipboard settings
set clipboard=unnamedplus
nnoremap d "dd
nnoremap D "dD
vnoremap d "dd
nnoremap c "cc
nnoremap C "cC
vnoremap c "cc



" highlight yank
hi HighlightedyankRegion cterm=reverse gui=reverse
" set highlight duration time to 1000 ms, i.e., 1 second
let g:highlightedyank_highlight_duration = 1000



"*******************
" key bindings

" leader key
let mapleader = " "

nnoremap <leader>x :x<CR>
nnoremap <leader>w :w<CR>
nnoremap <leader>q :q!<CR>

" map " to leader key
"nnoremap , " 

" split window
nnoremap <leader>v :vsplit<CR>
nnoremap <leader>s :split<CR>

" navigate between splits
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

" resize splits
nnoremap + :res +8<CR>
nnoremap - :res -8<CR>
nnoremap <leader>+ :vertical res +8<CR>
nnoremap <leader>- :vertical res -8<CR>

" list buffers
"nnoremap <leader>b :ls<CR>:b<space>

" deactivate highlight
nnoremap <leader>h :nohlsearch<CR>


" Plug shortcuts 
" p : settings 
nnoremap <leader>pi :PlugInstall<CR>
nnoremap <leader>pc :PlugClean<CR>
nnoremap <leader>pu :PlugUpdate<CR>
nnoremap <leader>pe :e ~/.config/nvim/init.vim<CR>
nnoremap <leader>pr :source ~/.config/nvim/init.vim<CR>

" tab -> select of auto completion list
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"


" ======================================================================
" add lsp configuration 
lua << EOF


-- Add additional capabilities supported by nvim-cmp
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)


local nvim_lsp = require('lspconfig')

-- Enable some language servers with the additional completion capabilities offered by nvim-cmp
local servers = { 'clangd', 'pyright', 'tsserver' }
for _, lsp in ipairs(servers) do
  nvim_lsp[lsp].setup {
    -- on_attach = my_custom_on_attach,
    capabilities = capabilities,
  }
end

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

-- luasnip setup
local luasnip = require 'luasnip'

-- nvim-cmp setup
local cmp = require 'cmp'
cmp.setup {
  snippet = {
    expand = function(args)
      require('luasnip').lsp_expand(args.body)
    end,
  },
  mapping = {
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-n>'] = cmp.mapping.select_next_item(),
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<CR>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ['<Tab>'] = function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end,
    ['<S-Tab>'] = function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end,
  },
  sources = {
    { name = 'nvim_lsp' },
    { name = 'luasnip' },
  },
}


-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  -- Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap=true, silent=true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '<space>e', '<cmd>lua vim.diagnostic.open_float()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<space>q', '<cmd>lua vim.diagnostic.setloclist()<CR>', opts)
  buf_set_keymap('n', '<space>f', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)

end

-- Use a loop to conveniently call 'setup' on multiple servers and
-- map buffer local keybindings when the language server attaches
local servers = { 'pyright', 'tsserver' }
for _, lsp in ipairs(servers) do
  nvim_lsp[lsp].setup {
    on_attach = on_attach,
    flags = {
      debounce_text_changes = 150,
    }
  }
end
EOF



" Use <C-j> for jump to next placeholder, it's default of coc.nvim
"let g:coc_snippet_next = '<c-j>'

" Use <C-k> for jump to previous placeholder, it's default of coc.nvim
"let g:coc_snippet_prev = '<c-k>'



" === Denite shorcuts === "
"   ;         - Browser currently open buffers
"   <leader>t - Browse list of files in current directory
"   <leader>g - Search current directory for occurences of given term and close window if no results
"   <leader>j - Search current directory for occurences of word under cursor
"nmap <leader>e :Denite buffer<CR>
"nmap <leader>t :DeniteProjectDir file/rec<CR>
"nnoremap <leader>g :<C-u>Denite grep:. -no-empty<CR>
"nnoremap <leader>j :<C-u>DeniteCursorWord grep:.<CR>

" Define mappings while in 'filter' mode
"   <C-o>         - Switch to normal mode inside of search results
"   <Esc>         - Exit denite window in any mode
"   <CR>          - Open currently selected file in any mode
"   <C-t>         - Open currently selected file in a new tab
"   <C-v>         - Open currently selected file a vertical split
"   <C-h>         - Open currently selected file in a horizontal split
"autocmd FileType denite-filter call s:denite_filter_my_settings()
"function! s:denite_filter_my_settings() abort
"  imap <silent><buffer> <C-o>
"  \ <Plug>(denite_filter_update)
"  inoremap <silent><buffer><expr> <Esc>
"  \ denite#do_map('quit')
"  nnoremap <silent><buffer><expr> <Esc>
"  \ denite#do_map('quit')
"  inoremap <silent><buffer><expr> <CR>
"  \ denite#do_map('do_action')
"  inoremap <silent><buffer><expr> <C-t>
"  \ denite#do_map('do_action', 'tabopen')
"  inoremap <silent><buffer><expr> <C-v>
"  \ denite#do_map('do_action', 'vsplit')
"  inoremap <silent><buffer><expr> <C-h>
"  \ denite#do_map('do_action', 'split')
"endfunction

" Define mappings while in denite window
"   <CR>        - Opens currently selected file
"   q or <Esc>  - Quit Denite window
"   d           - Delete currenly selected file
"   p           - Preview currently selected file
"   <C-o> or i  - Switch to insert mode inside of filter prompt
"   <C-t>       - Open currently selected file in a new tab
"   <C-v>       - Open currently selected file a vertical split
"   <C-h>       - Open currently selected file in a horizontal split
"autocmd FileType denite call s:denite_my_settings()
"function! s:denite_my_settings() abort
"  nnoremap <silent><buffer><expr> <CR>
"  \ denite#do_map('do_action')
"  nnoremap <silent><buffer><expr> q
"  \ denite#do_map('quit')
"  nnoremap <silent><buffer><expr> <Esc>
"  \ denite#do_map('quit')
"  nnoremap <silent><buffer><expr> d
"  \ denite#do_map('do_action', 'delete')
"  nnoremap <silent><buffer><expr> p
"  \ denite#do_map('do_action', 'preview')
"  nnoremap <silent><buffer><expr> i
"  \ denite#do_map('open_filter_buffer')
"  nnoremap <silent><buffer><expr> <C-o>
"  \ denite#do_map('open_filter_buffer')
"  nnoremap <silent><buffer><expr> <C-t>
"  \ denite#do_map('do_action', 'tabopen')
"  nnoremap <silent><buffer><expr> <C-v>
"  \ denite#do_map('do_action', 'vsplit')
"  nnoremap <silent><buffer><expr> <C-h>
"  \ denite#do_map('do_action', 'split')
"endfunction

"""""""""""" nnn configuration"""""""""""""""""""""" 
" Disable default mappings
"let g:nnn#set_default_mappings = 0

" Set custom mappings
"nnoremap <silent> <leader>b :NnnPicker<CR>
"autocmd FileType nnn tnoremap <buffer><silent> <C-l> <C-\><C-n><C-w>l
"let g:nnn#layout = { 'window': { 'width': 0.4, 'height': 0.5, 'highlight': 'Comment' } }



" configure whichkey package 
"nnoremap <silent> <leader> :WhichKey '<Space>'<CR>
"set timeoutlen=500

