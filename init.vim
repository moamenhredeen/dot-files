
call plug#begin('~/.local/share/nvim/plugged')

Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'jiangmiao/auto-pairs'
Plug 'preservim/nerdcommenter'
Plug 'sbdchd/neoformat'
Plug 'neomake/neomake'
Plug 'terryma/vim-multiple-cursors'
Plug 'machakann/vim-highlightedyank'
Plug 'tmhedberg/SimpylFold'
Plug 'morhetz/gruvbox'
Plug 'Olical/conjure', {'tag': 'v4.23.0'}
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'
Plug 'yamatsum/nvim-cursorline'
Plug 'lervag/vimtex'
Plug 'Shougo/denite.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'mcchrish/nnn.vim'

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

" simply fold
let g:SimpylFold_docstring_preview = 1


" highlight yank
hi HighlightedyankRegion cterm=reverse gui=reverse
" set highlight duration time to 1000 ms, i.e., 1 second
let g:highlightedyank_highlight_duration = 1000


" Enable alignment
let g:neoformat_basic_format_align = 1

" Enable trimmming of trailing whitespace
let g:neoformat_basic_format_trim = 1

let g:shfmt_opt="-ci"

"*******************
" linter
let g:neomake_python_enabled_makers = ['pylint']
let g:neomake_cpp_enabled_makers = ['cpplint']
call neomake#configure#automake('nrwi', 500)


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

" open config file
nnoremap <leader>e :e ~/.config/nvim/init.vim<CR>

" reload config file
nnoremap <leader>r :source ~/.config/nvim/init.vim<CR>

" format file
nnoremap <leader>f :Neoformat<CR>

" tab -> select of auto completion list
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

" launch programm using make 
nnoremap <leader>l :w<CR>:make<CR>


" Use <C-l> for trigger snippet expand.
imap <C-l> <Plug>(coc-snippets-expand)

" Use <C-j> for select text for visual placeholder of snippet.
vmap <C-j> <Plug>(coc-snippets-select)

" Use <C-j> for jump to next placeholder, it's default of coc.nvim
let g:coc_snippet_next = '<c-j>'

" Use <C-k> for jump to previous placeholder, it's default of coc.nvim
let g:coc_snippet_prev = '<c-k>'



" === Denite shorcuts === "
"   ;         - Browser currently open buffers
"   <leader>t - Browse list of files in current directory
"   <leader>g - Search current directory for occurences of given term and close window if no results
"   <leader>j - Search current directory for occurences of word under cursor
nmap ; :Denite buffer<CR>
nmap <leader>t :DeniteProjectDir file/rec<CR>
nnoremap <leader>g :<C-u>Denite grep:. -no-empty<CR>
nnoremap <leader>j :<C-u>DeniteCursorWord grep:.<CR>

" Define mappings while in 'filter' mode
"   <C-o>         - Switch to normal mode inside of search results
"   <Esc>         - Exit denite window in any mode
"   <CR>          - Open currently selected file in any mode
"   <C-t>         - Open currently selected file in a new tab
"   <C-v>         - Open currently selected file a vertical split
"   <C-h>         - Open currently selected file in a horizontal split
autocmd FileType denite-filter call s:denite_filter_my_settings()
function! s:denite_filter_my_settings() abort
  imap <silent><buffer> <C-o>
  \ <Plug>(denite_filter_update)
  inoremap <silent><buffer><expr> <Esc>
  \ denite#do_map('quit')
  nnoremap <silent><buffer><expr> <Esc>
  \ denite#do_map('quit')
  inoremap <silent><buffer><expr> <CR>
  \ denite#do_map('do_action')
  inoremap <silent><buffer><expr> <C-t>
  \ denite#do_map('do_action', 'tabopen')
  inoremap <silent><buffer><expr> <C-v>
  \ denite#do_map('do_action', 'vsplit')
  inoremap <silent><buffer><expr> <C-h>
  \ denite#do_map('do_action', 'split')
endfunction

" Define mappings while in denite window
"   <CR>        - Opens currently selected file
"   q or <Esc>  - Quit Denite window
"   d           - Delete currenly selected file
"   p           - Preview currently selected file
"   <C-o> or i  - Switch to insert mode inside of filter prompt
"   <C-t>       - Open currently selected file in a new tab
"   <C-v>       - Open currently selected file a vertical split
"   <C-h>       - Open currently selected file in a horizontal split
autocmd FileType denite call s:denite_my_settings()
function! s:denite_my_settings() abort
  nnoremap <silent><buffer><expr> <CR>
  \ denite#do_map('do_action')
  nnoremap <silent><buffer><expr> q
  \ denite#do_map('quit')
  nnoremap <silent><buffer><expr> <Esc>
  \ denite#do_map('quit')
  nnoremap <silent><buffer><expr> d
  \ denite#do_map('do_action', 'delete')
  nnoremap <silent><buffer><expr> p
  \ denite#do_map('do_action', 'preview')
  nnoremap <silent><buffer><expr> i
  \ denite#do_map('open_filter_buffer')
  nnoremap <silent><buffer><expr> <C-o>
  \ denite#do_map('open_filter_buffer')
  nnoremap <silent><buffer><expr> <C-t>
  \ denite#do_map('do_action', 'tabopen')
  nnoremap <silent><buffer><expr> <C-v>
  \ denite#do_map('do_action', 'vsplit')
  nnoremap <silent><buffer><expr> <C-h>
  \ denite#do_map('do_action', 'split')
endfunction

"""""""""""" nnn configuration"""""""""""""""""""""" 
" Disable default mappings
let g:nnn#set_default_mappings = 0

" Set custom mappings
nnoremap <silent> <leader>b :NnnPicker<CR>
autocmd FileType nnn tnoremap <buffer><silent> <C-l> <C-\><C-n><C-w>l
let g:nnn#layout = { 'window': { 'width': 0.4, 'height': 0.5, 'highlight': 'Comment' } }
