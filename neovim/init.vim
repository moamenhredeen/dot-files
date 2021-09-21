
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

call plug#end()



"***********************************************************
" General Settings

filetype plugin on
set number
set relativenumber
colorscheme gruvbox

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
nnoremap <leader>b :ls<CR>:b<space>

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
