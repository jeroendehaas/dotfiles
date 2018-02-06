" vim:foldmethod=marker
call plug#begin('~/.vim/plugged')
Plug 'rafi/awesome-vim-colorschemes'
Plug 'molok/vim-vombato-colorscheme'
Plug 'ntpeters/vim-better-whitespace'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'dhruvasagar/vim-table-mode'
Plug 'neomake/neomake'
Plug 'kana/vim-operator-user'
Plug 'rhysd/vim-clang-format'
Plug 'dracula/vim'
call plug#end()

colorscheme github
let g:airline_theme='cobalt2'
" Sensible defaults
set hidden
set et ts=4 sts=4 sw=4

" Sensible key bindings {{{1
let mapleader=' '
nnoremap <silent> <leader><leader> :w<CR>
nnoremap <silent> <leader>qw :wq<CR>
nnoremap <silent> <leader>qq :q<CR>
nnoremap <silent> <leader>qf :q!<CR>
nnoremap <silent> <leader>rce :tabe ~/.config/nvim/init.vim<CR>
nnoremap <silent> <leader>rcs :source ~/.config/nvim/init.vim<CR>
cmap <C-k> <Up>
cmap <C-j> <Down>
cmap <C-h> <Left>
cmap <C-l> <Right>
cmap <C-e> <End>
cmap <C-a> <Home>

"" Finding files {{{2
nnoremap <silent> <leader>ff :Files<CR>

"" Window management {{{2
nnoremap <silent> <leader>wo :only<CR>
nnoremap <silent> <leader>wc :close<CR>

"" Plugin installation
nnoremap <silent> <leader>pi :PlugInstall<CR>
nnoremap <silent> <leader>pu :PlugUpdate<CR>
nnoremap <silent> <leader>pU :PlugUpgrade<CR>

" Neomake {{{1
call neomake#configure#automake('nw', 500)
let g:neomake_open_list = 2

" Clang Format {{{1
map _ <Plug>(operator-clang-format)
let g:clang_format#code_style='llvm'
