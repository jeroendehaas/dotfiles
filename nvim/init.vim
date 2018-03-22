" vim:et:sts=2:sw=2:ts=8
call plug#begin('~/.local/share/nvim/plugged')
  Plug 'rhysd/vim-clang-format'
  Plug 'airblade/vim-gitgutter'
  Plug 'lervag/vimtex'
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
  Plug 'Shougo/neosnippet.vim'
  Plug 'Shougo/neosnippet-snippets'
  Plug 'tpope/vim-fugitive'
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'
  Plug 'neomake/neomake'
  Plug 'rust-lang/rust.vim'
  Plug 'racer-rust/vim-racer'
  Plug 'lifepillar/vim-solarized8'
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
  Plug 'junegunn/fzf.vim'
  Plug 'arcticicestudio/nord-vim'
  Plug 'dag/vim-fish'
call plug#end()

let g:deoplete#enable_at_startup = 1

let mapleader=' '
nnoremap <silent> <leader><leader> :w<CR>
nnoremap <silent> <leader>wq :wq<CR>
nnoremap <silent> <leader>qq :q<CR>
nnoremap <silent> <leader>qf :q!<CR>
nnoremap <silent> <leader>src :so $MYVIMRC<CR>
nnoremap <silent> <leader>erc :tabe $MYVIMRC<CR>
nnoremap <silent> <leader>pi :PlugInstall<CR>
nnoremap <silent> <leader>pu :PlugUpdate<CR>
nnoremap <silent> <leader>vs :vsplit<CR>
nnoremap <leader>jj :
nnoremap <leader>jf :e  <C-R>=expand('%:h')<CR>/
nnoremap <leader>jt :tabe  <C-R>=expand('%:h')<CR>/
nnoremap <leader>jv :vsplit <C-R>=expand('%:h')<CR>/
nnoremap <leader>jb :Buffers<CR>
nnoremap <leader>jg :GFiles<CR>
nnoremap <leader>jF :Files<CR>
nnoremap <leader>ef :e 
nnoremap <leader>et :tabe 
nnoremap <leader>ev :vsplit 
nnoremap <silent> <leader>hl :nohlsearch<CR> 
nnoremap <silent> <leader>lp :lprev<CR>
nnoremap <silent> <leader>ln :lnext<CR>
imap <C-j>     <Plug>(neosnippet_expand_or_jump)
smap <C-j>     <Plug>(neosnippet_expand_or_jump)
xmap <C-j>     <Plug>(neosnippet_expand_target)

set hidden
set et sts=4 ts=4 sw=4
colorscheme nord

" Rust {{{1
au FileType rust nmap gd <Plug>(rust-def)
au FileType rust nmap gs <Plug>(rust-def-split)
au FileType rust nmap gx <Plug>(rust-def-vertical)
au FileType rust nmap <leader>gd <Plug>(rust-doc)
let g:racer_cmd='/home/jeroen/.cargo/bin/racer'

" Git {{{1
nnoremap <silent> <leader>gs :Gstatus<CR>

" Neomake {{{1
call neomake#configure#automake('nw', 750)
let g:neomake_open_list = 2

" GitGutter {{{1
set updatetime=100
nnoremap <silent> <leader>sh :GitGutterStageHunk<CR>
nnoremap <silent> <leader>nh :GitGutterNextHunk<CR>
nnoremap <silent> <leader>ph :GitGutterPrevHunk<CR>

function! EnsureOnPath(dir)
  let dir = expand(a:dir)
  if isdirectory(l:dir) 
    for p in split($PATH, ":")
      if p ==# l:dir
        return
      endif
    endfor
    let $PATH .= ":" . l:dir
  endif
endfunction
call EnsureOnPath("~/.cargo/bin")
