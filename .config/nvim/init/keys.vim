let @c = '/^\ni```Ni```j0/```lllkxNA'

" Save file as root when I forgot to use sudoedit
cnoremap w!! execute 'write !sudo tee % >/dev/null' <bar> edit!
" Write file & source vimrc (really only makes sense from within vimrc to quickly test changes)
command! WS write <bar> source $MYVIMRC

command! FormatJSON %!python -m json.tool
command! DiffSwap :diffsp % " Diff for swap - replaced by 'chrisbra/Recover.vim'

" Sane yanking
map Y y$
" Disable Ex mode - https://vi.stackexchange.com/q/457
nnoremap Q @q

let mapleader=" "

nnoremap <leader>" ysiW"

 " Alias write and quit to leader
nnoremap <leader>q :wq<CR>
nnoremap <leader>w :w<CR>

 " Easy split navigation
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
 " Shortcut split opening
nnoremap <leader>h :split<Space>
nnoremap <leader>v :vsplit<Space>

 " Alias replace all to S
nnoremap S :%s//gI<Left><Left><Left>

 " Jump around without shift
nnoremap gl $
nnoremap gh 0
nnoremap gk H
nnoremap gj L
nnoremap gb G

 " Enable and disable auto comment
map <leader>c :setlocal formatoptions-=cro<CR>
map <leader>C :setlocal formatoptions=cro<CR>

map <leader>i :setlocal autoindent!<CR>
map <leader>s :setlocal spell!<CR>
map <leader>ls :setlocal colorcolumn=81,121 nowrap<CR>
map <leader>lw :setlocal colorcolumn=81 textwidth=80<CR>
map <leader>ln :setlocal colorcolumn= textwidth=0 wrap<CR>

" }}}
