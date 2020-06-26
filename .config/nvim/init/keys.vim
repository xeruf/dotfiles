" Save file as root when I forgot to use sudoedit
cnoremap w!! execute 'write !sudo tee % >/dev/null' <bar> edit!
" Write file & source vimrc (really only makes sense from within vimrc to quickly test changes)
command! WS write <bar> source $MYVIMRC

command! FormatJSON %!python -m json.tool
command! DiffSwap :diffsp % " Diff for swap - replaced by 'chrisbra/Recover.vim'

" shebang shortcut - https://www.reddit.com/r/vim/comments/4z7z7s/add_shebang_lines_to_your_vim_files_automatically/d6v7op8 and https://stackoverflow.com/a/52135425
inoreabbrev <expr> #!! "#!/usr/bin/env" . (empty(&filetype) ? '' : ' '.&filetype)
autocmd BufNewFile *.sh execute 'silent! 1s/.*/#!\/bin\/sh\r\r'| :startinsert
"autocmd BufNewFile * if !empty(&filetype) | execute 'silent! 1s/.*/#!\/usr\/bin\/' . &filetype . '\r\r'| :startinsert | endif

" Sane yanking
map Y y$
" Disable Ex mode - https://vi.stackexchange.com/q/457
nnoremap Q <nop>

let mapleader=" "

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
nnoremap gt gg
nnoremap gb G

 " Enable and disable auto comment
map <leader>c :setlocal formatoptions-=cro<CR>
map <leader>C :setlocal formatoptions=cro<CR>

map <leader>i :setlocal autoindent!<CR>
map <leader>s :setlocal spell!<CR>

" }}}
