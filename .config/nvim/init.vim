""
"_____  ______  _____ _______ . _     _  _____  _________
" | |  | |  \ \  | |    | |    | |   | |  | |  | | | | | \
" | |  | |  | |  | |    | |    \ \   / /  | |  | | | | | |
"_|_|_ |_|  |_| _|_|_   |_| []  \_\_/_/  _|_|_ |_| |_| |_|
""

" Plugins {{{
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin()

" Editing
 " text objects
Plug 'xerus2000/argtextobj.vim' " arguments in brackets as text objects
Plug 'bkad/CamelCaseMotion' "
Plug 'tpope/vim-surround' " edit surroundings - cs, ds, ys
 " commands
Plug 'inkarkat/vim-ReplaceWithRegister' " gr to replace with register
Plug 'tpope/vim-commentary' " gc to comment out (gcap for paragraph)
Plug 'terryma/vim-multiple-cursors' " <C-n> new coursor+selection on match, <C-x> skip, <C-p> remove cursor & back
 " libs
Plug 'tpope/vim-repeat'
Plug 'inkarkat/vim-ingo-library'

" Ex commands
Plug 'DataWraith/auto_mkdir' " mkdir parent dirs when saving
Plug 'AndrewRadev/bufferize.vim' " Command to buffer
Plug 'tpope/vim-eunuch' " OS helpers
Plug 'tpope/vim-fugitive' " Git commands in vim
Plug 'jreybert/vimagit' " Magit for vim - git stage individual changes
Plug 'doy/vim-diff-changes' " Commands for diffing - :DiffAgainstFilesystem :DiffAgainstVCS :DiffStop
Plug 'junegunn/fzf' " File selection with :FZF
 " Browse
Plug 'tyru/open-browser.vim' " :OpenBrowser, :OpenBrowserSearch
let g:openbrowser_default_search="duckduckgo"
Plug 'tyru/open-browser-github.vim'
let g:netrw_nogx = 1 " disable netrw's gx mapping.
nmap gs <Plug>(openbrowser-smart-search)
vmap gs <Plug>(openbrowser-smart-search)
nmap gx yi':OpenGithubProject <C-R>"<CR>
command! S OpenBrowserSmartSearch <C-R>"<CR>
command! GH OpenGithubProject <C-R>"<CR>

" QOL
Plug 'farmergreg/vim-lastplace' " Automatically jump to last edit position
Plug 'chrisbra/Recover.vim' " Add Recover options for swap files
 " Remove trailing whitespace on save
Plug 'ntpeters/vim-better-whitespace'
highlight ExtraWhitespace ctermbg=4
let g:strip_whitespace_on_save=1
let g:show_spaces_that_precede_tabs=1
let g:better_whitespace_skip_empty_lines=1
autocmd FileType taskedit if exists(":DisableWhitespace") | execute ":DisableWhitespace" | endif
autocmd BufRead $MYVIMRC DisableStripWhitespaceOnSave

" Aesthetic
Plug 'vim-airline/vim-airline' " Status bar on the bottom
Plug 'psliwka/vim-smoothie' " Smooth scrolling
Plug 'romainl/vim-cool' " Automatically stop highlighting search results when moving - https://stackoverflow.com/questions/657447/vim-clear-last-search-highlighting
Plug 'airblade/vim-gitgutter' " Git info on the left
Plug 'norcalli/nvim-colorizer.lua' " Highlights color literals
 " Visual indent guides
Plug 'nathanaelkane/vim-indent-guides'
let g:indent_guides_enable_on_vim_startup = 1

" Integrations
Plug 'neoclide/coc.nvim', {'branch': 'release' }
Plug 'udalov/kotlin-vim'
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app & yarn install'  }
Plug 'glacambre/firenvim', { 'do': { _ -> firenvim#install(0) } }

call plug#end()

" Colors
set termguicolors
lua require'colorizer'.setup()

command! PU PlugInstall | PlugUpdate | PlugUpgrade
" }}}

" Config {{{

" FILETYPES
syntax on
filetype plugin on
autocmd BufRead,BufNewFile $SHELL_CONFIG/* set filetype=zsh
autocmd FileType json syntax match Comment +\/\/.\+$+ " Comment highlighting in JSON
autocmd Filetype * set formatoptions-=o " Don't automatically comment on o

set number relativenumber " Relative line numbering on the left
set clipboard+=unnamedplus " Merge with system clipboard
set scrolloff=7 sidescrolloff=5 " Keep some context when scrolling
set tabstop=4 shiftwidth=4 " Indentation
set splitright " Vertical split to right by default
set cursorline cursorcolumn

set incsearch hlsearch " Handier search, even though nvim seems to enable it by default
set ignorecase smartcase " Only search case-sensitive when searching with uppercase

set wildmode=longest,list,full " Auto-completion
set mouse=a " Enable mouse in all modes

set showmatch " Highlight matching parenthesis
set mps+=<:> " More pairings for '%'

" Folding
set foldmethod=marker " Use triple curly braces for folds
set foldopen+=jump " Automatically open folds on jump

set noshowmode " Don't show current mode because airline already does and it inhibits echo in visual mode

set diffopt+=vertical
command! DiffSwap :diffsp % " Diff for swap - somewhat obsolete by 'chrisbra/Recover.vim'

set spelllang=en_us,de_de
autocmd FileType markdown setlocal spell " Spell checking in markdown

 " Center on insert mode
autocmd InsertEnter * norm zz

" }}}

" Mappings {{{

 " Allow saving of files as root when I forgot to use sudoedit
cnoremap w!! execute 'write !sudo tee % >/dev/null' <bar> edit!
command! WS write <bar> source $MYVIMRC
command! FormatJSON %!python -m json.tool

" Add shebang - https://www.reddit.com/r/vim/comments/4z7z7s/add_shebang_lines_to_your_vim_files_automatically/d6v7op8 and https://stackoverflow.com/a/52135425
inoreabbrev <expr> #!! "#!/usr/bin/env" . (empty(&filetype) ? '' : ' '.&filetype)
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

function! ShowPosition()
  return ":\<c-u>echo 'start=" . string(getpos("v")) . " end=" . string(getpos(".")) . "'\<cr>gv"
endfunction
vmap <expr> ;j ShowPosition()

function! MoveTest()
    exe 'silent! normal! ^va"'
    normal! :\<ESC>\<CR>
    echo getpos('.') getpos("'<") getpos("'>")
endfunction

" }}}
