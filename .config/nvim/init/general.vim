set termguicolors
set mouse=a " Enable mouse in all modes

set undofile
set wildmode=longest,list,full " Auto-completion
set clipboard+=unnamedplus " Merge with system clipboard

set tabstop=4 shiftwidth=4 " Indentation

" visuals
set number relativenumber " Relative line numbering on the left
set wrap linebreak
set cursorline cursorcolumn
set scrolloff=7 sidescrolloff=20 " Keep some context when scrolling

set colorcolumn=81,121
highlight ColorColumn ctermbg=grey guibg=#888888
" call matchadd('ColorColumn', '\%81v', 100)

" search
set incsearch hlsearch " Handier search, even though nvim seems to enable it by default
set ignorecase smartcase " Only search case-sensitive when searching with uppercase
" matching
set showmatch " Highlight matching parenthesis
set matchpairs+=<:> " More pairings for '%'

set splitright " Vertical split to right by default
set diffopt+=vertical

" Folding
set foldmethod=marker " Use triple curly braces for folds
set foldopen+=jump " Automatically open folds on jump

" FILETYPES
syntax on
filetype plugin on
autocmd FileType json syntax match Comment +\/\/.\+$+ " Comment highlighting in JSON
autocmd Filetype * set formatoptions-=o " Don't automatically comment on o
" custom hardcoded types
autocmd BufRead,BufNewFile $SHELL_CONFIG/* set filetype=zsh
autocmd BufRead,BufNewFile www.mixxx.org_wiki* set filetype=dokuwiki scrolloff=1

set spelllang=en_us,de_de
autocmd FileType markdown setlocal spell " Spell checking in markdown

 " Center on insert mode
autocmd InsertEnter * norm zz
