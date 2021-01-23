set termguicolors
set mouse=nvh " Disable mouse in insert mode to allow middle-click paste in remote session - https://vi.stackexchange.com/q/23080

" persist undo & backup files
set undofile
set backup
let &backupdir=expand(stdpath('data') . '/backup')
if !isdirectory(&backupdir)
  execute "!mkdir " . &backupdir
endif
let &backupext="_" . strftime("%y%m%dT%H%M")

set wildmode=longest,list,full " Auto-completion for cmdline
set clipboard+=unnamedplus " Merge with system clipboard

set shiftwidth=2 expandtab " Indentation
"set shiftwidth=4 tabstop = 4 " Indentation for programming

" visuals
set number relativenumber " Relative line numbering on the left
set linebreak
set cursorline cursorcolumn
set scrolloff=7 sidescrolloff=20 " Keep some context when scrolling

set colorcolumn=81,121
highlight ColorColumn ctermbg=grey guibg=#888888
" call matchadd('ColorColumn', '\%81v', 100)

" search
set incsearch hlsearch " Handier search, even though nvim seems to enable it by default
set ignorecase smartcase " Only search case-sensitive when searching with uppercase
set inccommand=split
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
" comments
autocmd Filetype * setlocal formatoptions-=o | setlocal formatoptions+=qn12jr " Change comment behavior
autocmd Filetype json syntax match Comment +\/\/.\+$+ " Comment highlighting in JSON
autocmd Filetype markdown let &comments = "b:*,b:-,b:+," . &comments
" custom hardcoded types
autocmd BufRead,BufNewFile $CONFIG_SHELLS/*,$CONFIG_ZSH/* setlocal filetype=zsh
autocmd BufRead $XDG_CONFIG_HOME/yadm/bootstrap setlocal filetype=sh

" shebang shortcut - https://www.reddit.com/r/vim/comments/4z7z7s/add_shebang_lines_to_your_vim_files_automatically/d6v7op8 and https://stackoverflow.com/a/52135425
inoreabbrev <expr> #!! "#!/usr/bin/env" . (empty(&filetype) ? '' : ' '.&filetype)
autocmd BufNewFile *.sh,$HOME/.local/bin/* execute 'silent! 1s/.*/#!\/bin\/sh\r\r'| setlocal filetype=sh | :startinsert
"autocmd BufNewFile * if !empty(&filetype) | execute 'silent! 1s/.*/#!\/usr\/bin\/' . &filetype . '\r\r'| :startinsert | endif

set spelllang=en_us,de_de
autocmd BufEnter *.txt setlocal ts=4 sw=4 formatoptions+=t
autocmd BufEnter *jrnl*.txt setlocal formatoptions+=a
autocmd FileType markdown setlocal wrap spell colorcolumn= " Spell checking & no guiding columns in markdown

 " Center on insert mode
autocmd InsertEnter * norm zz
