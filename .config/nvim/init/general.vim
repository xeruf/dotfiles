set termguicolors
set mouse=nvh " Disable mouse in insert mode to allow middle-click paste in remote session - https://vi.stackexchange.com/q/23080
set clipboard+=unnamedplus " Merge with system clipboard

set shiftwidth=0 tabstop=3 noexpandtab " Default simple indentation
set wildmode=longest,list,full " Auto-completion for cmdline
set splitright diffopt+=vertical,filler

" matchpairs
set showmatch " Highlight matchpairs
set matchpairs+=<:> " More pairings for '%'

" visuals
set number relativenumber
set linebreak " Wrap at word boundaries
set scrolloff=7 sidescrolloff=20 " Keep some context when scrolling

set cursorline cursorcolumn
set colorcolumn=81,121
highlight ColorColumn ctermbg=grey guibg=#888888
" call matchadd('ColorColumn', '\%81v', 100)

" search
set incsearch hlsearch " Handier search, even though nvim seems to enable it by default
set ignorecase smartcase " Only search case-sensitive when searching with uppercase
set inccommand=split " Replace preview in separate window

" Folding
set foldmethod=marker " Use triple curly braces for folds
set foldopen+=jump " Automatically open folds on jump

" persist undo & backup files
set undofile
set backup
let &backupdir=expand(stdpath('data') . '/backup')
if !isdirectory(&backupdir)
  execute "!mkdir " . &backupdir
endif
let &backupext="_" . strftime("%y%m%dT%H%M")
set backupskip=

" FILETYPES
syntax on
filetype plugin on
" comments
autocmd Filetype * setlocal formatoptions-=o | setlocal formatoptions+=qjln12r " Change comment behavior
autocmd Filetype json syntax match Comment +\/\/.\+$+ " Comment highlighting in JSON
autocmd Filetype markdown let &comments = "b:*,b:-,b:+," . &comments
" custom hardcoded types
autocmd BufRead,BufNewFile $CONFIG_SHELLS/*,$CONFIG_ZSH/* setlocal filetype=bash
autocmd BufRead $XDG_CONFIG_HOME/yadm/bootstrap setlocal filetype=sh

" shebang shortcut - https://www.reddit.com/r/vim/comments/4z7z7s/add_shebang_lines_to_your_vim_files_automatically/d6v7op8 and https://stackoverflow.com/a/52135425
inoreabbrev <expr> #!! "#!/usr/bin/env" . (empty(&filetype) ? '' : ' '.&filetype)
autocmd BufNewFile *.sh,$HOME/.local/bin/* execute 'silent! 1s/.*/#!\/bin\/sh\r'| setlocal filetype=sh | :startinsert
"autocmd BufNewFile * if !empty(&filetype) | execute 'silent! 1s/.*/#!\/usr\/bin\/' . &filetype . '\r\r'| :startinsert | endif

set spelllang=en_us,de_de
autocmd BufEnter *.txt,*jrnl* setlocal shiftwidth=0 tabstop=4 expandtab " formatoptions+=b textwidth=78
autocmd FileType markdown setlocal wrap spell colorcolumn= shiftwidth=0 tabstop=4 expandtab " Spell checking & no guiding columns in markdown

 " Center on entering insert mode
autocmd InsertEnter * norm zz
