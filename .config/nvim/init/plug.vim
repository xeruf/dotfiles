" Plug
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin()

" Editing
 " text objects
Plug 'xerus2000/argtextobj.vim' " arguments in brackets as text objects
Plug 'tpope/vim-surround'       " edit surroundings - cs, ds, ys
Plug 'bkad/CamelCaseMotion'     " move through camel case words
 " commands
Plug 'inkarkat/vim-ReplaceWithRegister' " gr to replace with register
Plug 'tpope/vim-commentary'             " gc to comment out (gcap for paragraph)
Plug 'terryma/vim-multiple-cursors'     " <C-n> new coursor+selection on match, <C-x> skip, <C-p> remove cursor & back
Plug 'junegunn/vim-easy-align'          " ga to align stuff
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)
 " libs
Plug 'tpope/vim-repeat'
Plug 'inkarkat/vim-ingo-library'

" Ex commands
Plug 'DataWraith/auto_mkdir'     " mkdir parent dirs when saving
Plug 'AndrewRadev/bufferize.vim' " Command to buffer
Plug 'tpope/vim-eunuch'          " OS helpers
Plug 'tpope/vim-fugitive'        " Git commands in vim
Plug 'jreybert/vimagit'          " Magit for vim - git stage individual changes
Plug 'doy/vim-diff-changes'      " Commands for diffing - :DiffAgainstFilesystem :DiffAgainstVCS :DiffStop
Plug 'junegunn/fzf'              " File selection with :FZF
 " Browse
Plug 'tyru/open-browser.vim' " :OpenBrowser, :OpenBrowserSearch
Plug 'tyru/open-browser-github.vim'

" QOL
Plug 'farmergreg/vim-lastplace' " Automatically jump to last edit position
Plug 'chrisbra/Recover.vim' " Add Recover options for swap files
Plug 'ntpeters/vim-better-whitespace' " Tools for trailing whitespace & mixed indents

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
Plug 'xerus2000/vim-dokuwiki'
Plug 'neoclide/coc.nvim', {'branch': 'release' }
Plug 'udalov/kotlin-vim'
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app & yarn install'  }
Plug 'glacambre/firenvim', { 'do': { _ -> firenvim#install(0) } }

call plug#end()

