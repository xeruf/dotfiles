" Plug
let plug_install = 0
let autoload_plug_path = stdpath('data') . '/site/autoload/plug.vim'
if !filereadable(autoload_plug_path)
	silent exe '!curl -fL --create-dirs -o ' . autoload_plug_path .
    	\ ' https://raw.github.com/junegunn/vim-plug/master/plug.vim'
    execute 'source ' . fnameescape(autoload_plug_path)
    let plug_install = 1
endif
unlet autoload_plug_path

call plug#begin()

" Editing
 " text objects
Plug 'xeruf/argtextobj.vim'             " arguments in brackets as text objects
Plug 'tpope/vim-surround'               " edit surroundings - cs, ds, ys
Plug 'bkad/CamelCaseMotion'             " move through camel case words
 " commands
Plug 'inkarkat/vim-ReplaceWithRegister' " gr to replace with register
Plug 'tpope/vim-commentary'             " gc to comment out (gcap for paragraph)
Plug 'junegunn/vim-easy-align'          " ga to align stuff
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)
 " libs
Plug 'tpope/vim-repeat'
Plug 'inkarkat/vim-ingo-library'

" Ex commands
Plug 'DataWraith/auto_mkdir'          " mkdir parent dirs when saving
Plug 'AndrewRadev/bufferize.vim'      " Command to buffer
Plug 'tpope/vim-eunuch'               " OS helpers
Plug 'doy/vim-diff-changes'           " Commands for diffing - :DiffAgainstFilesystem :DiffAgainstVCS :DiffStop

" QOL
Plug 'farmergreg/vim-lastplace'       " Automatically jump to last edit position
Plug 'chrisbra/Recover.vim'           " Add Recover options for swap files
Plug 'ntpeters/vim-better-whitespace' " Tools for trailing whitespace & mixed indents

" Aesthetic
Plug 'vim-airline/vim-airline'        " Status bar on the bottom
set noshowmode " Don't show current mode because airline already does and it inhibits echo in visual mode
Plug 'romainl/vim-cool'               " Automatically stop highlighting search results when moving - https://stackoverflow.com/questions/657447/vim-clear-last-search-highlighting
Plug 'airblade/vim-gitgutter'         " Git info on the left
 " Highlight color literals
Plug 'norcalli/nvim-colorizer.lua'
 " Visual indent guides
Plug 'nathanaelkane/vim-indent-guides'
let g:indent_guides_enable_on_vim_startup = 1

" Integrations
Plug 'glacambre/firenvim', { 'do': { _ -> firenvim#install(0) } }
Plug 'mipmip/vim-scimark'             " Edit markdown tables with sc-im

call plug#end()

" Install plugins automatically after installing plug
if plug_install
    PlugInstall --sync
endif
unlet plug_install
