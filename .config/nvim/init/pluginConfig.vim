command! PU PlugClean! | PlugUpdate | PlugUpgrade
" TODO Automatically do this in topgrade

" TODO auto-update on start/error

lua require 'colorizer'.setup({ '*'; markdown = { names = false; }; })

let g:strip_whitespace_on_save=1
autocmd FileType taskedit,crontab,markdown DisableStripWhitespaceOnSave
let g:show_spaces_that_precede_tabs=1
let g:better_whitespace_skip_empty_lines=1
let g:better_whitespace_guicolor=$THEME == 'light' ? 'LightGrey' : 'DarkGrey'
" see https://github.com/ntpeters/vim-better-whitespace/issues/134 - autocmd OptionSet background let g:better_whitespace_guicolor=&background == 'light' ? 'LightYellow' : 'Brown'

" remap movement commands to respect CamelCase
map <silent> w <Plug>CamelCaseMotion_w
map <silent> b <Plug>CamelCaseMotion_b
map <silent> e <Plug>CamelCaseMotion_e
map <silent> ge <Plug>CamelCaseMotion_ge
omap <silent> iw <Plug>CamelCaseMotion_iw
xmap <silent> iw <Plug>CamelCaseMotion_iw
omap <silent> ib <Plug>CamelCaseMotion_ib
xmap <silent> ib <Plug>CamelCaseMotion_ib
omap <silent> ie <Plug>CamelCaseMotion_ie
xmap <silent> ie <Plug>CamelCaseMotion_ie

source $INITDIR/init/firenvim.vim
