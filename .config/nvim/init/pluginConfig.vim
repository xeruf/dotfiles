command! PU PlugUpdate | PlugUpgrade

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

" firenvim
let g:firenvim_config = {
    \ 'globalSettings': {
        \ 'alt': 'all',
    \  },
    \ 'localSettings': {
        \ '.*': {
            \ 'cmdline': 'firenvim',
            \ 'priority': 0,
            \ 'selector': 'textarea:not([readonly]):not([class="handsontableInput"]):not([wrap="off"]):not([rows="1"]),
                \ div[role="textbox"]:not([aria-label="Search"])',
            \ 'takeover': 'always',
        \ },
        \ '.*mail\.protonmail\.com.*':   { 'priority': 9, 'takeover': 'never', },
        \ '.*twitter\.com.*':            { 'priority': 9, 'takeover': 'never', },
        \ '.*openstreetmap\.org.*':      { 'priority': 9, 'takeover': 'never', },
        \ '.*wikipedia\.org.*':          { 'priority': 9, 'takeover': 'never', },
        \
        \ '.*notion\.so.*':              { 'priority': 9, 'takeover': 'never', },
        \ '.*docs\.google\.com.*':       { 'priority': 9, 'takeover': 'never', },
        \ '.*cloud\.atomtoast\.xyz.*':   { 'priority': 9, 'takeover': 'never', },
        \ 'https://bigbluebutton.*':     { 'priority': 9, 'takeover': 'never', },
        \ 'https://dhall-lang.org.*':    { 'priority': 9, 'takeover': 'never', },
        \
        \ '.*stackexchange\.com.*':      { 'priority': 9, 'takeover': 'never', },
        \ '.*stackoverflow\.com.*':      { 'priority': 9, 'takeover': 'never', },
        \ '.*com/questions/[0-9]{7}/.*': { 'priority': 9, 'takeover': 'never', },
    \ }
\ }
if exists('g:started_by_firenvim')
	nnoremap <Esc><Esc> :call firenvim#focus_page()<CR>

	autocmd FocusLost,InsertLeave,BufLeave * ++nested call WriteSilent()
	function WriteSilent()
		let was_enabled=b:strip_whitespace_on_save
		DisableStripWhitespaceOnSave
		write
		if was_enabled
			EnableStripWhitespaceOnSave
		endif
	endfunction
endif
