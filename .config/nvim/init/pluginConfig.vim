" Don't show current mode because airline already does and it inhibits echo in visual mode
set noshowmode

command! PU PlugUpdate | PlugUpgrade

lua require 'colorizer'.setup({ '*'; markdown = { names = false; }; })

let g:strip_whitespace_on_save=1
autocmd FileType taskedit,crontab,markdown DisableStripWhitespaceOnSave
let g:show_spaces_that_precede_tabs=1
let g:better_whitespace_skip_empty_lines=1
let g:better_whitespace_guicolor=$THEME == 'light' ? 'LightGrey' : 'DarkGrey'
" see https://github.com/ntpeters/vim-better-whitespace/issues/134 - autocmd OptionSet background let g:better_whitespace_guicolor=&background == 'light' ? 'LightYellow' : 'Brown'

let g:blamer_enabled = 1
let g:blamer_template = '<committer>: <summary> • <commit-short> <committer-time>'
let g:blamer_relative_time = 1
let g:blamer_delay = 300
" let g:blamer_show_in_visual_modes = 0

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

" open-browser
let g:openbrowser_default_search="duckduckgo"
let g:netrw_nogx = 1 " disable netrw's gx mapping.
nmap gs <Plug>(openbrowser-smart-search)
vmap gs <Plug>(openbrowser-smart-search)
nmap gx yi':OpenGithubProject <C-R>"<CR>
command! S OpenBrowserSmartSearch <C-R>"<CR>
command! GH OpenGithubProject <C-R>"<CR>

" firenvim
let g:firenvim_config = {
    \ 'globalSettings': {
        \ 'alt': 'all',
    \  },
    \ 'localSettings': {
        \ '.*': {
            \ 'cmdline': 'firenvim',
            \ 'priority': 0,
            \ 'selector': 'textarea:not([readonly]):not([class="handsontableInput"]):not([wrap="off"]), div[role="textbox"]:not([aria-label="Search"])',
            \ 'takeover': 'always',
        \ },
        \ '.*notion\.so.*': { 'priority': 9, 'takeover': 'never', },
        \ '.*docs\.google\.com.*': { 'priority': 9, 'takeover': 'never', },
        \ '.*mail\.protonmail\.com.*': { 'priority': 9, 'takeover': 'never', },
	\ '.*cloud.atomtoast.xyz.*': { 'priority': 9, 'takeover': 'never', },
	\ 'https://bigbluebutton.*': { 'priority': 9, 'takeover': 'never', },
	\ 'https://dhall-lang.org.*': { 'priority': 9, 'takeover': 'never', },
    \ }
\ }
if exists('g:started_by_firenvim')
	let g:smoothie_no_default_mappings=1 "extremely slow, see https://github.com/psliwka/vim-smoothie/issues/17

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
