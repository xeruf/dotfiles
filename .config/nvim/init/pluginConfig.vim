" Don't show current mode because airline already does and it inhibits echo in visual mode
set noshowmode

command! PU PlugUpdate | PlugUpgrade

lua require 'colorizer'.setup({ '*'; markdown = { names = false; }; })

let g:strip_whitespace_on_save=1
let g:show_spaces_that_precede_tabs=1
let g:better_whitespace_skip_empty_lines=1
let g:better_whitespace_guicolor=$THEME == 'light' ? 'LightGrey' : 'DarkGrey'
autocmd FileType taskedit,markdown DisableWhitespace
" see https://github.com/ntpeters/vim-better-whitespace/issues/134 - autocmd OptionSet background let g:better_whitespace_guicolor=&background == 'light' ? 'LightYellow' : 'Brown'

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
            \ 'cmdline': 'neovim',
            \ 'priority': 0,
            \ 'selector': 'textarea:not([readonly]):not([class="handsontableInput"]), div[role="textbox"]',
            \ 'takeover': 'always',
        \ },
        \ '.*notion\.so.*': { 'priority': 1, 'takeover': 'never', },
        \ '.*openstreetmap\.org.*': { 'priority': 1, 'takeover': 'never', },
    \ }
\ }
if exists('g:started_by_firenvim')
	autocmd FocusLost,InsertLeave,BufLeave * ++nested call WriteSilent()
	function WriteSilent()
 		let tmp=b:better_whitespace_enabled
		let b:better_whitespace_enabled=0
		write
		let b:better_whitespace_enabled=tmp
	endfunction
	nnoremap <Esc><Esc> :call firenvim#focus_page()<CR>
	let g:strip_whitespace_on_save=0
endif
