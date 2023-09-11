" textarea.inputbox is for phpBB
let g:firenvim_config = {
  \ 'globalSettings': {
    \ 'alt': 'all',
  \  },
  \ 'localSettings': {
    \ '.*': {
      \ 'cmdline': 'firenvim',
      \ 'priority': 0,
      \ 'selector': 'textarea:not([readonly]):not([class="handsontableInput"]):not([wrap="off"]):not([rows="1"]):not([title="Replace"]):not([title="Search"]):not([name="message"]),
        \ div[role="textbox"]:not([aria-label="Search"]):not([class="ProseMirror"]), div[class="CodeMirror"]',
      \ 'takeover': 'always',
    \ },
    \ '.*com.*': {
      \ 'priority': 1,
      \ 'selector': 'textarea.inputbox',
      \ 'takeover': 'once',
    \ },
    \
    \ '.*wiki.*\.org.*':                     { 'priority': 9, 'takeover': 'never', },
    \ '://wiki\..*':                         { 'priority': 9, 'takeover': 'never', },
    \ '.*openstreetmap\.org.*':              { 'priority': 9, 'takeover': 'once', },
    \ '.*openstreetmap\.de.*':               { 'priority': 9, 'takeover': 'once', },
    \
    \ '.*mail.*':                            { 'priority': 9, 'takeover': 'once', },
    \ '.*church\.tools.*':                   { 'priority': 9, 'takeover': 'empty', },
    \ '.*element\.io.*':                     { 'priority': 9, 'takeover': 'never', },
    \
    \ '.*discord\.com.*':                    { 'priority': 9, 'takeover': 'never', },
    \ '://chat\..*':                         { 'priority': 9, 'takeover': 'never', },
    \ '.*twitter\.com.*':                    { 'priority': 9, 'takeover': 'never', },
    \
    \ '://pve.*':                            { 'priority': 9, 'takeover': 'never', },
    \ '://46.*':                             { 'priority': 9, 'takeover': 'never', },
    \ '.*:8006/.*':                          { 'priority': 9, 'takeover': 'never', },
    \
    \ '.*calendar\.google\.com.*':           { 'priority': 9, 'takeover': 'empty', },
    \ '.*docs\.google\.com.*':               { 'priority': 9, 'takeover': 'never', },
    \ '.*contacts\.google\.com.*':           { 'priority': 9, 'takeover': 'never', },
    \ '.*notion\.so.*':                      { 'priority': 9, 'takeover': 'never', },
    \ '.*cloud\.atomtoast\.xyz.*':           { 'priority': 9, 'takeover': 'never', },
    \ 'https://bigbluebutton.*':             { 'priority': 9, 'takeover': 'never', },
    \ 'https://dhall-lang.org.*':            { 'priority': 9, 'takeover': 'never', },
    \ 'https://aur.archlinux.org/account.*': { 'priority': 9, 'takeover': 'never', },
    \
    \ '.*stackexchange\.com.*':              { 'priority': 9, 'takeover': 'never', },
    \ '.*stackoverflow\.com.*':              { 'priority': 9, 'takeover': 'never', },
    \ '.*com/questions/[0-9]+/.*':           { 'priority': 9, 'takeover': 'never', },
    \ '.*com/posts/[0-9]+/.*':               { 'priority': 9, 'takeover': 'never', },
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
