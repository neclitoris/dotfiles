so ~/.config/nvim/util.vim

setlocal iskeyword+=\\
setlocal iskeyword+=$
setlocal wrap
setlocal linebreak
setlocal wrapmargin=0
setlocal textwidth=0

inoremap <buffer> $ <Esc>a$<C-]>
inoremap <buffer> $$ <Esc>a$<C-]>
iabbrev <buffer> <silent> $ \(\)<Left><Left><C-R>=ExpandAbbrev('$')<CR>
iabbrev <buffer> <silent> $$ \[\]<Left><Left><C-R>=ExpandAbbrev('$')<CR>

inoremap <buffer> \( <Esc>a\(<C-]>
inoremap <buffer> \[ <Esc>a\(<C-]>
iabbrev <buffer> <silent> \( \(\)<Left><Left><C-R>=ExpandAbbrev('\')<CR>
iabbrev <buffer> <silent> \[ \[\]<Left><Left><C-R>=ExpandAbbrev('\')<CR>
