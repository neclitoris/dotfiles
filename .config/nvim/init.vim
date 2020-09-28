" Don't try to be vi compatible
set nocompatible

" Helps force plugins to load correctly when it is turned back on below
filetype off

" Load plugins here (pathogen or vundle)

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
" Plugin 'Raimondi/delimitMate.git'
Plugin 'google/glaive'
Plugin 'Yggdroot/indentLine.git'
Plugin 'scrooloose/nerdcommenter.git'
Plugin 'scrooloose/nerdtree.git'
Plugin 'vim-airline/vim-airline.git'
Plugin 'vim-airline/vim-airline-themes.git'
Plugin 'edkolev/tmuxline.vim'
Plugin 'google/vim-codefmt.git'
" Plugin 'alx741/vim-hindent.git'
Plugin 'google/vim-maktaba'
Plugin 'mhinz/vim-startify.git'
Plugin 'ycm-core/YouCompleteMe'
Plugin 'lervag/vimtex'
" Plugin 'eagletmt/neco-ghc'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-repeat'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'derekwyatt/vim-fswitch'
" Plugin 'chrisbra/matchit'
Plugin 'altercation/vim-colors-solarized'
Plugin 'lyokha/vim-xkbswitch'
Plugin 'elzr/vim-json'
Plugin 'neovimhaskell/haskell-vim'
Plugin 'tpope/vim-obsession'

call vundle#end()

" Turn on syntax highlighting
syntax on

" For plugins to load correctly
filetype plugin indent on

" Startify
let g:startify_bookmarks = [
    \ { 'c': '~/.config/nvim/init.vim' },
    \ { 'f': '~/.clang-format' },
\ ]
let g:startify_custom_header =
    \ 'startify#center(startify#fortune#cowsay())'

" YCM
let g:ycm_use_clangd = 1
let g:ycm_clangd_binary_path = "/usr/bin/clangd"
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_global_ycm_extra_conf = "~/.ycm_extra_conf.py"
let g:ycm_extra_conf_globlist = ["~/doc/programming/*"]

" Hindent
" let g:hindent_indent_size=4

" Airline
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#keymap#enabled = 0
let g:airline#extensions#xkblayout#enabled = 0
let g:airline#extensions#tmuxline#enabled = 0

" DelimitMate
" let delimitMate_expand_cr = 1
" let delimitMate_expand_space = 1
" let delimitMate_balance_matchpairs = 1

" Nerdcommenter
let g:NERDSpaceDelims = 1
let g:NERDCustomDelimiters = {
    \ 'c': {'left': '//', 'leftAlt': '/**', 'rightAlt': '**/'},
    \ 'cpp': {'left': '//', 'leftAlt': '/**', 'rightAlt': '**/'}
\ }
let g:NERDCommentEmptyLines = 1

" IndentLine
let g:indentLine_char = '│'
let g:indentLine_first_char = '│'
let g:indentLine_showFirstIndentLevel = 1
let g:indentLine_fileTypeExclude = ['startify']
let g:indentLine_setConceal = 0

" vim-tmux-navigator
let g:tmux_navigator_no_mappings = 1
nnoremap <silent> <C-W>h :TmuxNavigateLeft<cr>
nnoremap <silent> <C-W>j :TmuxNavigateDown<cr>
nnoremap <silent> <C-W>k :TmuxNavigateUp<cr>
nnoremap <silent> <C-W>l :TmuxNavigateRight<cr>
nnoremap <silent> <C-W>p :TmuxNavigatePrevious<cr>

" vim-codefmt
augroup autoformat_settings
    autocmd FileType bzl AutoFormatBuffer buildifier
    autocmd FileType c,cpp,proto,javascript AutoFormatBuffer clang-format
    autocmd FileType dart AutoFormatBuffer dartfmt
    autocmd FileType go AutoFormatBuffer gofmt
    autocmd FileType gn AutoFormatBuffer gn autocmd FileType html,css,sass,scss,less,json AutoFormatBuffer js-beautify
    autocmd FileType java AutoFormatBuffer google-java-format
    autocmd FileType python AutoFormatBuffer yapf
    " Alternative: autocmd FileType python AutoFormatBuffer autopep8
    autocmd FileType vue AutoFormatBuffer prettier
augroup END
call glaive#Install()

if !exists('g:ycm_semantic_triggers')
    let g:ycm_semantic_triggers = {}
endif
if !exists('g:ycm_language_server')
    let g:ycm_language_server = []
endif

let g:ycm_semantic_triggers.tex = g:vimtex#re#youcompleteme
let g:vimtex_compiler_progname = 'nvr'
let g:vimtex_view_method = 'zathura'
let g:tex_conceal=''
let g:tex_flavor = 'latex'

let g:ycm_semantic_triggers.haskell = ['.']
" let g:necoghc_enable_detailed_browse = 0
let g:ycm_language_server += [{
            \   'name': 'haskell',
            \   'cmdline': ['hie-wrapper', '--lsp'],
            \   'filetypes': ['hs', 'haskell', 'lhs'],
            \   'project_root_files': ['stack.yaml', 'cabal.project', 'package.yaml']
            \}]

let g:XkbSwitchEnabled = 1

let g:solarized_termtrans = 1

let g:vim_json_syntax_conceal = 0

set foldlevel=0
set belloff=all

let mapleader = "'"
let maplocalleader = "'"

" Colorscheme
" syntax enable
colorscheme solarized
" highlight ExtraWhitespace cterm=bold ctermfg=236 ctermbg=233
" match ExtraWhitespace /\s\+$/
" autocmd BufEnter,WinEnter * match ExtraWhitespace /\s\+$/
set listchars=trail:·
set list

" Security
set modelines=0

" Show line numbers
set number
" set relativenumber

" Show file stats
set ruler

" Encoding
set encoding=utf-8

" Whitespace
set nowrap
set textwidth=120
set formatoptions=tcqrn1
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set noshiftround

" Cyrrilic in normal mode
" set keymap=russian-jcukenwin
" set iminsert=0
" set imsearch=0

" Cursor motion
set scrolloff=0
set backspace=indent,eol,start
set mouse=a
set conceallevel=1
set concealcursor="nc"

" Cursor style
" au VimEnter,InsertLeave * silent execute '!echo -ne "\e[4 q"' | redraw!
" au InsertEnter,InsertChange *
    " \ if v:insertmode == 'i' |
    " \   silent execute '!echo -ne "\e[5 q"' | redraw! |
    " \ elseif v:insertmode == 'r' |
    " \   silent execute '!echo -ne "\e[1 q"' | redraw! |
    " \ endif
" au VimLeave * silent execute '!echo -ne "\e[ q"' | redraw!

" Cursor settings:

" 1 -> blinking block
" 2 -> solid block
" 3 -> blinking underscore
" 4 -> solid underscore
" 5 -> blinking vertical bar
" 6 -> solid vertical bar

" Move up/down editor lines
nnoremap j gj
nnoremap k gk

set tildeop

" Allow hidden buffers
set hidden

" Rendering
set ttyfast

" Status bar
set laststatus=2

" Last line
set showmode
set showcmd

" Searching
set hlsearch
let @/ = ""
set incsearch
set ignorecase
set smartcase
set showmatch

" Remap help key.
" inoremap <F1> <ESC>:set invfullscreen<CR>a
" nnoremap <F1> :set invfullscreen<CR>
" vnoremap <F1> :set invfullscreen<CR>

" Textmate holdouts

" Formatting
" map <leader>q gqip

" useful bindings (I guess...)
tnoremap <Esc> <C-\><C-n>
nnoremap <leader>k i<Enter><Esc>k$
nnoremap <leader>j i<Enter><Esc>
nnoremap <leader>ev :edit $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>
nnoremap <leader>\ :NERDTreeFocus<cr>
nnoremap <leader>S :Startify<cr>
cnoremap <Left> <Space><BS><Left>
cnoremap <Right> <Space><BS><Right>
nnoremap <leader>lg :YcmCompleter GoTo<CR>
vnoremap <leader>lg :YcmCompleter GoTo<CR>
nnoremap <leader>lf :YcmCompleter Format<CR>
vnoremap <leader>lf :YcmCompleter Format<CR>

" TODO: figure out how to create "argument" textblock
function InnerArgument()
    let l:cur = matchstr(getline('.'), '\%' . col('.') . 'c.')
    while 1
        call search('\(,\|(\|{\|\[\|)\|}\|\]\)', 'b')
        let l:cur = matchstr(getline('.'), '\%' . col('.') . 'c.')
        if l:cur == ')' || l:cur == '}' || l:cur == ']'
            norm %
        else
            break
        endif
    endwhile
    call search('\S')
    let l:left = getpos('.')
    while 1
        call search('\(,\|(\|{\|\[\|)\|}\|\]\)')
        let l:cur = matchstr(getline('.'), '\%' . col('.') . 'c.')
        if l:cur == '(' || l:cur == '{' || l:cur == '['
            norm %
        else
            break
        endif
    endwhile
    call search('\S', 'b')
    norm v
    call setpos('.', l:left)
endfunction

function AnArgument()
    let l:cur = matchstr(getline('.'), '\%' . col('.') . 'c.')
    while 1
        call search('\(,\|(\|{\|\[\|)\|}\|\]\)', 'b')
        let l:cur = matchstr(getline('.'), '\%' . col('.') . 'c.')
        if l:cur == ')' || l:cur == '}' || l:cur == ']'
            norm %
        else
            if l:cur == ','
                call search('\S', 'b')
                let l:cut_left = 1
            else
                let l:cut_left = 0
            endif
            break
        endif
    endwhile
    norm l
    let l:left = getpos('.')
    while 1
        call search('\(,\|(\|{\|\[\|)\|}\|\]\)')
        let l:cur = matchstr(getline('.'), '\%' . col('.') . 'c.')
        if l:cur == '(' || l:cur == '{' || l:cur == '['
            norm %
        else
            if l:cur == ',' && !l:cut_left
                call search('\S')
            endif
            break
        endif
    endwhile
    norm hv
    call setpos('.', l:left)
endfunction

vnoremap ia :<c-u>call InnerArgument()<cr>
onoremap ia :normal via<cr>
vnoremap aa :<c-u>call AnArgument()<cr>
onoremap aa :normal vaa<cr>

function ExpandAbbrev(close)
    let l:c = nr2char(getchar())
    if l:c == ' ' || l:c == a:close
        return "\<Right>\<Right>"
    elseif l:c == ''
        return '	kA'
    endif
    return l:c
endfunction

augroup vimrc
    autocmd BufRead,BufWritePre,FileWritePre * silent! %s/[\r \t]\+$//
augroup END

set noswapfile
