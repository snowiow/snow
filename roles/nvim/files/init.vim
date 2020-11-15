"================================= pack ======================================"

packadd fzf.vim
packadd UltiSnips
packadd vim-commentary
packadd vim-gutentags
packadd LanguageClient-neovim
packadd vim-surround
packadd vim-unimpaired
packadd vim-jsonnet


"=== Neovim Settings ========================================================="
if has('nvim')
    hi! link TermCursor Cursor
    hi! TermCursorNC ctermfg=15 guifg=#fdf6e3 ctermbg=14 guibg=#93a1a1 cterm=NONE gui=NONE
endif

set guicursor=n-v-c:block-nCursor
  \,i-ci-ve:ver25-iCursor",r-cr:hor20-iCursor,o:hor50-iCursor
  \,sm:block-blinkwait175-blinkoff150-blinkon175

if (has("termguicolors") || has('nvim'))        "24 bit colorspace
    set termguicolors
endif

"=== VIM Design =============================================================="
filetype plugin indent on               "make vim improved
syntax on                               "set syntax highlighting on
set background=dark
set list                                "Show invisible chracters
set listchars=tab:\ \ ,trail:¬          "Which invisible characters should be
                                        "shown and how
set number                              "shows line numbers
set colorcolumn=80                      "Colorize the x'th column
set lazyredraw                          "update screen instead of full redraw
set noshowmode                          "Show which mode I am in in the message
                                        "line

augroup cursorline                      "Highlight the current line of the cursor
    autocmd!
    autocmd WinEnter * set cursorline
    autocmd WinLeave * set nocursorline
augroup END

colorscheme base16-tomorrow-night

"--- Statusline --------------------------------------------------------------"
function! MyGitStatus()
    let branch = fugitive#head()
    if branch == ''
        return ''
    endif
    return '%#SignifySignAdd# ' . branch . ' '
endfunction

function! MyMode()
    let mode = mode()
    if mode == 'n'
        return '%#Label# N '
    elseif mode == 'i'
        return '%#String# I '
    elseif mode == 't'
        return '%#String# T '
    elseif mode == 'v' || mode == 'V'
        return '%#Define# ' . mode . ' '
    elseif mode == 'R'
        return '%#ErrorMsg# ' . mode . ' '
    endif
    return '%#Operator#' . mode . ' '
endfunction

function! MyStatusLine(active)
    let activeHl = '%#StatusLineNC#'
    if a:active == 1
        let activeHl = '%#Normal#'
    endif
    let statusStr = ''
    let statusStr .= MyMode()
    let statusStr .= MyGitStatus()
    let statusStr .= activeHl
    let statusStr .= ' %t%m%r '
    let statusStr .= tagbar#currenttag('(%s)', '')
    let statusStr .= '%='
    let statusStr .= '%y '
    let statusStr .= activeHl
    let statusStr .= ' %{&fileencoding?&fileencoding:&encoding} '
    let statusStr .= '| %{&fileformat} '
    let statusStr .= '| %p%% '
    return statusStr
endfunction

set statusline=%!MyStatusLine(1)
augroup statusline
    autocmd!
    autocmd WinEnter * setlocal statusline=%!MyStatusLine(1)
    autocmd WinLeave * setlocal statusline=%!MyStatusLine(0)
augroup END

"------------------------------------Tabline----------------------------------"
function! MyTabLine()
    let tabStr = ''
    let curTabPage = tabpagenr()
    let tabPageCount = tabpagenr('$')
    let tabNumList = range(1, tabPageCount)
    for i in tabNumList
        if i == curTabPage
            let tabStr .= '%#Cursor#'
        else
            let tabStr .= '%#Operator#'
        endif
        let tabStr .= ' ' . i . ' '
    endfor
    let tabStr .= '%#Operator#'
    return tabStr
endfunction
set tabline=%!MyTabLine()
set showtabline=1

"=== Vim Behavior ============================================================"
set hidden                                      "allow hidden buffers to exist
set undofile                                    "save persistent undos to file
set encoding=utf8                               "Default encoding
set path+=**                                    "path is where vim is opened
set wildmenu                                    "activate wildmenu
set wildmode=list:longest,full                  "how wildmenu should act
set foldmethod=indent                           "Fold on indents
set mouse=a                                     "activate mouse in all modes
"set grep program
if executable('rg')                             "set rg as grep program
  set grepprg=rg\ --vimgrep\ --no-heading
  set grepformat^=%f:%l:%c:%m                   "file:line:column:message
endif
set wildignore+=*.so,*.swp,*.zip,*.lnk,*.aux,   "which filetype patterns should
               \*.class,*.aux,*.bbl,            "be ignored
               \*.fdb_latexmk,*.fls,*.pdf
                \*.hi, *.o
set nofoldenable                                "disable folding
set inccommand=nosplit                          "Live Preview substitutions in
                                                "the current buffer
set ignorecase                                  "ignore difference between
                                                "upper and lower case
set smartcase                                   "case sensitive when writing
                                                "upper case characters
set so=5                                        "show number of lines above or
                                                "under the cursor
set tags=./tags,tags                            "Sources for tag files
autocmd! bufwritepost init.vim source %         "Automatically reload vimrc
                                                "after save
set rnu                                         "relative numbers

set spelllang=de                                "Default spelling language
set thesaurus+=~/.config/nvim/spell/openthesaurus.txt
let g:tex_flavor='latex'                        "Starting with Vim 7, the
                                                "filetype of empty .tex files
                                                "defaults to plaintext instead
set breakindent                                 "also indent wrapped lines
set completeopt=menu,preview,noselect           "Complete Options:
                                                "menu: Always show completion menu
                                                "preview: show preview window
                                                "noselect: no automatic
                                                "selection of completion item
let g:tex_conceal = "abdmgs"                    "Don't show any conceals in latex
set shortmess+=c                                "Disable CompletionMessages at
                                                "the buttom

set switchbuf=usetab
if exists('$SUDO_USER')                         "Turn off temp files for sudo
    set nobackup
    set nowritebackup
    set noswapfile
    set noundofile
endif

"Automatically set matching bracket
ino " ""<left>
ino ( ()<left>
ino [ []<left>
ino { {}<left>
ino {<CR> {<CR>}<ESC>O
ino {;<CR> {<CR>};<ESC>O

"------------------------------Indentation------------------------------------"
set smartindent
set tabstop=4
set shiftwidth=4
set softtabstop=4 expandtab

"============================Keybindings======================================"
"get rid of ex mode for good
nnoremap Q <nop>
let mapleader = "\<Space>"
let maplocalleader = ','

" Still be able to revert last f or t motion
nnoremap ,, ,
"Make Y behave in a sane manner
nnoremap Y y$

"copy paste from clipboard
vnoremap <Leader>y "+y
nnoremap <Leader>yy "+yy
vnoremap <Leader>d "+d
nnoremap <Leader>p "+p
nnoremap <Leader>P "+P
vnoremap <Leader>p "+p
vnoremap <Leader>P "+P
"turn off search results
nnoremap <silent> <leader>r :nohlsearch<CR>

"open vimrc
nnoremap <leader>c :e ~/.config/nvim/init.vim<CR>

"close quickfix window
nnoremap <leader>q :cclose<CR>

" Open alternate file with c-space
nnoremap <c-space> <c-^>

"Search
" nnoremap <leader>o :find<space>
" nnoremap <leader>b :ls<cr>:buffer<space>
nnoremap <leader>7 :silent grep<space>
nnoremap <leader>8 :silent grep  %:h/*<left><left><left><left><left><left>
" Autocomplete
" Keyword
" Omni
inoremap <c-o> <c-x><c-o>
" Line
inoremap <c-l> <c-x><c-l>
" File
inoremap <c-f> <c-x><c-f>
" Include
inoremap <c-c> <c-x><c-i>
" Thesaurus
" Tags
inoremap <c-t> <c-x><c-]>
" Dictionary
inoremap <c-d> <c-x><c-d>

if (has("nvim"))
    tnoremap <C-n> <C-\><C-n>
endif
noremap <c-space> <c-^>

" unimpaired stuff
noremap [q :cprevious<CR>
noremap ]q :cnext<CR>
noremap [Q :cfirst<CR>
noremap ]Q :clast<CR>

noremap [b :bprevious<CR>
noremap ]b :bnext<CR>
noremap [B :bfirst<CR>
noremap ]B :blast<CR>

"===============================Autocommands=================================="

"-----------------------------------Make-------------------------------------"
augroup quickfix
    autocmd!
    autocmd QuickFixCmdPost l* lwindow
    autocmd QuickFixCmdPost [^l]* cwindow
augroup END

"----------------------------------Autocompletion-----------------------------"
augroup autocomplete
    autocmd!
    autocmd InsertEnter  * set noignorecase
    autocmd InsertLeave  * set ignorecase
    autocmd CompleteDone * pclose
augroup END

"------------------------------Opt Packackes----------------------------------"
augroup filetype_php
    autocmd!
    autocmd BufNewFile,BufRead .php_cs :set filetype=php syntax=php
    autocmd BufWritePost *.php silent! call PhpCsFixer()
    autocmd BufWritePost *.php silent lmake!
augroup END

augroup filetype_rust
    autocmd!
    " autocmd BufWritePost *.rs silent make!
    autocmd BufRead *.rs :setlocal tags=./tags,./rusty-tags.vi;/,$RUST_SRC_PATH/rusty-tags.vi
    autocmd BufWritePost *.rs :silent! exec "!rusty-tags vi --quiet --start-dir=" . expand('%:p:h') . "&" | redraw!
augroup END


augroup filetype_wiki
    autocmd!
    autocmd Filetype *.wiki setlocal spell
    autocmd Filetype *.wiki setlocal spelllang=de,en
augroup END

"=================================Plugins====================================="
"--- Elm-Vim -----------------------------------------------------------------"
let g:elm_setup_keybindings = 0
let g:elm_format_autosave = 1
let g:elm_detailed_complete = 1

"--- Intero-Neovim -----------------------------------------------------------"
augroup intero_mapping
    autocmd!
    autocmd FileType haskell nnoremap <localleader>d :InteroGoToDef<CR>
    autocmd FileType haskell nnoremap <localleader>t :InteroGenericType<CR>
    autocmd FileType haskell nnoremap <localleader>is :InteroOpen<CR>
    autocmd FileType haskell nnoremap <localleader>iv :InteroOpen<CR><C-W>H
    autocmd FileType haskell nnoremap <localleader>ih :InteroHide<CR>
augroup END

"--- Iron --------------------------------------------------------------------"
let g:iron_map_defaults=0
let g:iron_repl_open_cmd="vsplit"


nmap <leader>s <Plug>(iron-send-motion)
nnoremap <leader>is :SIronRepl<CR>
nnoremap <leader>ii :IronRepl<CR>
vmap <leader>s <Plug>(iron-send-motion)

function! MyHorizontalIron()
    let g:iron_repl_open_cmd="split"
    execute 'IronRepl'
    let g:iron_repl_open_cmd="vsplit"
endfunction
command! SIronRepl call MyHorizontalIron()

"---------------------------------Fugitive------------------------------------"
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gcc :Gcommit<CR>
nnoremap <leader>gcb :Git checkout -b 
nnoremap <leader>gcm :Git checkout master<CR>
nnoremap <leader>gco :Git checkout 
nnoremap <leader>gp :Gpush<CR>
nnoremap <leader>gd :Gdiff<CR>
nnoremap <leader>gbb :Gblame<CR>
nnoremap <leader>gbd :Git branch --delete 
nnoremap <leader>gw :Gbrowse<CR>
nnoremap <leader>gl :Gpull<CR>

"--------------------------------------FZF------------------------------------"
set rtp+=/usr/local/opt/fzf
nnoremap <leader>o :Files<cr>
nnoremap <leader>b :Buffers<cr>
nnoremap <leader>ft :Tags<cr>
nnoremap <leader>t :BTags<cr>
nnoremap <leader>f/ :History/<cr>
nnoremap <leader>f: :History:<cr>
nnoremap <leader>fw :Windows<cr>
nnoremap <leader>fh :Helptags<cr>
nnoremap <leader>fc :Colors<cr>
nnoremap ' :Marks<cr>

"-------------------------------------Gutentags-------------------------------"
let g:gutentags_resolve_symlinks=1
let g:gutentags_add_default_project_roots=1

"--- Haskell Vim -------------------------------------------------------------"
let g:haskell_indent_let = 2
let g:haskell_indent_in = 1

"--- Jedi Vim ----------------------------------------------------------------"
let g:jedi#completions_enabled=0
let g:jedi#documentation_command='<localleader>k'

"---------------------------Language Client-----------------------------------"
function! LC_maps()
  if has_key(g:LanguageClient_serverCommands, &filetype)
    nnoremap <buffer> <silent> K :call LanguageClient#textDocument_hover()<cr>
    nnoremap <buffer> <silent> gd :call LanguageClient#textDocument_definition()<CR>
    nnoremap <buffer> <silent> lr :call LanguageClient#textDocument_rename()<CR>
    nnoremap <buffer> <silent> lf :call LanguageClient#textDocument_formatting()<CR>
    nnoremap <buffer> <Leader>m :call LanguageClient_contextMenu()<CR>
    vnoremap <buffer> <Leader>m :call LanguageClient_contextMenu()<CR>
  endif
endfunction

autocmd! FileType python call LC_maps()
autocmd! FileType php call LC_maps()
autocmd! FileType go call LC_maps()
autocmd! FileType dart call LC_maps()

let g:LanguageClient_diagnosticsEnable=1
let g:LanguageClient_serverCommands = {
    \ 'dart': ['dart', '~/flutter/bin/cache/dart-sdk/bin/snapshots/analysis_server.dart.snapshot', '--lsp'],
    \ 'go': ['gopls'],
    \ 'php': ['php-language-server.php'],
    \ 'python': ['pyls'],
    \ 'typescript': ['typescript-language-server', '--stdio', '--tsserver-path', 'node_modules/.bin/tsserver'],
    \ }

let g:LanguageClient_rootMarkers = {
    \ 'dart': ['pubspec.yaml'],
    \ }
"--- Neco GHC ----------------------------------------------------------------"
let g:haskellmode_completion_ghc = 0
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

"-------------------------------Netrw-----------------------------------------"
let g:netrw_list_hide='.*\.swp$,
            \.*\.aux$,
            \.*\.bbl$,
            \.*\.fdb_latexmk$,
            \.*\.fls$,
            \.*\.blg,
            \.*\.o,
            \.*\.hi'

noremap <leader>e :Explore<CR>
augroup netrw_mapping
    autocmd!
    autocmd filetype netrw call NetrwMapping()
augroup END

function! NetrwMapping()
    map <buffer> l <CR>
    map <buffer> h -
endfunction

let g:netrw_banner=0
let g:netrw_liststyle=1
let g:netrw_localrmdir='rm -rf'

"-------------------------------------PHPActor--------------------------------"
augroup phpactor
    autocmd!
    autocmd FileType php setlocal omnifunc=phpactor#Complete
    autocmd Filetype php nnoremap <localleader>u :call phpactor#UseAdd()<CR>
    autocmd Filetype php nnoremap <localleader>m :call phpactor#ContextMenu()<CR>
    autocmd Filetype php nnoremap <localleader>d :call phpactor#GotoDefinition()<CR>
augroup END

"--- Vim Clang ---------------------------------------------------------------"
let g:clang_c_options = "-w"

"--------------------------------Vim Polyglot---------------------------------"
" let g:polyglot_disabled = ['elm']
let g:rustfmt_autosave = 1
let g:terraform_fmt_on_save = 1

"--- Vim Markdown Preview ----------------------------------------------------"
let vim_markdown_preview_hotkey='<M-l>'
let vim_markdown_preview_github=1
let vim_markdown_preview_pandoc=1
let vim_markdown_preview_use_xdg_open=1

"-------------------------------------Vim Racer-------------------------------"
let g:racer_experimental_completer = 1
augroup racer
    autocmd!
    autocmd FileType rust nmap <localleader>d <Plug>(rust-def)
    autocmd FileType rust nmap <localleader>K <Plug>(rust-doc)
augroup END

"---------------------------------Tagbar--------------------------------------"
nnoremap <leader>z :TagbarOpenAutoClose<CR>

"----------------------------UltiSnips----------------------------------------"
let g:UltiSnipsSnippetsDir="~/.local/share/nvim/site/UltiSnips"
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

"--- Vim-Go ------------------------------------------------------------------"
augroup vimgo
    autocmd!
    autocmd FileType go nmap <localleader>d :GoDef<cr>
augroup END

"--- Vim-Hindent -------------------------------------------------------------"
let g:hindent_on_save = 1

"--- Vimtex ------------------------------------------------------------------"
let g:vimtex_view_method = 'mupdf'

"===========================Custom Functions=================================="
"-----Function and Command for opening the current keyword in devdocs---"
let stub = "xdg-open 'https://devdocs.io/?q="
command! -nargs=* DD silent! call system(len(split(<q-args>, ' ')) == 0 ?
            \ stub . expand('<cword>') . "'" : len(split(<q-args>, ' ')) == 1 ?
\ stub . <q-args> . "'" : stub . <q-args> . "'")

set keywordprg=:DD "Set the keywordprg to be opened in devdocs

"=========================== Custom Commands ================================="
command! Bd :buffer #|bd #
command! Tt :tabedit|terminal
