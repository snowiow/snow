packadd vimtex

call vimtex#init()
nnoremap <localleader>f :VimtexCompile<CR>
setlocal spell
setlocal spelllang=de
setlocal colorcolumn=74
let b:undo_ftplugin .= '|setlocal spell<'
