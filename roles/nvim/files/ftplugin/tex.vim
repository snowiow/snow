packadd vimtex

call vimtex#init()
nnoremap <localleader>f :VimtexCompile<CR>
setlocal spell
setlocal spelllang=de

let b:undo_ftplugin = 'setlocal spell<'
