packadd vimtex

call vimtex#init()
nnoremap <localleader>f :VimtexCompile<CR>
setlocal tw=79
setlocal spell
setlocal spelllang=de

let b:undo_ftplugin = 'setlocal tw< spell< spelllang<'
