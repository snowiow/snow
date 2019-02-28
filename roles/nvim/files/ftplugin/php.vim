inoremap ' ''<left>

set tabstop=4
set shiftwidth=4
set softtabstop=4 expandtab

set makeprg=php\ -ln\ %
set errorformat=%m\ in\ %f\ on\ line\ %l
    \,%-GErrors\ parsing\ %f
    \,%-G,%-GNo\ syntax\ errors\ detected\ in\ %f
