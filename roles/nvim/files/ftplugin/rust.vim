packadd vim-racer

set makeprg=cargo\ check
set errorformat=%Eerror:\ %m
    \,%-Gerror:\ aborting\ due\ to\ previous\ error
    \,%-Gerror:\ could\ not\ compile%.%#
    \,%Eerror:\ %m
    \,%Wwarning:\ %m
    \,%C\ %#-->\ %f:%l:%c
    \,%-G%.%#
