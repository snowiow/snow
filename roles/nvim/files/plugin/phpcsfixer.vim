function PhpCsFixer()
    execute "!php-cs-fixer fix --config=" . expand("~") . "/.php_cs %"
    e
endfunction
