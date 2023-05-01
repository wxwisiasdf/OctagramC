define print_ast
    set $i =0
    while $i < $argc
        eval "call cc_ast_print($arg%d)", $i
        call (int)printf("\n")
        set $i = $i + 1
    end
end
br cc_diag_error
