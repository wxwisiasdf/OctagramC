define print_ast
    set $i = 0
    while $i < $argc
        eval "call cc_ast_print($arg%d)", $i
        call (int)printf("\n")
        set $i = $i + 1
    end
end
define print_ssa
    set $i = 0
    while $i < $argc
        eval "call cc_ssa_print_func($arg%d)", $i
        call (int)printf("\n")
        set $i = $i + 1
    end
end
define strview
    set $i = 0
    while $i < $argc
        eval "p cc_strview($arg%d)", $i
        set $i = $i + 1
    end
end
br cc_diag_error
