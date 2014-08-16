# <Object>
#   MSB              LSB
#   gc(1) data(28) tag(3)
#
# <Tag>
#   nil: 0
#   cons: 1
#   num: 2
#   sym: 3
#   subr: 4
#   expr: 5
#   error: 6

# Change recursionlimit if you need more stack.
# interp recursionlimit {} 10000

# Change cell_limit if you need more heap.
set cell_limit 65536

proc getTag {x} {
    return [expr {$x & 0x7}]
}

proc setTag {x tag} {
    return [expr {($x & ~0x7) | $tag}]
}

proc getData {x} {
    return [expr {($x & 0x7ffffff8) >> 3}]
}

proc setData {x data} {
    return [expr {($x & ~0x7ffffff8) | (($data << 3) & 0x7fffffff)}]
}

proc getGc {x} {
    return [expr {$x & 0x80000000}]
}

proc setGc {x gc} {
    return [expr {($x & ~0x80000000) | ($gc << 31)}]
}

set car_list(0) 0
set cdr_list(0) 0
set free_list 1
proc car {cell} {
    global car_list
    return $car_list([getData $cell])
}
proc cdr {cell} {
    global cdr_list
    return $cdr_list([getData $cell])
}
proc setCar {cell val} {
    global car_list
    set car_list([getData $cell]) $val
}
proc setCdr {cell val} {
    global cdr_list
    set cdr_list([getData $cell]) $val
}

proc makeCons {a d} {
    global car_list
    global cdr_list
    global free_list
    global cell_limit
    if {$free_list == $cell_limit} then {
        lispush $a
        lispush $d
        gc
        lispop 2
    }
    if {$free_list == [array size car_list]} then {
        set car_list($free_list) [expr $free_list + 1]
        set cdr_list($free_list) -1
    }
    set cell $free_list
    set free_list $car_list($free_list)
    set car_list($cell) $a
    set cdr_list($cell) $d
    return [setData [setTag 0 1] $cell]
}

set sp 0
set lstack(0) 0
proc lispush {obj} {
    global sp
    global lstack
    set lstack($sp) $obj
    incr sp
}
proc lispop {n} {
    global sp
    incr sp [expr {0 - $n}]
}

set g_env 0

proc gc {} {
    global free_list
    global cell_limit
    puts "GCing..."
    mark
    sweep
    if {$free_list == $cell_limit} then {
        append msg "MEMORY EXCEEDED (over " $cell_limit " cells)"
        puts $msg
        exit 1
    }
}

proc mark {} {
    global sp
    global lstack
    global g_env
    markObj $g_env
    set i 0
    while {$i < $sp} {
        markObj $lstack($i)
        incr i
    }
}

proc markObj {obj} {
    if {[getTag $obj] != 1 && [getTag $obj] != 5} then {
        return
    } elseif {[getGc [car $obj]]} {
        return
    }
    setCar $obj [setGc [car $obj] 1]
    markObj [car $obj]
    markObj [cdr $obj]
}

proc sweep {} {
    global car_list
    global cdr_list
    global free_list
    global cell_limit

    set free_list [array size car_list]
    set i 0
    set used 0
    while {$i < [array size car_list]} {
        if {[getGc $car_list($i)]} then {
            set car_list($i) [setGc $car_list($i) 0]
            incr used
        } else {
            set car_list($i) $free_list
            set free_list $i
            set cdr_list($i) -1
        }
        incr i
    }
    append msg "Used: " $used "\nAvailable: " [expr $cell_limit - $used]
    puts $msg
}

proc nreverse {lst} {
    set ret 0
    while {[getTag $lst] == 1} {
        set tmp [cdr $lst]
        setCdr $lst $ret
        set ret $lst
        set lst $tmp
    }
    return $ret
}

proc pairlis {lst1 lst2} {
    set ret 0
    lispush $lst1
    lispush $lst2
    while {[getTag $lst1] == 1 && [getTag $lst2] == 1} {
        lispush $ret
        set tmp [makeCons [car $lst1] [car $lst2]]
        lispush $tmp
        set ret [makeCons $tmp $ret]
        lispop 2
        set lst1 [cdr $lst1]
        set lst2 [cdr $lst2]
    }
    lispop 2
    return [nreverse $ret]
}

proc isSpace {str i} {
    set c [string index $str $i]
    return [expr {[string equal $c "\t"]
                  || [string equal $c "\n"]
                  || [string equal $c "\r"]
                  || [string equal $c " "]}]
}

proc isDelimiter {str i} {
    set c [string index $str $i]
    return [expr {[string equal $c "("]
                  || [string equal $c ")"]
                  || [string equal $c "'"]
                  || [isSpace $str $i]}]
}

proc skipSpaces {str} {
    set i 0
    while {$i < [string length $str]} {
        if {![isSpace $str $i]} {
            break
        }
        incr i
    }
    return [string range $str $i [string length $str]]
}

set sym_table(0) ""
proc makeSym {str} {
    global sym_table
    if {[string equal $str "nil"]} {
        return 0
    }
    set i 1
    while {$i < [array size sym_table]} {
        if {[string equal $str $sym_table($i)]} {
            return [setData [setTag 0 3] $i]
        }
        incr i
    }
    set sym_table($i) $str
    return [setData [setTag 0 3] $i]
}

set sym_t [makeSym "t"]
set sym_quote [makeSym "quote"]
set sym_if [makeSym "if"]
set sym_lambda [makeSym "lambda"]
set sym_defun [makeSym "defun"]
set sym_setq [makeSym "setq"]
set sym_loop [makeSym "loop"]
set sym_return [makeSym "return"]
set loop_val 0

proc makeError {str} {
    set ret [makeSym $str]
    return [setTag $ret 6]
}

proc makeNum {num} {
    return [setData [setTag 0 2] $num]
}

proc getNum {num} {
    set raw [getData $num]
    if {$raw & 0x08000000} then {
        set raw [expr $raw | ~0x0fffffff]
    }
    return $raw
}

proc makeSubr {n} {
    return [setData [setTag 0 4] $n]
}

proc makeExpr {args env} {
    set ret [makeCons $env $args]
    return [setTag $ret 5]
}

proc makeNumOrSym {str} {
    if {[string is integer $str]} then {
        return [makeNum $str]
    }
    return [makeSym $str]
}

proc readAtom {str} {
    set next ""
    set i 0
    while {$i < [string length $str]} {
        if {[isDelimiter $str $i]} {
            set next [string range $str $i [string length $str]]
            set str [string range $str 0 [expr {$i - 1}]]
            break
        }
        incr i
    }
    set obj [makeNumOrSym $str]
    return [list $obj $next]
}

proc read {str} {
    global sym_quote
    set kLPar "("
    set kRPar ")"
    set kQuote "'"
    set str [skipSpaces $str]
    if {[string length $str] == 0} then {
        return [list [makeError "empty input"] ""]
    }
    set c [string index $str 0]
    if {[string equal $c "("]} then {
        return [readList [string range $str 1 [string length $str]]]
    } elseif {[string equal $c ")"]} then {
        append msg "invalid syntax: " $str
        return [list [makeError $msg] ""]
    } elseif {[string equal $c "'"]} then {
        set tmp [read [string range $str 1 [string length $str]]]
        set elm [lindex $tmp 0]
        lispush $elm
        set ret [makeCons $elm 0]
        lispush $ret
        set ret [makeCons $sym_quote $ret]
        lispop 2
        return [list $ret [lindex $tmp 1]]
    }
    return [readAtom $str]
}

proc readList {str} {
    set ret 0
    while {1} {
        set str [skipSpaces $str]
        if {[string length $str] == 0} then {
            return [list [makeError "unfinished parenthesis"] ""]
        }
        set c [string index $str 0]
        if {[string equal $c "("]} then {  # for editor
        } elseif {[string equal $c ")"]} then {
            break
        }
        lispush $ret
        set tmp [read $str]
        lispop 1
        set elm [lindex $tmp 0]
        set next [lindex $tmp 1]
        if {[getTag $elm] == 6} then {
            return [list $elm ""]
        }
        lispush $ret
        set ret [makeCons $elm $ret]
        lispop 1
        set str $next
    }
    return [list [nreverse $ret] [string range $str 1 [string length $str]]]
}

proc printObj {obj} {
    global sym_table
    set tag [getTag $obj]
    if {$tag == 0} then {
        return "nil"
    } elseif {$tag == 1} then {
        return [printList $obj]
    } elseif {$tag == 2} then {
        return [getNum $obj]
    } elseif {$tag == 3} then {
        return $sym_table([getData $obj])
    } elseif {$tag == 4} then {
        return "<subr>"
    } elseif {$tag == 5} then {
        return "<expr>"
    } elseif {$tag == 6} then {
        append ret "<error: " $sym_table([getData $obj]) ">"
        return $ret
    }
    return "<unknown>"
}

proc printList {obj} {
    set ret ""
    set first 1
    while {[getTag $obj] == 1} {
        if {$first} then {
            set first 0
        } else {
            append ret " "
        }
        append ret [printObj [car $obj]]
        set obj [cdr $obj]
    }
    if {$obj == 0} then {
        append ret2 "(" $ret ")"
    } else {
        append ret2 "(" $ret " . " [printObj $obj] ")"
    }
    return $ret2
}

proc findVar {sym env} {
    while {[getTag $env] == 1} {
        set alist [car $env]
        while {[getTag $alist] == 1} {
            if {[car [car $alist]] == $sym} then {
                return [car $alist]
            }
            set alist [cdr $alist]
        }
        set env [cdr $env]
    }
    return 0
}

set g_env [makeCons 0 0]

proc addToEnv {sym val env} {
    lispush $sym
    lispush $val
    lispush $env
    set tmp [makeCons $sym $val]
    lispush $tmp
    set tmp [makeCons $tmp [car $env]]
    setCar $env $tmp
    lispop 4
}

proc eval1 {obj env} {
    global g_env
    global sym_quote
    global sym_if
    global sym_lambda
    global sym_defun
    global sym_setq
    global sym_loop
    global sym_return
    global loop_val
    set tag [getTag $obj]
    if {$tag == 0 || $tag == 2 || $tag == 6} then {
        return $obj
    } elseif {$tag == 3} then {
        set bind [findVar $obj $env]
        if {$bind == 0} then {
            append msg [printObj $obj] " has no value"
            return [makeError $msg]
        } else {
            return [cdr $bind]
        }
    }

    set op [car $obj]
    set args [cdr $obj]
    if {$op == $sym_quote} then {
        return [car $args]
    } elseif {$op == $sym_if} then {
        lispush $args
        lispush $env
        set cond [eval1 [car $args] $env]
        lispop 2
        if {[getTag $cond] == 6} then {
            return $cond
        } elseif {$cond == 0} then {
            return [eval1 [car [cdr [cdr $args]]] $env]
        }
        return [eval1 [car [cdr $args]] $env]
    } elseif {$op == $sym_lambda} then {
        return [makeExpr $args $env]
    } elseif {$op == $sym_defun} then {
        lispush $args
        set tmp [makeExpr [cdr $args] $env]
        addToEnv [car $args] $tmp $g_env
        lispop 1
        return [car $args]
    } elseif {$op == $sym_setq} then {
        lispush $args
        lispush $env
        set val [eval1 [car [cdr $args]] $env]
        lispop 2
        if {[getTag $val] == 6} then {
            return $val
        }
        set sym [car $args]
        set bind [findVar $sym $env]
        if {$bind == 0} then {
            addToEnv $sym $val $g_env
        } else {
            setCdr $bind $val
        }
        return $val
    } elseif {$op == $sym_loop} then {
        return [loop1 $args $env]
    } elseif {$op == $sym_return} then {
        set loop_val [eval1 [car $args] $env]
        return [makeError ""]
    }
    lispush $env
    lispush $args
    set op [eval1 $op $env]
    lispop 1
    lispush $op
    set args [evlis $args $env]
    lispop 2
    return [apply1 $op $args $env]
}

proc evlis {lst env} {
    set ret 0
    lispush $lst
    lispush $env
    while {[getTag $lst] == 1} {
        lispush $ret
        set elm [eval1 [car $lst] $env]
        lispush $elm
        set ret [makeCons $elm $ret]
        lispop 2
        set lst [cdr $lst]
    }
    lispop 2
    return [nreverse $ret]
}

proc progn {bdy env} {
    set ret 0
    lispush $bdy
    lispush $env
    while {[getTag $bdy] == 1} {
        set ret [eval1 [car $bdy] $env]
        if {[getTag $ret] == 6} then {
            break
        }
        set bdy [cdr $bdy]
    }
    lispop 2
    return $ret
}

proc loop1 {bdy env} {
    global sym_table
    global loop_val
    while {1} {
        set ret [progn $bdy $env]
        if {[getTag $ret] == 6} then {
            if {$sym_table([getData $ret]) == ""} then {
                return $loop_val
            } else {
                return $ret
            }
        }
    }
}

proc apply1 {fn args env} {
    if {[getTag $fn] == 6} then {
        return $fn
    } elseif {[getTag $args] == 6} then {
        return $args
    } elseif {[getTag $fn] == 4} then {
        return [subrCall [getData $fn] $args]
    } elseif {[getTag $fn] == 5} then {
        lispush $fn
        lispush $env
        set tmp [pairlis [car [cdr $fn]] $args]
        set tmp [makeCons $tmp [car $fn]]
        lispop 2
        return [progn [cdr [cdr $fn]] $tmp]
    }
    append msg [printObj $fn] " is not function"
    return [makeError $msg]
}

proc subrCall {n args} {
    global sym_t
    if {$n == 0} then {
        return [car [car $args]]
    } elseif {$n == 1} then {
        return [cdr [car $args]]
    } elseif {$n == 2} then {
        return [makeCons [car $args] [car [cdr $args]]]
    } elseif {$n == 3} then {
        if {[car $args] == [car [cdr $args]]} {
            return $sym_t
        }
        return 0
    } elseif {$n == 4} then {
        if {[getTag [car $args]] == 1} then {
            return 0
        }
        return $sym_t
    } elseif {$n == 5} then {
        if {[getTag [car $args]] == 2} then {
            return $sym_t
        }
        return 0
    } elseif {$n == 6} then {
        if {[getTag [car $args]] == 3} then {
            return $sym_t
        }
        return 0
    } else {
        return [arithCall [expr $n - 7] $args]
    }
    return [makeError "unknown subr"]
}

proc arithCall {n args} {
    if {[getTag [car $args]] != 2 || [getTag [car [cdr $args]]] != 2} then {
        return [makeError "wrong type"]
    }
    set x [getNum [car $args]]
    set y [getNum [car [cdr $args]]]
    if {$n == 0} then {
        return [makeNum [expr $x + $y]]
    } elseif {$n == 1} then {
        return [makeNum [expr $x * $y]]
    } elseif {$n == 2} then {
        return [makeNum [expr $x - $y]]
    } elseif {$n == 3} then {
        return [makeNum [expr $x / $y]]
    } elseif {$n == 4} then {
        return [makeNum [expr $x % $y]]
    }
    return [makeError "unknown subr"]
}

addToEnv [makeSym "car"] [makeSubr 0] $g_env
addToEnv [makeSym "cdr"] [makeSubr 1] $g_env
addToEnv [makeSym "cons"] [makeSubr 2] $g_env
addToEnv [makeSym "eq"] [makeSubr 3] $g_env
addToEnv [makeSym "atom"] [makeSubr 4] $g_env
addToEnv [makeSym "numberp"] [makeSubr 5] $g_env
addToEnv [makeSym "symbolp"] [makeSubr 6] $g_env
addToEnv [makeSym "+"] [makeSubr 7] $g_env
addToEnv [makeSym "*"] [makeSubr 8] $g_env
addToEnv [makeSym "-"] [makeSubr 9] $g_env
addToEnv [makeSym "/"] [makeSubr 10] $g_env
addToEnv [makeSym "mod"] [makeSubr 11] $g_env

puts -nonewline "> "
flush stdout
while {[gets stdin line] >= 0} {
    set obj [lindex [read $line] 0]
    puts [printObj [eval1 $obj $g_env]]
    puts -nonewline "> "
    flush stdout
}
