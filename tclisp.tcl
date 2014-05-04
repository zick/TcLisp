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
set cell_index 1
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
    global cell_index
    # Don't forget to push car/cdr when gcing.
    set car_list($cell_index) $a
    set cdr_list($cell_index) $d
    incr cell_index
    return [setData [setTag 0 1] [expr {$cell_index - 1}]]
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
        set ret [makeCons [makeSym "quote"] $ret]
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
        set tmp [read $str]
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
    if {$op == [makeSym "quote"]} then {
        return [car $args]
    } elseif {$op == [makeSym "if"]} then {
        lispush $args
        lispush $env
        set cond [eval1 [car $args] $env]
        lispop 2
        if {$cond == 0} then {
            return [eval1 [car [cdr [cdr $args]]] $env]
        }
        return [eval1 [car [cdr $args]] $env]
    } elseif {$op == [makeSym "lambda"]} then {
        return [makeExpr $args $env]
    } elseif {$op == [makeSym "defun"]} then {
        lispush $args
        set tmp [makeExpr [cdr $args] $env]
        addToEnv [car $args] $tmp $g_env
        lispop 1
        return [car $args]
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
        set bdy [cdr $bdy]
    }
    lispop 2
    return $ret
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
    if {$n == 0} then {
        return [car [car $args]]
    } elseif {$n == 1} then {
        return [cdr [car $args]]
    } elseif {$n == 2} then {
        return [makeCons [car $args] [car [cdr $args]]]
    }
    return [makeError "unknown subr"]
}

addToEnv [makeSym "car"] [makeSubr 0] $g_env
addToEnv [makeSym "cdr"] [makeSubr 1] $g_env
addToEnv [makeSym "cons"] [makeSubr 2] $g_env
addToEnv [makeSym "t"] [makeSym "t"] $g_env

puts -nonewline "> "
flush stdout
while {[gets stdin line] >= 0} {
    global sp
    set obj [lindex [read $line] 0]
    puts [printObj [eval1 $obj $g_env]]
    puts $sp
    puts -nonewline "> "
    flush stdout
}
