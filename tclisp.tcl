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
            set str [string range $str 0 $i]
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
        return [list [makeError "noimpl"] ""]
    } elseif {[string equal $c ")"]} then {
        append msg "invalid syntax: " $str
        return [list [makeError $msg] ""]
    } elseif {[string equal $c "'"]} then {
        return [list [makeError "noimpl"] ""]
    }
    return [readAtom $str]
}

proc printObj {obj} {
    global sym_table
    set tag [getTag $obj]
    if {$tag == 0} then {
        return "nil"
    } elseif {$tag == 1} then {
        return "CONS"
    } elseif {$tag == 2} then {
        return [getNum $obj]
    } elseif {$tag == 3} then {
        return $sym_table([getData $obj])
    } elseif {$tag == 6} then {
        append ret "<error: " $sym_table([getData $obj]) ">"
        return $ret
    }
    return "<unknown>"
}

puts -nonewline "> "
flush stdout
while {[gets stdin line] >= 0} {
    set obj [lindex [read $line] 0]
    puts [printObj $obj]
    puts -nonewline "> "
    flush stdout
}
