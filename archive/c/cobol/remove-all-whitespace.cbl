identification division.
program-id. remove-all-whitespace.

data division.
working-storage section.
01  arg-count           pic 9(4) comp.
01  input-len           pic 9(4) comp.
01  i                   pic 9(4) comp.
01  out-ptr             pic 9(4) comp value 1.

01  input-area.
    05  input-string    pic x(500).
    05  in-char         redefines input-string 
                        pic x occurs 500 times.

01  output-area.
    05  output-string   pic x(500) value spaces.
    05  out-char        redefines output-string 
                        pic x occurs 500 times.

procedure division.
main.
    accept arg-count from argument-number
    if arg-count = 0
        display "Usage: please provide a string"
        stop run
    end-if

    accept input-string from argument-value
    if input-string = spaces
        display "Usage: please provide a string"
        stop run
    end-if

    inspect input-string replacing 
        all x"09" by space  *> \t 
        all x"0A" by space  *> \n
        all x"0D" by space. *> \r

    compute input-len = function stored-char-length(input-string)

    perform varying i from 1 by 1 until i > input-len
        if in-char(i) not = space
            move in-char(i) to out-char(out-ptr)
            add 1 to out-ptr
        end-if
    end-perform

    if out-ptr > 1
        subtract 1 from out-ptr
        display output-string(1:out-ptr)
    else
        stop run
    end-if

    goback.
