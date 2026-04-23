identification division.
program-id. reverse-string.

data division.
working-storage section.
01  arg-count           pic 9(4) comp.
01  input-string        pic x(500).

procedure division.
main.
    accept arg-count from argument-number
    if arg-count = 0
        stop run
    end-if

    accept input-string from argument-value
    
    if input-string = spaces
        stop run
    end-if

    display function reverse(input-string)
    
    goback.
