identification division.
program-id. rot13.

data division.
working-storage section.

01 arg-count        pic 9(4) comp.
01 input-text       pic x(500).

01 translation-keys.
    05 alpha-lower   pic x(26) value "abcdefghijklmnopqrstuvwxyz".
    05 rot13-lower   pic x(26) value "nopqrstuvwxyzabcdefghijklm".
    05 alpha-upper   pic x(26) value "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
    05 rot13-upper   pic x(26) value "NOPQRSTUVWXYZABCDEFGHIJKLM".

procedure division.

main.
    accept arg-count from argument-number
    
    if arg-count = 0
        perform show-usage
    end-if

    accept input-text from argument-value
    
    if input-text = spaces
        perform show-usage
    end-if

    perform do-rot13
    goback.

do-rot13.
    inspect input-text 
        converting alpha-lower to rot13-lower
    
    inspect input-text 
        converting alpha-upper to rot13-upper
    
    display function trim(input-text).

show-usage.
    display "Usage: please provide a string to encrypt"
    stop run.
