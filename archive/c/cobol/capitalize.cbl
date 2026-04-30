identification division.
program-id. capitalize.

data division.
working-storage section.

01 arg-count        pic 9(4) comp.
01 input-text       pic x(4096).
01 c                pic x.
01 tmp-ord          pic 9(4) comp.

procedure division.

main.
    accept arg-count from argument-number

    if arg-count < 1
        perform show-usage
    end-if

    accept input-text from argument-value

    if input-text = spaces
        perform show-usage
    end-if

    move input-text(1:1) to c

    if c >= "a" and c <= "z"
        compute tmp-ord = function ord(c) - 32
        move function char(tmp-ord) to c
        move c to input-text(1:1)
    end-if

    display function trim(input-text)
    goback.

show-usage.
    display "Usage: please provide a string"
    stop run.
