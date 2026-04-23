identification division.
program-id. zeckendorf.

data division.
working-storage section.

01 input-str        pic x(32).
01 n                pic 9(18) comp.

01 a                pic 9(18) comp.
01 b                pic 9(18) comp.
01 c                pic 9(18) comp.
01 last-valid       pic 9(18) comp.

01 first-flag       pic 9 value 1.
   88 is-first      value 1.
   88 not-first     value 0.

01 display-num      pic ---------9.
01 separator        pic x(2) value ", ".

procedure division.

main.
    perform get-input
    perform solve
    goback.
    

get-input.
    accept input-str from argument-value

    if input-str = spaces
        perform show-usage
    end-if

    if function trim(input-str) not numeric
        perform show-usage
    end-if

    compute n = function numval(input-str)

    if n < 0
        perform show-usage
    end-if.


solve.
    if n = 0
        exit paragraph
    end-if

    set is-first to true

    perform until n = 0
        
        move 1 to a
        move 2 to b

        perform until b > n
            compute c = a + b
            move b to a
            move c to b
        end-perform

        move a to last-valid

        if not-first
            display separator with no advancing
        end-if

        move last-valid to display-num
        display function trim(display-num) with no advancing

        subtract last-valid from n
        set not-first to true

    end-perform

    display space.


show-usage.
    display "Usage: please input a non-negative integer"
    stop run.
