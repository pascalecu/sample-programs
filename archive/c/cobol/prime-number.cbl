identification division.
program-id. prime-number.

data division.
working-storage section.

01 cmdargs     pic x(38).
01 num         pic s9(15) comp.
01 dot-count   pic 9 value 0.
01 cnt         pic s9(15).
01 upper-limit pic s9(15).
01 step        pic 9 value 2.

procedure division.

main.
    accept cmdargs from command-line

    if function test-numval(cmdargs) not = 0
        perform show-usage
        stop run
    end-if

    inspect cmdargs tallying dot-count for all "."
    if dot-count > 0
        perform show-usage
        stop run
    end-if

    compute num = function numval(cmdargs)

    if num < 0
        perform show-usage
        stop run
    end-if

    evaluate num
        when 0
        when 1
            perform print-composite
            stop run
        when 2
        when 3
            perform print-prime
            stop run
    end-evaluate

    if function mod(num 2) = 0 or function mod(num 3) = 0
        perform print-composite
        stop run
    end-if

    compute upper-limit = function integer-part(function sqrt(num))

    move 5 to cnt
    move 2 to step

    perform until cnt > upper-limit
        if function mod(num cnt) = 0
            perform print-composite
            stop run
        end-if

        add step to cnt

        if step = 2
            move 4 to step
        else
            move 2 to step
        end-if
    end-perform

    perform print-prime
    stop run.

print-prime.
    display "prime".

print-composite.
    display "composite".

show-usage.
    display "Usage: please input a non-negative integer"
    stop run.
