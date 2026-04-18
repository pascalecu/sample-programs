identification division.
program-id. even-odd.

data division.
working-storage section.

01 cmdargs pic x(38).
01 num     pic s9(30).

procedure division.

main.
    accept cmdargs from command-line

    if function test-numval(cmdargs) not = 0
        display "Usage: please input a number"
        stop run
    end-if

    compute num = function numval(cmdargs)

    if function mod(num 2) = 0
        display "Even"
    else
        display "Odd"
    end-if

    stop run.
