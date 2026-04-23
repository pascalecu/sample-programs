identification division.
program-id. palindromic-number.

data division.
working-storage section.
01 cmdargs            pic x(38).

01 buffer-num-area.
    05 buffer-num     pic 9(15).

01 num                pic s9(15) comp.
01 temp               pic s9(15) comp.
01 rev                pic s9(15) comp value 0.
01 digit              pic 9.
01 original           pic s9(15) comp.

procedure division.
main.
    accept cmdargs from command-line

    if cmdargs = spaces or function trim(cmdargs) is not numeric
        perform show-usage
    end-if

    move function trim(cmdargs) to buffer-num
    move buffer-num to num

    if num < 0
        perform show-usage
    end-if

    move num to original
    move num to temp

    perform until temp = 0
        divide temp by 10 giving temp remainder digit
        compute rev = (rev * 10) + digit
    end-perform

    if rev = original
        display "true"
    else
        display "false"
    end-if

    stop run.

show-usage.
    display "Usage: please input a non-negative integer"
    stop run.