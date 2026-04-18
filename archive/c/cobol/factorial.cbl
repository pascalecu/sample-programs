identification division.
program-id. factorial.

data division.
working-storage section.

01 cmd-args  pic x(38).
01 num       pic s9(7).
01 result    pic z(18)9 value 1.

procedure division.

main.
    accept cmd-args from command-line

    if function test-numval(cmd-args) not = 0
        perform show-usage
    end-if

    compute num = function numval(cmd-args)

    if num < 0
        perform show-usage
    end-if

    compute result = function factorial(num)
    display result

    stop run.

show-usage.
    display "Usage: please input a non-negative integer"
    stop run.
