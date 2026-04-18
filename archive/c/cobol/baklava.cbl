identification division.
program-id. baklava.

data division.
working-storage section.

01 max-width        pic 9(2) value 21.
01 half-width       pic 9(2).

01 row              pic 9(2).
01 num-spaces       pic 9(2).
01 num-stars        pic 9(2).

01 space-line       pic x(21) value all spaces.
01 star-line        pic x(21) value all "*".

procedure division.
main.
    compute half-width = (max-width - 1) / 2

    perform varying row from 0 by 1 until row = max-width
        perform compute-line
        perform render-line
    end-perform

    stop run.

compute-line.
    compute num-spaces = function abs(row - half-width)
    compute num-stars  = max-width - (2 * num-spaces).

render-line.
    display space-line(1:num-spaces) with no advancing
    display star-line(1:num-stars).
