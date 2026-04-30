identification division.
program-id. duplicate-character-counter.

data division.
working-storage section.

01 input-str        pic x(200).
01 usage-msg        pic x(50) value 'Usage: please provide a string'.

01 i                pic 9(4).
01 len              pic 9(4).

01 ascii-table.
   05 ascii-map occurs 256 times pic 9(4) value 0.

01 seen-table.
   05 seen occurs 256 times pic 9 value 0.

01 current-char     pic x.
01 ascii-val        pic 9(4).

01 has-duplicates   pic 9 value 0.

01 out-count        pic z(4)9.

procedure division.

main.

    accept input-str from argument-value

    if input-str = spaces
        display usage-msg
        stop run
    end-if

    move function length(function trim(input-str)) to len

    if len = 0
        display usage-msg
        stop run
    end-if

    perform varying i from 1 by 1 until i > len
        move input-str(i:1) to current-char
        move function ord(current-char) to ascii-val
        add 1 to ascii-map(ascii-val)
    end-perform

    perform varying i from 1 by 1 until i > len

        move input-str(i:1) to current-char
        move function ord(current-char) to ascii-val

        if ascii-map(ascii-val) > 1 and seen(ascii-val) = 0

            move ascii-map(ascii-val) to out-count

            display current-char ": " function trim(out-count)

            move 1 to seen(ascii-val)
            move 1 to has-duplicates
        end-if

    end-perform

    if has-duplicates = 0
        display "No duplicate characters"
    end-if

    stop run.