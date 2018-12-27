test "$TESTNAME: Fail if no commands are given"
    1 = (
        spin
        echo $status
        )
end

test "$TESTNAME - Fail if there is any output to standard error"
    ok = (
        if not spin "echo errored >& 2" --error=/dev/null
            echo ok
        end
        )
end

test "$TESTNAME - Run commands in the background"
    3 = (spin "math 1 + 2")
end

