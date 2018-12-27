set -l fixtures $DIRNAME/fixtures

test "$TESTNAME - Read input and write it to standard output"
    (echo foo | get) = foo
end

test "$TESTNAME - Use --silent=num to read up to an arbitrary number of characters"
    (echo foo | get --silent=1 ^ /dev/null) = f
end

test "$TESTNAME - Use --prompt=msg to display a prompt"
    (cat $fixtures/prompt) = (echo | get --prompt=foo ^& 1)
end

test "$TESTNAME - Use --rule=regex to validate user input"
    123 = (
        echo 123 | get --rule="[0-9]"
        )
end

test "$TESTNAME - Use -d=value flag"
    y = (
        echo | get -d y
        )
end

test "$TESTNAME - Use --default=value flag"
    y = (
        echo | get --default y
        )
end

test "$TESTNAME - Use --default=value flag with --regex"
    y = (
        echo | get --default y --rule="^[yn]\$"
        )
end

test "$TESTNAME - Use --default=value flag with --silent"
    y = (
        echo | get --default y --silent
        )
end

test "$TESTNAME - Use --default=value flag with --no-cursor"
    y = (
        echo | get --default y --no-cursor
        )
end
