set -l path $DIRNAME/.t-$TESTNAME-(random)

function -S setup
    mkdir -p $path/{foo,bar,baz}

    for name in foo bar baz
        echo $name > $path/$name/$name

        command git -C $path/$name init --quiet
        command git -C $path/$name config user.email "name@fisherman.sh"
        command git -C $path/$name config user.name "name"
        command git -C $path/$name add -A
    end

    echo dirty > $path/foo/foo

    command git -C $path/bar commit -m "Clean" > /dev/null
    command git -C $path/baz commit -m "Clean" > /dev/null

    echo untracked > $path/baz/untracked
end

function -S teardown
    rm -rf $path
end

test "$TESTNAME - Test if there are any changes in the working tree #1"
    0 -eq (
        pushd $path/foo
        git_is_touched
        echo $status
        popd
        )
end

test "$TESTNAME - Test if there are any changes in the working tree #2"
    1 -eq (
        pushd $path/bar
        git_is_touched
        echo $status
        popd
        )
end

test "$TESTNAME - Test if there are any changes in the working tree #3"
    0 -eq (
        pushd $path/baz
        git_is_touched
        echo $status
        popd
        )
end

test "$TESTNAME - Test if there are any changes in the working tree #4"
    1 -eq (
        pushd $HOME
        git_is_touched
        echo $status
        popd
        )
end
