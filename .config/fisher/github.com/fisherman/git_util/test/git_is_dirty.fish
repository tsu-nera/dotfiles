set -l path $DIRNAME/.t-$TESTNAME-(random)

function -S setup
    mkdir -p $path/{foo,bar}

    for name in foo bar
        echo $name > $path/$name/$name

        command git -C $path/$name init --quiet
        command git -C $path/$name config user.email "name@fisherman.sh"
        command git -C $path/$name config user.name "name"
        command git -C $path/$name add -A
    end

    echo dirty > $path/foo/foo

    command git -C $path/bar commit -m "Clean" > /dev/null
end

function -S teardown
    rm -rf $path
end

test "$TESTNAME - Test if there are changes not staged for commit #1"
    0 -eq (
        pushd $path/foo
        git_is_dirty
        echo $status
        popd
        )
end

test "$TESTNAME - Test if there are changes not staged for commit #2"
    1 -eq (
        pushd $path/bar
        git_is_dirty
        echo $status
        popd
        )
end

test "$TESTNAME - Test if there are changes not staged for commit #3"
    1 -eq (
        pushd $HOME
        git_is_dirty
        echo $status
        popd
        )
end
