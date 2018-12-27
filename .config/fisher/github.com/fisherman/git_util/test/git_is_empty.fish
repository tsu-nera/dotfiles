set -l path $DIRNAME/.t-$TESTNAME-(random)

function -S setup
    mkdir -p $path/{foo,bar,baz}

    for name in foo bar
        git -C $path/$name init --quiet
        git -C $path/$name config user.email "name@fisherman.sh"
        git -C $path/$name config user.name "name"

        if test "$name" = foo
            echo $name > $path/$name/$name
            git -C $path/$name add -A
            git -C $path/$name commit -m "Clean" > /dev/null
        end
    end
end

function -S teardown
    rm -rf $path
end

test "$TESTNAME - Test if a repository is empty #1"
    1 -eq (
        pushd $path/foo
        git_is_empty
        echo $status
        popd
        )
end

test "$TESTNAME - Test if a repository is empty #2"
    1 -eq (
        pushd $path/baz
        git_is_empty
        echo $status
        popd
        )
end

test "$TESTNAME - Test if a repository is empty #3"
    0 -eq (
        pushd $path/bar
        git_is_empty
        echo $status
        popd
        )
end
