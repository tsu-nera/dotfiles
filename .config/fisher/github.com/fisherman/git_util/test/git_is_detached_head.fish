set -l path $DIRNAME/.t-$TESTNAME-(random)

function -S setup
    mkdir -p $path/{foo,bar,baz}

    begin
        pushd $path/baz
        command git init --quiet
        command git config user.email "name@fisherman.sh"
        command git config user.name "name"
        echo baz > baz
        command git add -A
        command git commit -m baz
        popd

        pushd $path/foo
        echo 1 > 1
        command git init --quiet
        command git config user.email "name@fisherman.sh"
        command git config user.name "name"
        command git add -A
        command git commit -m 1
        set -l sha (command git rev-parse HEAD)
        echo 2 > 2
        command git add -A
        command git commit -m 2
        command git checkout $sha
        popd
    end > /dev/null ^ /dev/null
end

function -S teardown
    rm -rf $path
end

test "$TESTNAME - Test if a repository is in a detached HEAD state #1"
    0 -eq (
        pushd $path/foo
        git_is_detached_head
        echo $status
        popd
        )
end

if not set -q CI
    # Fails in travis when the directory is not a git repo
    test "$TESTNAME - Test if a repository is in a detached HEAD state #2"
        1 -eq (
            pushd $path/bar
            git_is_detached_head
            echo $status
            popd
            )
    end
end

test "$TESTNAME - Test if a repository is in a detached HEAD state #3"
    1 -eq (
        pushd $path/baz
        git_is_detached_head
        echo $status
        popd
        )
end
