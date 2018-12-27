set -l path $DIRNAME/.t-$TESTNAME-(random)

function -S setup
    mkdir -p $path/{foo,bar}

    for name in foo bar
        echo $name > $path/$name/$name

        command git -C $path/$name init --quiet
        command git -C $path/$name config user.email "name@fisherman.sh"
        command git -C $path/$name config user.name "name"
        command git -C $path/$name add -A
        command git -C $path/$name commit -m "First commit" > /dev/null
    end

    command git -C $path/bar tag -am 'This is the first release' v1.0.0
    command git -C $path/bar checkout -q v1.0.0

end

function -S teardown
    rm -rf $path
end

test "$TESTNAME - Test if HEAD ref points to a tag #1"
    0 -eq (
        pushd $path/bar
        git_is_tag
        echo $status
        popd
        )
end

test "$TESTNAME - Test if HEAD ref points to a tag #2"
    1 -eq (
        pushd $path/foo
        git_is_tag
        echo $status
        popd
        )
end

test "$TESTNAME - Test if HEAD ref points to a tag #3"
    1 -eq (
        pushd $HOME
        git_is_tag
        echo $status
        popd
        )
end
