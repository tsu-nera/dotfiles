set -l path $DIRNAME/.t-$TESTNAME-(random)

function -S setup
  mkdir -p $path/{foo,bar}

  for name in foo bar
    command git -C $path/$name init --quiet
    command git -C $path/$name config user.email "name@fisherman.sh"
    command git -C $path/$name config user.name "name"
  end

  command touch $path/foo/{a,b,c}
end

function -S teardown
    rm -rf $path
end

test "$TESTNAME - Get the number of untracked files in a repository"
    3 -eq (
        pushd $path/foo
        git_untracked_files
        popd
    )
end

test "$TESTNAME - Set status to 1 if there are 0 untracked files"
    "0:1" = (
        pushd $path/bar
        set -l count (git_untracked_files)
        printf "$count:$status"
        popd
    )
end
