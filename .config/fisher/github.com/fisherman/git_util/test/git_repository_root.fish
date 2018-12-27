test "$TESTNAME - Get the path of a root level directory in a repository while in the root"
    (git_repository_root) = (command git rev-parse --show-toplevel)
end

test "$TESTNAME - Get the path of a root level directory in a respoistory while in a subdirectory of the root"
    (git_repository_root) = (cd ./test; and command git rev-parse --show-toplevel)
end
