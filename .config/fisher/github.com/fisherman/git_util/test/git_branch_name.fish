test "$TESTNAME - Get the name of the current Git branch"
    (git_branch_name) = (
        command git branch --no-color \
        | sed -nE 's|^\* (.+)$|\1|p' \
        | sed -E 's/(\((HEAD )?detached (at)?(from)? (.+)\)$)/\5/'
        )
end
