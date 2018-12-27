function last_job_id -d "Get the id of one or more existing jobs"
    builtin jobs $argv | command awk -v FS=\t '
        /[0-9]+\t/{
            aJobs[++nJobs] = $1
        }
        END {
            for (i = 1; i <= nJobs; i++) {
                print(aJobs[i])
            }

            exit nJobs == 0
        }
    '
end
