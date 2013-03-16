#!/bin/bash
#
# Purpose:
#   A script to pull files from many hosts and push to HDFS through HttpFS proxy.
#
#   Files are pulled from hosts by "ssh $host cat" and directly pipe to "curl"
#   to push to HDFS through HttpFS proxy.
#
#   hosts  --------> collectors -------> hdfs-httpfs proxy
#            ssh cat              curl
#        (connect with UNIX pipe on collector boxes)
#
# Usage:
#   Run "./pull-to-hdfs.sh -h" for help, expected usage is like these:
#       * cron job: sleep random-time; cat hosts.txt | ./pull-to-hdfs.sh /var/log '-type f -mtime -7 -name "*.gz"'
#           (this cron job can be run on a collector cluster)
#       * Hadoop job:
#           o merge small files into big sequence files
#           o touch xxx.gz.done and then delete xxx.gz
#           o delete *.done and *.tmp older than 7 days
#
#   The maximum number of concurrent jobs is controlled by "pgrep -f $0", so
#       a) don't copy this script to another name and run both against *same* set of hosts,
#          since that breaks bandwidth limit;
#       b) run against *different* sets of hosts on same collector box is fine
#
#   On each host there is at most one "ssh cat" process to pull data, this
#   is implemented by lock file /tmp/$(basename $0 .sh).lck, notice the
#   lock file is created on the source host, not collector box. This also
#   works for multiple pull-to-hdfs.sh instances running as different
#   users.
#
# Dependencies:
#   ssh, curl, util-linux, perl modules List::Util and XML::Simple
#
# Features:
#   * no agent on collected hosts,  pull mode to fetch files
#   * don't cache on hard drive of collector box,  I never knew server hard drive is so error-prone
#   * state is stored on HDFS, can be interrupted at any time and recover later
#   * bandwidth limit by host and by whole synchronization job
#   * concurrent pulling from multiple hosts
#   * linearly extensible on collector boxes,  all collectors collect different hosts at the same time
#   * very few dependencies, ~200 lines shell script, easy to understand and deploy
#
# Cons:
#   * aim to batch collecting, not real-time collecting, that's doable but hard to be reliable
#   * many hits to HDFS proxy in a run of synchronization, roughly num_collectors x num_files x 5,
#     to collect five hourly log files in a week on 80 boxes with 1 collector, it's
#     1 x (7 * 24 * 5) * 5 = 4200 hits, luckily the QPS is very low
#
# TODO:
#   * pluggble storage backend, not limit to HDFS, such as local or remote disk
#   * calculate md5sum during "ssh cat | curl" and upload it to grid
#     (actually can be bypased by a MANIFEST and MANIFEST.MD5 file)
#
# Author:
#   Yubao Liu <yubao.liu@gmail.com>     2013-03-16

########################################################################
pull_to_hdfs () {
    local host=$1 find_dir find_args
    local file_paths file_sizes file_info retries=0 s i t
    shift

    declare -a file_paths
    declare -a file_sizes
    declare -a file_info

    while [ "$2" ]; do
        find_dir="$1"
        find_args="$2"
        shift 2

        find_dir=${find_dir%/}
        file_paths=()
        file_sizes=()

        while read s; do
            file_info=($s)
            [ "${file_info[8]}" ] || continue
            [ "${file_info[4]}" ] || continue
            [ "${file_info[4]}" != 0 ] || continue

            file_paths[${#file_paths[@]}]=${file_info[8]}
            file_sizes[${#file_sizes[@]}]=${file_info[4]}
        done < <(ssh $SSH_OPTS $host find $find_dir $find_args -exec ls -l '{}' '\;')

        for (( i=0; i < ${#file_paths[@]}; ++i )); do
            t=$(date +%s)
            pull_file_to_hdfs $host $find_dir ${file_paths[$i]} ${file_sizes[$i]}

            case $? in
                0) t=$(( $(date +%s) - t ))
                   [ $t -le 0 ] && t=1
                   log_info "successfully upload: $host:${file_paths[$i]} ${file_sizes[$i]} bytes, $t seconds," \
                        $(perl -le "printf '%.3f KBps', ${file_sizes[$i]} / $t")
                   ;;
                1) log_info "already exists: $host:${file_paths[$i]} ${file_sizes[$i]} bytes"
                   ;;
                2) log_info "being uploaded by others: $host:${file_paths[$i]} ${file_sizes[$i]} bytes"
                   ;;
              254) log_error "failed to ssh or lock $LOCK_FILE or read: $host:${file_paths[$i]} ${file_sizes[$i]} bytes"
                   [ $(( ++retries )) -gt $RETRIES_ON_READ ] && {
                        log_error "continously failed on $host more than $RETRIES_ON_READ times, give up..."
                        return 1
                    }
                   ;;
              255) log_error "failed to upload: $host:${file_paths[$i]} ${file_sizes[$i]} bytes"
                   ;;
                *) log_error "unknown: $host:${file_paths[$i]} ${file_sizes[$i]} bytes"
                   ;;
            esac

            sleep 2     # make HDFS proxy's life a little easier
        done
    done
}

# exit code:
#   0   upload successfully
#   1   already exists
#   2   being uploaded by others
#   254 failed to ssh or lock $LOCK_FILE or read
#   255 failed to upload
pull_file_to_hdfs () {
    local host=$1 find_dir=$2 file_path=$3 file_size=$4
    local file_name tmp_file_name done_file_name hdfs_file_size new_hdfs_file_size s

    file_name=${file_path#$find_dir/}
    tmp_file_name="$file_name.tmp"
    done_file_name="$file_name.done"

    ##
    ## Check whether the file has already been uploaded
    ##

    s=$(query_hdfs_httpfs "$host/$done_file_name?op=status")
    hdfs_file_size=$(echo $s | perl -MXML::Simple -le 'print XMLin("-")->{file}{size}')
    [ "$hdfs_file_size" ] && return 1

    s=$(query_hdfs_httpfs "$host/$file_name?op=status")
    hdfs_file_size=$(echo $s | perl -MXML::Simple -le 'print XMLin("-")->{file}{size}')

    if [ "$file_size" = "$hdfs_file_size" ]; then
        # delete possibly existed temporary file
        query_hdfs_httpfs "$host/$tmp_file_name?op=delete" -X PUT >/dev/null 2>&1
        return 1
    elif [ "$hdfs_file_size" ]; then
        log_error "different file sizes: $host:$file_path is $file_size bytes, but $HDFS/$DEST_DIR/$host/$file_name is $hdfs_filesize bytes!"
        return 255
    fi

    ##
    ## hdfs_file_size is empty, means that file doesn't exist or there are something wrong in the HTTP call
    ##

    s=$(query_hdfs_httpfs "$host/$tmp_file_name?op=status")
    hdfs_file_size=$(echo $s | perl -MXML::Simple -le 'print XMLin("-")->{file}{size}')

    if [ "$file_size" = "$hdfs_file_size" ]; then
        s=$(query_hdfs_httpfs "$host/$tmp_file_name?op=move&dest=$DEST_DIR/$host/$file_name" -X PUT)
        [ "$s" ] && {
            log_error "failed to rename HDFS file: $s"
            return 255
        }

        return 1;
    elif [ "$hdfs_file_size" ]; then
        local seconds=$(( 10 + $RANDOM % 30 ))
        log_info "sleep $seconds seconds to check whether $host:$file_path is being uploaded by others..."
        sleep $seconds

        s=$(query_hdfs_httpfs "$host/$tmp_file_name?op=status")
        new_hdfs_file_size=$(echo $s | perl -MXML::Simple -le 'print XMLin("-")->{file}{size}')

        if [ "$hdfs_file_size" != "$new_hdfs_file_size" ]; then
            # other uploader is working on this file
            return 2
        fi

        s=$(query_hdfs_httpfs "$host/$tmp_file_name?op=delete" -X PUT)
        [ "$s" ] && {
            log_error "failed to delete HDFS file: $s"
            return 255
        }
    fi

    ##
    ## the partial temporary file wasn't uploaded successfully or this file is never uploaded before
    ##

    # NOTICE: don't delete temporary file after failure because that may be created by other uploader
    if s=$(set -o pipefail; ssh $SSH_OPTS $host flock -n "$LOCK_FILE" cat $file_path |
                query_hdfs_httpfs "$host/$tmp_file_name?op=create" -T "-" --limit-rate $RATE); then
        [ "$s" ] && {
            log_error "failed to upload $host:$file_path: $s"
            return 255
        }
    else
        [ "$s" ] && {
            log_error "failed to upload $host:$file_path: $s"
            return 255
        }

        return 254
    fi

    # not delete temporary file because this may be temporary failure, let next run to cleanup it,
    # anyway, it's not terrible to leave a few garbage on HDFS.
    s=$(query_hdfs_httpfs "$host/$tmp_file_name?op=move&dest=$DEST_DIR/$host/$file_name" -X PUT)
    [ "$s" ] && {
        log_error "failed to rename HDFS file: $s"
        return 255
    }

    return 0
}

query_hdfs_httpfs () {
    local filename=$1

    shift
    curl -s -k --negotiate -u : "$HDFS/$DEST_DIR/$filename" "$@"
}

log_info () {
    [ "$QUIET" ] || echo [$(date "+%Y-%m-%d %H:%M:%S")] INFO "$@"
}

log_error () {
    echo [$(date "+%Y-%m-%d %H:%M:%S")] ERROR "$@" >&2
}

########################################################################

CONCURRENCY=${CONCURRENCY:-10}      # copy files from 10 machines at the same time
DEST_DIR=${DEST_DIR:-/user/liuyb/logs/raw}
HDFS=${HDFS:-"https://httpfs-host:4443/webhdfs/v1"}
RATE=${RATE:-1m}                    # 1 MB/s bandwidth limit
SSH_OPTS=${SSH_OPTS:-"-o StrictHostKeyChecking=no -o BatchMode=yes -o ConnectTimeout=20 -o ServerAliveInterval=30 -o TCPKeepAlive=yes"}
RETRIES_ON_READ=${RETRIES_ON_READ:-10}

while getopts "a:c:d:r:s:hq" opt; do
    case $opt in
        c) CONCURRENCY=$OPTARG ;;
        d) DEST_DIR=$OPTARG ;;
        r) RATE=$OPTARG ;;
        s) HDFS=$OPTARG ;;
        q) QUIET=1 ;;
        *) echo "Usage: cat hosts.txt | $0 [-c concurrency] [-d dest_hdfs_dir] [-r rate_BPS] [-s HDFS_URL] [-q] dir1 find_args1 dir2 find_args2 ..." >&2
           echo "        Example: cat hosts.txt | $0 -s https://httpfs-host:4443/webhdfs/v1 -d /users/liuyb/logs/raw -c 10 -r 1m /var/log '-type f -mtime -7 -name \"*.gz\"'" >&2
           exit 0
    esac
done

shift $(( OPTIND - 1 ))

HDFS=${HDFS%/}
DEST_DIR=${DEST_DIR%/}
LOCK_FILE=/tmp/$(basename $0 .sh).lck

[ 0 = $(expr "$DEST_DIR" : "/.") ] && {
    log_error "destination HDFS directory must start with '/'!"
    exit 1
}

# try to avoid multiple uploaders pulling files from same host
HOSTS=( $(perl -le 'use List::Util "shuffle"; chomp(@a=<STDIN>); @a=shuffle @a; print "@a"') )
[ ${#HOSTS[@]} -eq 0 ] && exit 0

log_info "start to collect files from ${HOSTS[*]}"

for (( i=0; i < ${#HOSTS[@]}; ++i )); do
    while : ; do
        [ $(pgrep -f $0 | wc -l) -le $CONCURRENCY ] && break
        sleep 5
    done

    ( pull_to_hdfs ${HOSTS[$i]} "$@" ) &
done

# wait all child worker processes
wait

log_info "done."

