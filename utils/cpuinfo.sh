#!/bin/bash

[ -r /proc/cpuinfo -o -d /sys/devices/system/cpu ] || {
    echo "no /proc/cpuinfo and /sys/devices/system/cpu/ found."
    exit 1
}
cat <<EOF
# ----------------------------- EXPLANATION -------------------------------
# physical id   : physical package id
#   core id     : cpu core id of a processor
#     processor : id of a processor, represent a processing unit seen by OS
# cores         : number of cores in the corresponding physical package
# siblings      : number of processors in the corresponding physical package
# core siblings : the thread siblings to cpu X in the same core
# thread siblings : the thread siblings to cpu X in the same physical package
#
# core and thread siblings are cpu masks, each bit in thread siblings represents
# a processor.
EOF

physical_id=?
core_id=?
processor=?
cores=?
siblings=?
flags=?

if [ -r /proc/cpuinfo ]; then
    echo    "--------------------------- /proc/cpuinfo ---------------------------"
    echo -e "physical id   core id   processor   cores   siblings\tflags"
    while read r ; do
        case $r in
        physical*)
        physical_id=${r#*: }
        ;;
        core*)
        core_id=${r#*: }
        ;;
        processor*)
        processor=${r#*: }
        ;;
        cpu\ cores*)
        cores=${r#*: }
        ;;
        siblings*)
        siblings=${r#*: }
        ;;
        flags*)
        flags=32-bit
        echo $r | grep -qw lm && flags=64-bit
        echo $r | grep -qw ht && flags="$flags HyperThread"
        ;;
        esac

        if [ -z  "$r" -a '?' != $physical_id ]; then
            echo -e "$physical_id\t      $core_id\t\t$processor\t    $cores\t    $siblings\t\t$flags"
            physical_id=?
            core_id=?
            processor=?
            cores=?
            siblings=?
            flags=?
        fi
    done < /proc/cpuinfo
fi

if [ -d /sys/devices/system/cpu ]; then
    echo    "----------------------- /sys/devices/system/cpu/ --------------------"
    echo -e "physical id   core id   processor   core_siblings\tthread_siblings"
    for cpu in /sys/devices/system/cpu/cpu* ; do
        processor=${cpu##*cpu}
        [ -r $cpu/topology/physical_package_id ] || continue
        physical_id=`cat $cpu/topology/physical_package_id`
        core_id=`cat $cpu/topology/core_id`
        core_siblings=`cat $cpu/topology/core_siblings`
        thread_siblings=`cat $cpu/topology/thread_siblings`
        echo -e "$physical_id\t      $core_id\t\t$processor\t    $core_siblings\t\t$thread_siblings"
    done
fi

