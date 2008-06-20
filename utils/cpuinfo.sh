#!/bin/bash

[ -r /proc/cpuinfo -o -d /sys/devices/system/cpu ] || {
    echo "no /proc/cpuinfo and /sys/devices/system/cpu/ found."
    exit 1
}
cat <<EOF
# ----------------------------- EXPLANATION -------------------------------
# physical_id   : Physical package id of the logical CPU
#   core_id     : Core id of the logical CPU
#     processor : Id of a logical CPU
# cores         : Total number of cores in the physical package currently in
#                 use by the OS
# siblings      : Total number of logical processors(includes both threads
#                 and cores) in the physical package currently in use by
#                 the OS 
# core_siblings : Siblings mask of all the logical CPUs in a physical package
# thread_siblings : Siblings mask of all the logical CPUs in a CPU core
#
# core and thread siblings are cpu masks, each bit in thread siblings represents
# a processor.
#
# Reference: Multi-core and Linux Kernel, Suresh Siddha
#
EOF

physical_id=?
core_id=?
processor=?
cores=?
siblings=?
flags=?

if [ -r /proc/cpuinfo ]; then
    echo    "--------------------------- /proc/cpuinfo ---------------------------"
    echo -e "physical_id   core_id   processor   cores   siblings\tflags"
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
    echo -e "physical_id   core_id   processor   core_siblings\tthread_siblings"
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

