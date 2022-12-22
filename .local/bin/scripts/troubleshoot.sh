#!/bin/bash
# Troubleshoot.sh
# A more elaborate version of Troubleshoot.sh.

SUCCESS=0
E_DB=99    # Error code for missing entry.

declare -A address

date=$(date +%F)
hd_log="/tmp/${date}_HDs.log"

smartctl --scan | awk '{print $1}' > $hd_log
lspci | grep -i raid >> $hd_log

getArray () {
    i=0
    while read line # Read a line
    do
        array[i]=$line # Put it into the array
        i=$(($i + 1))
    done < $1
}

getArray $hd_log

for e in "${array[@]}"
do
    if [[ $e =~ /dev/sd* || $e =~ /dev/hd* ]]
        then
            echo "smartctl -i -A $e" >> ${date}_Troubleshoot.log
            smartctl -i -A $e >> ${date}_Troubleshoot.log # Run smartctl on all disks
    fi
done
exit $?   # In this case, exit code = 99, since that is function return.
