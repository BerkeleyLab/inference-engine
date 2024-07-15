#!/bin/zsh
i=0
j=2
while (( j++ < 10)); do
 while (( i++ < 12)); do
   if [ -f stop ]; then
     echo "---------> 'stop' file found -- removing 'stop' & exiting <-------------"
     rm stop
     exit 0
   fi 
   print ""
   echo "---------> Run $i <--------->"
   ./train-cloud-microphysics --base training --epochs 1000000 --bins $j --report 1000 --start 360 --stride 10
 done
done
