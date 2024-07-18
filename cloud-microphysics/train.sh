#!/bin/bash
min_bins=$1
max_bins=$2
let subfloor=$min_bins-1
j=subfloor
while (( j++ < max_bins )); do
 echo ""
 echo "---------> Training with $j bins along each phase-space dimension <---------"
 max_inner=1000
 i=0
 while (( i++ < max_inner )); do

   if [ -f stop ]; then
     echo ""
     echo "---------> 'stop' file found -- removing 'stop' & exiting script <---------" 
     rm stop
     exit 0
   fi 

   echo ""
   echo "---------> Run $i <---------"
   ./train-cloud-microphysics --base training --epochs 1000000 --bins $j --report 1000 --start 360 --stride 10 --tolerance "5.0E-08"

   if [ -f converged ]; then
     echo ""
     echo "---------> 'converged' file found -- removing 'converged' & exiting inner loop <-------------"
     rm converged
     break
   fi 
 done
done
