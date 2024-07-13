#!/bin/zsh
i=0
while (( i++ < 120)); do
  if [ -f stop ]; then
    echo "---------> File named 'stop' found. train.sh removing 'stop' and exiting <-------------"
    rm stop
    exit 0
  fi 
  print ""
  echo "---------> Run $i <-------------"
  ./build/run-fpm.sh run train-cloud-microphysics -- --base training --epochs 100000 --bins 10 --start 720 --report 1000
done
