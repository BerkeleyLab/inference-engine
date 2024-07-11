#!/bin/zsh
i=0
while (( i++ < 20)); do
  print ""
  echo "---------> Run $i <-------------"
  ./build/run-fpm.sh run train-cloud-microphysics -- --base training --epochs 20000 --bins 80 --stride 20 --report 500
done
