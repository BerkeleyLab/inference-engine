#!/bin/bash
file_path=$1
[ -z $1 ] && echo "Usage: ./aerosol-inference.sh <file-path>" && exit 1
./build/run-fpm.sh run infer-aerosol --profile release --flag "-fopenmp" -- --file-path $file_path