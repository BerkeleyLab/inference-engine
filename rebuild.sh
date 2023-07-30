#!/bin/bash

yes "yes" | rm -vRI build/
fpm run --example train-and-write --compiler ifx --profile debug --flag "-coarray=single"

