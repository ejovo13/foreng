#!/bin/bash
MY_FILES=($(echo *.f95))

for FILE in ${MY_FILES[@]}; do
    mv $FILE "$(basename -- "$FILE" .f95).f90"
done