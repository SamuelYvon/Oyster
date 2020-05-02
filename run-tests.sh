#!/bin/bash

set -e 

echo "------------------------------------"
echo "------------------------------------"
echo "            OYSTER TESTS            "
echo "------------------------------------"
echo "------------------------------------"

for f in $( ls ./tests )
do
    echo "Running file $f"
    /usr/local/Gambit/bin/gsi "./tests/$f"
done
