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
    "$( whereis gsi | awk '{print $2}')" "./tests/$f"
done
