#!/bin/bash

set -e

echo "------------------------------------"
echo "------------------------------------"
echo "            OYSTER TESTS            "
echo "------------------------------------"
echo "------------------------------------"

echo "$( whereis gsi)"

for f in $( ls ./tests )
 do
     echo "Running file $f"
     if [[ "$1" == "travis" ]]; then
         /usr/local/Cellar/gambit-scheme/4.9.3_2/v4.9.3/bin/gsi "./tests/$f"
     else
         "$( whereis gsi | awk '{print $2}')" "./tests/$f"
     fi
done
