#!/bin/bash

set -e

echo "Please enter the Gambit lib path (nothing for /usr/local/Gambit/lib)"
read -r path

if [[ "" == "$path" ]]; then
  path="/usr/local/Gambit/lib" 
fi


cp -r ./src/. "$path"
# cp "./oyster-core.scm" "$path"
# cp "./oyster.scm" "$path"

echo "Done :)"
