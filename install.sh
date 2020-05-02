#!/bin/bash

set -e

if [[ "$1" == "" ]]; then
    echo "Please enter the Gambit lib path (nothing for /usr/local/Gambit/lib)"
    read -r path

    if [[ "" == "$path" ]]; then
        path="/usr/local/Gambit/lib" 
    fi
else
    path="$1"
fi

cp -r ./src/. "$path"
# cp "./oyster-core.scm" "$path"
# cp "./oyster.scm" "$path"

echo "Done :)"
