#!/bin/bash

set -e

echo "Please enter the Gambit lib path (nothing for /usr/local/Gambit/lib)"
read -r path

if [[ "" == "$path" ]]; then
  path="/usr/local/Gambit" 
fi

cp "oyster.scm" "$path"

echo "Done :)"
