#!/bin/sh
# opens Windows .URL files in your default browser
# requires: xdg-open sed grep xargs
sed 's/^BASEURL=/URL=/' "$1" | grep -m 1 '^URL=' | sed 's/^URL=//' | sed 's/\r//' | xargs xdg-open

