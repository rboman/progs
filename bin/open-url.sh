#!/bin/sh
# opens Windows .URL files in your default browser
# requires: xdg-open sed grep xargs
sed 's/^BASEURL=/URL=/' "$1" | grep -m 1 '^URL=' | sed 's/^URL=//' | sed 's/\r//' | xargs xdg-open

# see http://www.danielbrice.net/blog/opening-url-file-like-a-pro/


# create file:
# ; ~/.local/share/applications/openurl.desktop
# [Desktop Entry]
# Version=1.0
# Name=openurl
# Comment=A script for opening windows `.url` files.
# Exec=/home/boman/dev/progs/bin/open-url.sh
# Type=Application
# MimeType=application/x-mswinurl

# then:
# xdg-mime default openurl.desktop application/x-mswinurl

