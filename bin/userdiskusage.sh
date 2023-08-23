#!/bin/bash

cd /home

for d in `ls`
do
	du -hs $d
done

