#!/bin/bash
# retreive all the mac addresses of hmem cluster

rm macs.txt

for i in `seq 1 20`
do
   printf -v var '%02d' $i
   echo hmem$var
   #echo $i
   (ssh hmem$var /sbin/ifconfig ) | grep eth0 >>macs.txt
done

cat macs.txt

