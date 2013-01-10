#!/bin/bash

awk 'BEGIN {FS=", "} ; { if ($15==">50K") print $1 ": " $14 }' adult.data > ricachones.txt

