#!/bin/sh

gnuplot <<!
set term pngcairo size 1200,1200
set output "make_box.png"
set size sq
unset key
unset border
unset xtics
unset ytics
set xrange [-1:1]
set yrange [-1:1]

plot \
"data/points" w p pt 7 ps 1.5, \
"<./make_box < data/points" w l lw 3
!
