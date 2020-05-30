#!/bin/sh

me=make_box.gp
i=$1
o=make_box.svg

gnuplot <<!
set term svg
set output "$o"
set size sq
unset key
unset border
unset xtics
unset ytics
set xrange [-1:1]
set yrange [-1:1]

plot \
"$i" w p pt 7, \
"<./make_box < $i" w l lw 3
!

printf %s\\n "$me: $o"

