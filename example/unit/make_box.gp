#!/bin/sh

: ${GNUPLOT=gnuplot}

case $# in
     0) printf %s\\n "make_box.gp: FILE is missing"
	exit 1
	;;
esac

i=$1
o=make_box.svg

"$GNUPLOT" <<!
set term svg background rgb 'white'
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

rc=$?
case $rc in
    0) printf %s\\n "make_box.gp: $o" ;;
    *) printf %s\\n "make_box.gp failed"
       exit $rc
       ;;
esac
