#!/bin/sh

gnuplot -p <<!
set size sq
unset key
plot "<./box_dim < data/omegaI.dat" w l lw 3, "data/omegaI.dat" w p ps 0.5 pt 7
!
