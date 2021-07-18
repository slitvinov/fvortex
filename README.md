<h2>Dependencies</h2>

make, fortran compiler (tested with GNU Fortran, Intel, PGI, and HPE/Cray)

<h2>Build</h2>

Compile using gfortran by default (edit [conf.mk](conf.mk) for other
options).

    (cd lib && make)
    (cd example/gauss && make)
    (cd example/omegaI && make)

<h2>Unit tests</h2>

  [example/unit](example/unit/)

    ./make_box.gp data/points

<p align="center"><img src="./img/make_box.svg"/></p>

<h2>Run</h2>

Elliptic <a href="https://en.wikipedia.org/wiki/Lamb%E2%80%93Oseen_vortex">Lamb–Oseen vortex</a>:

    cd example/gauss
    ./main

MMZ vortex:

    cd example/omegaI
    ./main
