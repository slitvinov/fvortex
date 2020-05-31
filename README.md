# Dependencies

make, fortran compiler (tested with GNU Fortran, Intel, PGI, and HPE/Cray)

# Build

Compile using gfortran by default (edit [conf.mk](conf.mk) for other
options).

    (cd lib && make)
    (cd example/gauss && make)
    (cd example/omegaI && make)

# Unit tests

  [example/unit](example/unit/)

    ./make_box.gp data/points

<p align="center"><img src="./img/make_box.svg"/></p>

# Run

Lamb vortex:

    cd example/gauss
    ./main

MMZ vortex:

    cd example/omegaI
    ./main
