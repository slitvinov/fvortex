# Dependencies

gfortran, make

# Build

    (cd lib && make)
    (cd example/gauss && make)
    (cd example/omegaI && make)

# Unit tests

  [example/unit](example/unit/)

# Run

Lamb vortex:

    cd example/gauss
    ./main

MMZ vortex:

    cd example/omegaI
    ./main
