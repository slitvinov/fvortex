<h2>Dependencies</h2>

make, Fortran 77 compiler (tested with GNU Fortran, Intel, PGI, and HPE/Cray)

<h2>Build</h2>

Compile using gfortran by default (edit [conf.mk](conf.mk) for other
options).

<pre>
; (cd lib && make)
; (cd example/gauss && make)
; (cd example/omegaI && make)
</pre>

<h2>Unit tests</h2>

[example/unit](example/unit/)

<pre>
; ./make_box.gp data/points
</pre>

<p align="center"><img src="./img/make_box.svg"/></p>

<h2>Run</h2>

Elliptic <a href="https://en.wikipedia.org/wiki/Lamb%E2%80%93Oseen_vortex">Lamb–Oseen vortex</a>:

<pre>
; cd example/gauss
; ./main
</pre>


<a href="https://doi.org/10.1017/S0022112087001150">Melander,  McWilliams,  and  Zabusky vortex</a>:

<pre>
; cd example/omegaI
; ./main
</pre>

<h2>Hacking</h2>

<pre>
; for i in *.f; do findent --indent_procedure=0 < $i > t && mv t $i; done
</pre>

<h2>Results</h2>

<p align="center"><img src="./img/gauss.gif"/></p>
