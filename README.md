<h2>Introduction</h2>

This is a Fortran implementation of the vortex method, a numerical
technique for simulating fluid flow. The code solves the
two-dimensional incompressible Euler and Navier-Stokes equations using
a Lagrangian particle-based approach, where the fluid is represented
by a collection of discrete vortices.

<h2>Dependencies</h2>

To run this program, you need a Fortran 77 compiler (tested with GNU
Fortran, Intel, PGI, and HPE/Cray) and make.

<h2>Build</h2>

By default, the program uses GNU Fortran. To build the program, run:

<pre>
$ (cd lib && make)
$ (cd example/gauss && make)
$ (cd example/omegaI && make)
</pre>

Edit the
<a href="conf.mk">conf.mk</a>
file to use a different Fortran compiler,

<h2>Unit tests</h2>

Navigate to the
<a href="example/unit/">example/unit</a>
directory and run the following command (you will need to have
<a href="http://www.gnuplot.info/">gnuplot</a>
installed)

<pre>
$ make
$ ./make_box.gp data/points
</pre>

This will create a box for paritcles in
<a href="example/unit/data/points">data/points</a>

<p align="center"><img src="./img/make_box.svg"/></p>

<h2>Run</h2>

To run the program, navigate to one of the example directories and run
the main executable. Here are two examples:

Elliptic <a href="https://en.wikipedia.org/wiki/Lamb%E2%80%93Oseen_vortex">Lamb–Oseen vortex</a>:

<pre>
$ cd example/gauss
$ ./main
 initial number of Particles        60025
           Particles :    60025      Time :  0.3405
...	   
</pre>

<a href="https://doi.org/10.1017/S0022112087001150">Melander,  McWilliams,  and  Zabusky vortex</a>:

<pre>
$ cd example/omegaI
$ ./main
 initial number of Particles       672400
           Particles :   672400      Time : 12.1381
...
</pre>

<h2>Postprocessing</h2>

You can use the
<a href="tool/heat.awk">tool/heat.awk</a>
script to generate a heatmap from the
simulation data. Here's an example:

<pre>
$ awk -f tool/heat.awk example/gauss/w.00000001.dat > heat.ppm
$ convert heat.ppm heat.png
</pre>

<p align="center"><img src="./img/heat.png"/></p><br/>

<h2>Results</h2>

<p align="center"><img src="./img/gauss.gif"/></p><br/>
<p align="center"><img src="./img/omegaI.gif"/></p>
