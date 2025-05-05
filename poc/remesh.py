from math import sqrt, exp, pi

Rmax = 1.1
ell_x = 6.0
ell_y = 1.0
s2 = 1e-4
ovrlp = 0.9

h2 = s2*ovrlp**2
deltax = sqrt(h2)
h2 = deltax*deltax
Nmx = int(2*Rmax/deltax + 1)
denom = 1.0/(0.1*Rmax)**2

twopi = 2.*pi
twopiinv = 1./twopi
dh = 2.*ovrlp*sqrt(s2/pi)
dhinv = 1./dh


xp = []
yp = []
gp = []
for ix in range(1, Nmx + 1):
    for iy in range(1, Nmx + 1):
        x = -Rmax + deltax*(ix - 0.5)
        y = -Rmax + deltax*(iy - 0.5)
        r_arg = (x/ell_x)**2 + (y/ell_y)**2
        strength = denom*h2*exp(-r_arg*denom)
        xp.append(x)
        yp.append(y)
        gp.append(strength)
np = len(xp)


xmin = min(xp)
xmax = max(xp)
ymin = min(yp)
ymax = max(yp)

xr = xmax + 5.*dh
xl = xmin - 5.*dh
yt = ymax + 5.*dh
yb = ymin - 5.*dh
nx_r = round(xr/dh)
nx_l = round(xl/dh)
ny_t = round(yt/dh)
ny_b = round(yb/dh) - 1

dhhaf = 0.5*dh
ig = 0
for ix in range(nx_l, nx
      do 110 ix = nx_l, nx_r, 1
         xx = dhhaf + ix*dh
         do 111 iy = ny_b, ny_t, 1
            yy = dhhaf + iy*dh
            ig = ig + 1
            xg(ig) = xx
            yg(ig) = yy
            gg(ig) = 0.0
            indx(ix, iy) = ig
 111     continue
 110  continue


