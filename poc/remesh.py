from math import sqrt, exp, pi

Rmax = 1.1
ell_x = 6.0
ell_y = 1.0
s2 = 1e-4
ovrlp = 0.9
vortlim = 1e-03

h2 = s2 * ovrlp**2
deltax = sqrt(h2)
h2 = deltax * deltax
Nmx = int(2 * Rmax / deltax + 1)
denom = 1.0 / (0.1 * Rmax)**2

twopi = 2. * pi
twopiinv = 1. / twopi
dh = 2. * ovrlp * sqrt(s2 / pi)
dhinv = 1. / dh

cutoff = vortlim * s2 * ovrlp**2
cut_far = 10. * cutoff

xp = []
yp = []
gp = []
xg = []
yg = []
gg = []

for ix in range(1, Nmx + 1):
    for iy in range(1, Nmx + 1):
        x = -Rmax + deltax * (ix - 0.5)
        y = -Rmax + deltax * (iy - 0.5)
        r_arg = (x / ell_x)**2 + (y / ell_y)**2
        strength = denom * h2 * exp(-r_arg * denom)
        xp.append(x)
        yp.append(y)
        gp.append(strength)
xmin = min(xp)
xmax = max(xp)
ymin = min(yp)
ymax = max(yp)

xr = xmax + 5. * dh
xl = xmin - 5. * dh
yt = ymax + 5. * dh
yb = ymin - 5. * dh
nx_r = round(xr / dh)
nx_l = round(xl / dh)
ny_t = round(yt / dh)
ny_b = round(yb / dh) - 1

dhhaf = 0.5 * dh
indx = {}
for ix in range(nx_l, nx_r + 1):
    xx = dhhaf + ix * dh
    for iy in range(ny_b, ny_t + 1):
        yy = dhhaf + iy * dh
        indx[ix, iy] = len(xg)
        xg.append(xx)
        yg.append(yy)
        gg.append(0.0)

nmesh = len(xg)

cold = sum(gp)
cx = sum(gp * xp for gp, xp in zip(gp, xp))
cy = sum(gp * yp for gp, yp in zip(gp, yp))
print(f"{cold=:.16g} {cx=:.16g} {cy=:.16g}")

in_count = 0
ifar = 0

for g, x, y in zip(gp, xp, yp):
    sdist = x
    ndist = y
    ix = int(round(sdist * dhinv - 0.5))
    iy = int(round(ndist * dhinv - 0.5))
    in_count += 1
    ix0, ix1, ix2 = ix, ix - 1, ix + 1
    iy0, iy1, iy2 = iy, iy - 1, iy + 1

    k00 = indx[ix0, iy0]
    k01 = indx[ix0, iy1]
    k02 = indx[ix0, iy2]
    k10 = indx[ix1, iy0]
    k11 = indx[ix1, iy1]
    k12 = indx[ix1, iy2]
    k20 = indx[ix2, iy0]
    k21 = indx[ix2, iy1]
    k22 = indx[ix2, iy2]

    u = (sdist - xg[k00]) * dhinv
    v = (ndist - yg[k00]) * dhinv

    Fy0 = 1.0 - v * v
    Fy1 = 0.5 * v * (v - 1.0)
    Fy2 = 0.5 * v * (v + 1.0)
    Fx0 = g * (1.0 - u * u)
    Fx1 = g * 0.5 * u * (u - 1.0)
    Fx2 = g * 0.5 * u * (u + 1.0)

    gg[k00] += Fx0 * Fy0
    gg[k01] += Fx0 * Fy1
    gg[k02] += Fx0 * Fy2
    gg[k10] += Fx1 * Fy0
    gg[k11] += Fx1 * Fy1
    gg[k12] += Fx1 * Fy2
    gg[k20] += Fx2 * Fy0
    gg[k21] += Fx2 * Fy1
    gg[k22] += Fx2 * Fy2

xp = xg
yp = yg
gp = gg

# Diagnostics: circulation and impulse
cnew = sum(gp[1:])
cx = sum(gp * xp for gp, xp in zip(gp, xp))
cy = sum(gp * yp for gp, yp in zip(gp, yp))
print(f"{cold=:.16g} {cx=:.16g} {cy=:.16g}")
