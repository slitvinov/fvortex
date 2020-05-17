!------------------ LOCATIONS --------------------
real :: xp(Nvort), yp(Nvort), gp(Nvort)
common/vort1/xp, yp, gp
real :: xn(Nvort), yn(Nvort), gn(Nvort)
common/vort2/xn, yn, gn
!------------------ VELOCITIES --------------------
real :: uu(Nvort), vv(Nvort)
common/vel/uu, vv
real :: Uold(Nvort), Vold(Nvort)
common/oldvel/Uold, Vold
!------------------ DIFFUSION --------------------
real :: Gdiff(Nvort), GDold(Nvort)
common/diff/Gdiff, GDold
