!-----------------LOCATIONS --------------------
      real :: xp(nvort), yp(nvort), gp(nvort)
      common/vort1/xp, yp, gp
      real :: xn(nvort), yn(nvort), gn(nvort)
      common/vort2/xn, yn, gn
!-----------------VELOCITIES --------------------
      real :: uu(nvort), vv(nvort)
      common/vel/uu, vv
      real :: Uold(nvort), Vold(nvort)
      common/oldvel/Uold, Vold
!-----------------DIFFUSION --------------------
      real :: Gdiff(nvort), GDold(nvort)
      common/diff/Gdiff, GDold
