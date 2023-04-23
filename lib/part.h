      real xp(nvort), yp(nvort), gp(nvort)
      real xn(nvort), yn(nvort), gn(nvort)
      real uu(nvort), vv(nvort)
      real Uold(nvort), Vold(nvort)
      real Gdiff(nvort), GDold(nvort)
      common/vort1/xp, yp, gp
      common/vort2/xn, yn, gn
      common/vel/uu, vv
      common/oldvel/Uold, Vold
      common/diff/Gdiff, GDold
