      real xp(nvort)
      real yp(nvort)
      real gp(nvort)
      real xn(nvort)
      real yn(nvort)
      real gn(nvort)
      real uu(nvort)
      real vv(nvort)
      real Uold(nvort)
      real Vold(nvort)
      real Gdiff(nvort)
      real GDold(nvort)
      common/vort1/xp, yp, gp
      common/vort2/xn, yn, gn
      common/vel/uu, vv
      common/oldvel/Uold, Vold
      common/diff/Gdiff, GDold
