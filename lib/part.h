C------------------ LOCATIONS --------------------
        REAL XP(Nvort),YP(Nvort),GP(Nvort)
        COMMON/vort1/XP,YP,GP
        REAL XN(Nvort),YN(Nvort),GN(Nvort)
        COMMON/vort2/XN,YN,GN
C------------------ VELOCITIES --------------------
        REAL UU(Nvort),VV(Nvort)
        COMMON/vel/UU,VV
        REAL Uold(Nvort),Vold(Nvort)
        COMMON/oldvel/Uold,Vold
C------------------ DIFFUSION --------------------
        REAL Gdiff(Nvort),GDold(Nvort)
        COMMON/diff/Gdiff,GDold
