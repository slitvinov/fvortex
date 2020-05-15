c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        SUBROUTINE  INT_CHLESS9(kp8,kp9)

c  Same idea as in int_chless2 but now for level 9 childless boxes.
c  For descriptive comments, go back to int_chless1&2

        implicit none

        include 'tree_tmp.h'
        include 'main_dim.h'
        include 'part.h'
        include 'tree9.h'

        integer limpar
        real x0,y0
        COMMON/GEOM/X0,Y0,Limpar

        integer kp8,kp9

        integer Listfar(Nhlp),Listclose(Nhlp),Listexam(Nhlp)
        integer Lclg(10),nns,ipar,jpar,kc,j,m,ks,km
        integer kexam,kfar,kclose,i,kb,ib,jb
        integer nb1,nb2,k,id,n1,n2,np,kfp,nn,n
        real r99,dyopiinv
        real up1,vp1,gp1
c----------------------------------------------------------------------------
        
        dyopiinv=1./(8.*atan(1.))
        
        r99 = 1.0

        DO 20 kb = 1,Kp9
          nns = 0   ! List 1 (same level)
          nn = 0    ! List 1 (finer levels)
          kfp = 0   ! List 3 
          ib = IC9(kb)
          jb = JC9(kb)
          nb1 = NPB9(kb,1)
          nb2 = NPB9(kb,2)
          ipar = (XC9(kb)-X0)/ds8 + 1
          jpar = (YC9(kb)-Y0)/ds8 + 1
           kc = 0
           DO 21 k =1, Kp8 ! Loop over boxes in parents level.
             i = IC8(k)
             j = JC8(k)
             IF( (IABS(i-ipar).GT.1).OR.(IABS(j-jpar).GT.1) ) GOTO 21
             kc = kc + 1
             Lclg(kc) = k
21         CONTINUE

           kexam = 0
           DO 22 m = 1,4
             DO 23 k = 1,kc
               ks = Lclg(k)
               km = Ipar8Ch9(ks,m)
               IF(km.EQ.0)GOTO 23
               kexam = kexam + 1
               Listexam(kexam) = km
23           CONTINUE
22         CONTINUE

          CALL near_far(Nmax9,ib,jb,r99,IC9,JC9,kexam,Listexam
     &                ,kfar,Listfar,Kclose,Listclose)

        DO 25 k=1,kclose
          id = Listclose(k)
          n1 = NPB9(id,1)
          n2 = NPB9(id,2)
          DO 250 np = n1,n2
            nns = nns + 1
            XT(nns) = XN(np)
            YT(nns) = YN(np)    ! childless boxes same level
            GT(nns) = GN(np)
250       CONTINUE
25      CONTINUE

        IF(nns.GT.np_max) WRITE(*,*)'error in int_chless8',nns
        DO 251 n = nb1,nb2
            CALL int_part1(XN(n),YN(n),GN(n),up1,vp1,gp1,nns)
            UU(n) = UU(n) + up1*dyopiinv
            VV(n) = VV(n) + vp1*dyopiinv
            gdiff(n) = gdiff(n) + gp1
251     CONTINUE
20      CONTINUE

        RETURN
        END   
