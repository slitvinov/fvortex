c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        subroutine build_tree(icheck,xbc,ybc,nn,kfp)

c  This subroutine builds the interaction tree for the boxes and particles
c  effecting the point (xbc,ybc).  It starts at the highest(coarsest)level
c  and descends down, picking out boxes to interact with and particles when
c  boxes are childless until passing all levels, at which point any boxes 
c  still too close to interact with must be done on a particle basis.

        implicit none

        include 'main_dim.h'
        include 'tree_tmp.h'
        include 'part.h'
        include 'tree9.h'

        integer limpar
        real x0,y0
        COMMON/GEOM/X0,Y0,Limpar

        integer icheck,nn,kfp
        real xbc,ybc

        integer Listfar(Nhlp),Listclose(Nhlp),Listexam(Nhlp)
        integer Listpart(Nhlp),kclose,kfar,kexam,kpart,k,m
        integer np1,np2,npt,level,id,n1,n2
        real xst,yst
c--------------------------------------------------------
           nn = 0
           kfp = 0
           kclose = 0
           kfar = 0
           kexam = 0
           kpart = 0

c  Start out with the first four boxes
           DO 432 k = 1,kp1
             Listclose(k) = Liststart(k)
432        CONTINUE
           kclose = kp1

c          LEVEL = 1
           CALL CHECK_BOX(Nmax1,kclose,Listclose,kexam,Listexam,kpart,
     &                   Listpart,Ipar1Ch2,Imark1)
           IF (Kpart.ne.0) then
             DO 12 m = 1,Kpart  ! these are all from childless level 1 boxes
               np1 = NPB1(Listpart(m),1)
               np2 = NPB1(Listpart(m),2)
               DO 121 npt = np1,np2
                 nn = nn + 1
                 XT(nn) = XN(npt)   ! will interact with these as particles
                 YT(nn) = YN(npt)
                 GT(nn) = GN(npt)
121            CONTINUE
12           CONTINUE
           endif

           LEVEL = 2
           IF(kexam.EQ.0)GOTO 9876
           xst = x0 - 0.5 * ds2
           yst = y0 - 0.5 * ds2
           CALL FAR_CLS(icheck,NMax2,Xbc,Ybc,ds2,IC2,JC2,kexam,xst,yst,
     &                    Listexam,kfar,Listfar,kclose,Listclose)
           IF(kfar.ne.0) then   ! boxes far enough away to interact with
             DO 211 k = 1,kfar
               kfp = kfp + 1
               id = Listfar(k)
               Xbox(kfp) = XC2(id)
               Ybox(kfp) = YC2(id)
               Prbox(kfp,0) = Pr2(id,0)
               Pibox(kfp,0) = Pi2(id,0)
               Prbox(kfp,1) = Pr2(id,1)
               Pibox(kfp,1) = Pi2(id,1)
               Prbox(kfp,2) = Pr2(id,2)
               Pibox(kfp,2) = Pi2(id,2)
               Prbox(kfp,3) = Pr2(id,3)
               Pibox(kfp,3) = Pi2(id,3)
               Prbox(kfp,4) = Pr2(id,4)
               Pibox(kfp,4) = Pi2(id,4)
               Prbox(kfp,5) = Pr2(id,5)
               Pibox(kfp,5) = Pi2(id,5)
               Prbox(kfp,6) = Pr2(id,6)
               Pibox(kfp,6) = Pi2(id,6)
               Prbox(kfp,7) = Pr2(id,7)
               Pibox(kfp,7) = Pi2(id,7)
211          CONTINUE
           endif

           CALL CHECK_BOX(Nmax2,kclose,Listclose,kexam,Listexam,
     &                    kpart,Listpart,Ipar2Ch3,Imark2)
           IF(kpart.ne.0) then          ! again childless boxes
             DO 22 k = 1,Kpart
               n1 = NPB2(Listpart(k),1)
               n2 = NPB2(Listpart(k),2)
               DO 221 npt = n1,n2
                 nn = nn + 1
                 XT(nn) = XN(npt)
                 YT(nn) = YN(npt)
                 GT(nn) = GN(npt)
221            CONTINUE
22           CONTINUE
           endif

           LEVEL = 3
           IF(kexam.EQ.0)GOTO 9876
           xst = x0 - 0.5 * ds3
           yst = y0 - 0.5 * ds3
           CALL FAR_CLS(icheck,NMax3,Xbc,Ybc,ds3,IC3,JC3,kexam,xst,yst,
     &                    Listexam,kfar,Listfar,kclose,Listclose)

           IF(kfar.ne.0) then
             DO 311 k = 1,kfar
               kfp = kfp + 1
               id = Listfar(k)
               Xbox(kfp) = XC3(id)
               Ybox(kfp) = YC3(id)
               Prbox(kfp,0) = Pr3(id,0)
               Prbox(kfp,1) = Pr3(id,1)
               Pibox(kfp,1) = Pi3(id,1)
               Prbox(kfp,2) = Pr3(id,2)
               Pibox(kfp,2) = Pi3(id,2)
               Prbox(kfp,3) = Pr3(id,3)
               Pibox(kfp,3) = Pi3(id,3)
               Prbox(kfp,4) = Pr3(id,4)
               Pibox(kfp,4) = Pi3(id,4)
               Prbox(kfp,5) = Pr3(id,5)
               Pibox(kfp,5) = Pi3(id,5)
               Prbox(kfp,6) = Pr3(id,6)
               Pibox(kfp,6) = Pi3(id,6)
               Prbox(kfp,7) = Pr3(id,7)
               Pibox(kfp,7) = Pi3(id,7)
311          CONTINUE
           endif

           CALL CHECK_BOX(Nmax3,kclose,Listclose,kexam,Listexam,
     &                    Kpart,Listpart,Ipar3Ch4,Imark3)

           IF(kpart.ne.0) then
             DO 32 k = 1,Kpart
               n1 = NPB3(Listpart(k),1)
               n2 = NPB3(Listpart(k),2)
               DO 321 npt = n1,n2
                 nn = nn + 1
                 XT(nn) = XN(npt)
                 YT(nn) = YN(npt)
                 GT(nn) = GN(npt)
321            CONTINUE
32           CONTINUE
           endif

           LEVEL = 4
           IF(kexam.EQ.0)GOTO 9876
           xst = x0 - 0.5 * ds4
           yst = y0 - 0.5 * ds4
           CALL FAR_CLS(icheck,Nmax4,Xbc,Ybc,ds4,IC4,JC4,kexam,xst,yst,
     &                    Listexam,kfar,Listfar,kclose,Listclose)

           IF(kfar.ne.0) then
             DO 411 k = 1,kfar
               kfp = kfp + 1
               id = Listfar(k)
               Xbox(kfp) = XC4(id)
               Ybox(kfp) = YC4(id)
               Prbox(kfp,0) = Pr4(id,0)
               Prbox(kfp,1) = Pr4(id,1)
               Pibox(kfp,1) = Pi4(id,1)
               Prbox(kfp,2) = Pr4(id,2)
               Pibox(kfp,2) = Pi4(id,2)
               Prbox(kfp,3) = Pr4(id,3)
               Pibox(kfp,3) = Pi4(id,3)
               Prbox(kfp,4) = Pr4(id,4)
               Pibox(kfp,4) = Pi4(id,4)
               Prbox(kfp,5) = Pr4(id,5)
               Pibox(kfp,5) = Pi4(id,5)
               Prbox(kfp,6) = Pr4(id,6)
               Pibox(kfp,6) = Pi4(id,6)
               Prbox(kfp,7) = Pr4(id,7)
               Pibox(kfp,7) = Pi4(id,7)
411          CONTINUE
           endif

           CALL CHECK_BOX(NMax4,kclose,Listclose,kexam,Listexam,
     &                    kpart,Listpart,Ipar4Ch5,Imark4)

           IF(kpart.ne.0) then
             DO 43 k = 1,Kpart
               n1 = NPB4(Listpart(k),1)
               n2 = NPB4(Listpart(k),2)
               DO 431 npt = n1,n2
                 nn = nn + 1
                 XT(nn) = XN(npt)
                 YT(nn) = YN(npt)
                 GT(nn) = GN(npt)
431            CONTINUE
43           CONTINUE
           endif

           LEVEL = 5
           IF(kexam.EQ.0)GOTO 9876
           xst = x0 - 0.5 * ds5
           yst = y0 - 0.5 * ds5
           CALL FAR_CLS(icheck,Nmax5,xbc,ybc,ds5,IC5,JC5,kexam,xst,yst,
     &                    Listexam,kfar,Listfar,kclose,Listclose)

           IF(kfar.ne.0) then
             DO 511 k = 1,kfar
               kfp = kfp + 1
               id = Listfar(k)
               Xbox(kfp) = XC5(id)
               Ybox(kfp) = YC5(id)
               Prbox(kfp,0) = Pr5(id,0)
               Prbox(kfp,1) = Pr5(id,1)
               Pibox(kfp,1) = Pi5(id,1)
               Prbox(kfp,2) = Pr5(id,2)
               Pibox(kfp,2) = Pi5(id,2)
               Prbox(kfp,3) = Pr5(id,3)
               Pibox(kfp,3) = Pi5(id,3)
               Prbox(kfp,4) = Pr5(id,4)
               Pibox(kfp,4) = Pi5(id,4)
               Prbox(kfp,5) = Pr5(id,5)
               Pibox(kfp,5) = Pi5(id,5)
               Prbox(kfp,6) = Pr5(id,6)
               Pibox(kfp,6) = Pi5(id,6)
               Prbox(kfp,7) = Pr5(id,7)
               Pibox(kfp,7) = Pi5(id,7)
511          CONTINUE
           endif

           CALL CHECK_BOX(Nmax5,kclose,Listclose,kexam,
     &            Listexam,kpart,Listpart,Ipar5Ch6,Imark5)

           IF(kpart.ne.0) then
             DO 52 k = 1,Kpart
               n1 = NPB5(Listpart(k),1)
               n2 = NPB5(Listpart(k),2)
               DO 521 npt = n1,n2
                 nn = nn + 1
                 XT(nn) = XN(npt)
                 YT(nn) = YN(npt)
                 GT(nn) = GN(npt)
521            CONTINUE
52           CONTINUE
           endif
 
           LEVEL = 6
           IF(kexam.EQ.0)GOTO 9876
           xst = x0 - 0.5 * ds6
           yst = y0 - 0.5 * ds6
           CALL FAR_CLS(icheck,Nmax6,Xbc,Ybc,ds6,IC6,JC6,kexam,xst,yst,
     &                    Listexam,kfar,Listfar,kclose,Listclose)

            IF(kfar.ne.0) then
             DO 611 k = 1,kfar
               kfp = kfp + 1
               id = Listfar(k)
               Xbox(kfp) = XC6(id)
               Ybox(kfp) = YC6(id)
               Prbox(kfp,0) = Pr6(id,0)
               Prbox(kfp,1) = Pr6(id,1)
               Pibox(kfp,1) = Pi6(id,1)
               Prbox(kfp,2) = Pr6(id,2)
               Pibox(kfp,2) = Pi6(id,2)
               Prbox(kfp,3) = Pr6(id,3)
               Pibox(kfp,3) = Pi6(id,3)
               Prbox(kfp,4) = Pr6(id,4)
               Pibox(kfp,4) = Pi6(id,4)
               Prbox(kfp,5) = Pr6(id,5)
               Pibox(kfp,5) = Pi6(id,5)
               Prbox(kfp,6) = Pr6(id,6)
               Pibox(kfp,6) = Pi6(id,6)
               Prbox(kfp,7) = Pr6(id,7)
               Pibox(kfp,7) = Pi6(id,7)
611          CONTINUE
           endif

           CALL CHECK_BOX(Nmax6,kclose,Listclose,kexam,Listexam,
     &                    kpart,Listpart,Ipar6Ch7,Imark6)

           IF(kpart.ne.0) then
             DO 62 k = 1,Kpart
               n1 = NPB6(Listpart(k),1)
               n2 = NPB6(Listpart(k),2)
               DO 621 npt = n1,n2
                 nn = nn + 1
                 XT(nn) = XN(npt)
                 YT(nn) = YN(npt)
                 GT(nn) = GN(npt)
621            CONTINUE
62           CONTINUE
           endif

           LEVEL = 7
           IF(kexam.EQ.0)GOTO 9876
           xst = x0 - 0.5 * ds7
           yst = y0 - 0.5 * ds7
           CALL FAR_CLS(icheck,Nmax7,Xbc,Ybc,ds7,IC7,JC7,kexam,xst,yst,
     &                    Listexam,kfar,Listfar,kclose,Listclose)

           IF(kfar.ne.0) then
             DO 711 k = 1,kfar
               kfp = kfp + 1
               id = Listfar(k)
               Xbox(kfp) = XC7(id)
               Ybox(kfp) = YC7(id)
               Prbox(kfp,0) = Pr7(id,0)
               Prbox(kfp,1) = Pr7(id,1)
               Pibox(kfp,1) = Pi7(id,1)
               Prbox(kfp,2) = Pr7(id,2)
               Pibox(kfp,2) = Pi7(id,2)
               Prbox(kfp,3) = Pr7(id,3)
               Pibox(kfp,3) = Pi7(id,3)
               Prbox(kfp,4) = Pr7(id,4)
               Pibox(kfp,4) = Pi7(id,4)
               Prbox(kfp,5) = Pr7(id,5)
               Pibox(kfp,5) = Pi7(id,5)
               Prbox(kfp,6) = Pr7(id,6)
               Pibox(kfp,6) = Pi7(id,6)
               Prbox(kfp,7) = Pr7(id,7)
               Pibox(kfp,7) = Pi7(id,7)
711          CONTINUE
           endif

           CALL CHECK_BOX(Nmax7,kclose,Listclose,kexam,Listexam,
     &                    kpart,Listpart,Ipar7Ch8,Imark7)

           IF(kpart.ne.0) then
             DO 72 k = 1,Kpart
               n1 = NPB7(Listpart(k),1)
               n2 = NPB7(Listpart(k),2)
               DO 721 npt = n1,n2
                 nn = nn + 1
                 XT(nn) = XN(npt)
                 YT(nn) = YN(npt)
                 GT(nn) = GN(npt)
721            CONTINUE
72           CONTINUE
           endif



           LEVEL = 8
           IF(kexam.EQ.0)GOTO 9876
           xst = x0 - 0.5 * ds8
           yst = y0 - 0.5 * ds8
           CALL FAR_CLS(icheck,Nmax8,Xbc,Ybc,ds8,IC8,JC8,kexam,xst,yst,
     &                    Listexam,kfar,Listfar,kclose,Listclose)

           IF(kfar.ne.0) then
             DO 811 k = 1,kfar
               kfp = kfp + 1
               id = Listfar(k)
               Xbox(kfp) = XC8(id)
               Ybox(kfp) = YC8(id)
               Prbox(kfp,0) = Pr8(id,0)
               Prbox(kfp,1) = Pr8(id,1)
               Pibox(kfp,1) = Pi8(id,1)
               Prbox(kfp,2) = Pr8(id,2)
               Pibox(kfp,2) = Pi8(id,2)
               Prbox(kfp,3) = Pr8(id,3)
               Pibox(kfp,3) = Pi8(id,3)
               Prbox(kfp,4) = Pr8(id,4)
               Pibox(kfp,4) = Pi8(id,4)
               Prbox(kfp,5) = Pr8(id,5)
               Pibox(kfp,5) = Pi8(id,5)
               Prbox(kfp,6) = Pr8(id,6)
               Pibox(kfp,6) = Pi8(id,6)
               Prbox(kfp,7) = Pr8(id,7)
               Pibox(kfp,7) = Pi8(id,7)
811          CONTINUE
           endif

           CALL CHECK_BOX(Nmax8,kclose,Listclose,kexam,Listexam,
     &                    kpart,Listpart,Ipar8Ch9,Imark8)

           IF(kpart.ne.0) then
             DO 82 k = 1,Kpart
               n1 = NPB8(Listpart(k),1)
               n2 = NPB8(Listpart(k),2)
               DO 821 npt = n1,n2
                 nn = nn + 1
                 XT(nn) = XN(npt)
                 YT(nn) = YN(npt)
                 GT(nn) = GN(npt)
821            CONTINUE
82           CONTINUE
           endif


           LEVEL = 9
           IF(kexam.EQ.0)GOTO 9876
           xst = x0 - 0.5 * ds9
           yst = y0 - 0.5 * ds9
           CALL FAR_CLS(icheck,Nmax9,Xbc,Ybc,ds9,IC9,JC9,kexam,xst,yst,
     &                    Listexam,kfar,Listfar,kclose,Listclose)

           IF(kfar.ne.0) then
             DO 911 k = 1,kfar
               kfp = kfp + 1
               id = Listfar(k)
               Xbox(kfp) = XC9(id)
               Ybox(kfp) = YC9(id)
               Prbox(kfp,0) = Pr9(id,0)
               Prbox(kfp,1) = Pr9(id,1)
               Pibox(kfp,1) = Pi9(id,1)
               Prbox(kfp,2) = Pr9(id,2)
               Pibox(kfp,2) = Pi9(id,2)
               Prbox(kfp,3) = Pr9(id,3)
               Pibox(kfp,3) = Pi9(id,3)
               Prbox(kfp,4) = Pr9(id,4)
               Pibox(kfp,4) = Pi9(id,4)
               Prbox(kfp,5) = Pr9(id,5)
               Pibox(kfp,5) = Pi9(id,5)
               Prbox(kfp,6) = Pr9(id,6)
               Pibox(kfp,6) = Pi9(id,6)
               Prbox(kfp,7) = Pr9(id,7)
               Pibox(kfp,7) = Pi9(id,7)
911          CONTINUE
           endif

C  No more checking here. All close boxes are cosidered childless

           DO 92 k = 1,Kclose
             n1 = NPB9(Listclose(k),1)
             n2 = NPB9(Listclose(k),2)
             DO 921 npt = n1,n2
               nn = nn + 1
               XT(nn) = XN(npt)
               YT(nn) = YN(npt)
               GT(nn) = GN(npt)
921          CONTINUE
92         CONTINUE

9876    CONTINUE

        RETURN
        END
