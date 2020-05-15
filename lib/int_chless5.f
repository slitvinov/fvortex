c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        SUBROUTINE  INT_CHLESS5(kp4,kchildless5)

c  Same idea as in int_chless2 but now for level 5 childless boxes.
c  For descriptive comments, go back to int_chless1&2

        implicit none

        include 'tree_tmp.h'
        include 'main_dim.h'
        include 'part.h'
        include 'tree9.h'

        integer limpar
        real x0,y0
        COMMON/GEOM/X0,Y0,Limpar

        integer kp4,kchildless5

        integer Listfar(Nhlp),Listclose(Nhlp),Listexam(Nhlp)
        integer Listpart(Nhlp),Lclg(10),nns,ipar,jpar,kc,j,m,ks,km
        integer kexam,kfar,kclose,i,kh,kb,ib,jb
        integer nb1,nb2,k,id,n1,n2,np,level,kfp,nn,kpart,n
        real r55,r56,r57,r58,r59,xnn,ynn,gnn,dyopiinv
        real up1,vp1,gp1,up2,vp2,gp2,ubox,vbox
c----------------------------------------------------------------------------
        
        dyopiinv=1./(8.*atan(1.))
        
        r55 = 1.0
        r56 = 2.0
        r57 = 4.0
        r58 = 8.0
        r59 = 16.0

        DO 20 kh = 1,Kchildless5
          nns = 0   ! List 1 (same level)
          nn = 0    ! List 1 (finer levels)
          kfp = 0   ! List 3
          kb = Ichildless5(kh)           ! box b index
          ib = IC5(kb)
          jb = JC5(kb)
          nb1 = NPB5(kb,1)
          nb2 = NPB5(kb,2)
          ipar = (XC5(kb)-X0)/ds4 + 1
          jpar = (YC5(kb)-Y0)/ds4 + 1

           kc = 0
           DO 21 k =1, Kp4 ! Loop over boxes in parents level.
             i = IC4(k)
             j = JC4(k)
             IF( (IABS(i-ipar).GT.1).OR.(IABS(j-jpar).GT.1) ) GOTO 21
             kc = kc + 1
             Lclg(kc) = k
21         CONTINUE

           kexam = 0
           DO 22 m = 1,4
             DO 23 k = 1,kc
               ks = Lclg(k)
               km = Ipar4Ch5(ks,m)
               IF(km.EQ.0)GOTO 23
               kexam = kexam + 1
               Listexam(kexam) = km
23           CONTINUE
22         CONTINUE

          CALL near_far(Nmax5,ib,jb,r55,IC5,JC5,kexam,Listexam
     &                ,kfar,Listfar,Kclose,Listclose)

        CALL check_box(Nmax5,kclose,Listclose,kexam,Listexam,kpart
     &              ,Listpart,Ipar5Ch6,Imark5)

        DO 25 k=1,kpart
          id = Listpart(k)
          n1 = NPB5(id,1)
          n2 = NPB5(id,2)
          DO 250 np = n1,n2
            nns = nns + 1
            XT(nns) = XN(np)
            YT(nns) = YN(np)    ! childless boxes same level
            GT(nns) = GN(np)
250       CONTINUE
25      CONTINUE

        IF(nns.GT.np_max) WRITE(*,*)'error in int_chless5',nns
        DO 251 n = nb1,nb2
            CALL int_part1(XN(n),YN(n),GN(n),up1,vp1,gp1,nns)
            UU(n) = UU(n) + up1*dyopiinv
            VV(n) = VV(n) + vp1*dyopiinv
            gdiff(n) = gdiff(n) + gp1
251     CONTINUE

C_____________________
        LEVEL = 6
        IF(kexam.EQ.0)GOTO 201

        CALL near_far(Nmax6,ib,jb,r56,IC6,JC6,kexam,Listexam,
     &               kfar,Listfar,kclose,Listclose)

        DO 32 k =1,kfar
            kfp = kfp + 1
            id = Listfar(k)
            Xbox(kfp) = XC6(id)
            Ybox(kfp) = YC6(id)
            Prbox(kfp,0) = Pr6(id,0)
            Pibox(kfp,0) = Pi6(id,0)
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
32      CONTINUE

         CALL check_box(Nmax6,Kclose,Listclose,kexam,Listexam,Kpart
     &               ,Listpart,Ipar6Ch7,Imark6)

         DO 33 k = 1,kpart    
            id = Listpart(k)           
            n1 = NPB6(id,1)
            n2 = NPB6(id,2)
 
            DO 330 np = n1,n2
              nn = nn + 1
              XT(nn) = XN(np)
              YT(nn) = YN(np)
              GT(nn) = GN(np)
              IT(nn) = np
330         CONTINUE
33       CONTINUE

C_____________________
        LEVEL = 7
        IF(kexam.EQ.0)GOTO 201

        CALL near_far(Nmax7,ib,jb,r57,IC7,JC7,kexam,Listexam,
     &              kfar,Listfar,kclose,Listclose)

        DO 34 k =1,kfar
            kfp = kfp + 1
            id = Listfar(k)
            Xbox(kfp) = XC7(id)
            Ybox(kfp) = YC7(id)
            Prbox(kfp,0) = Pr7(id,0)
            Pibox(kfp,0) = Pi7(id,0)
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
34      CONTINUE

         CALL check_box(Nmax7,Kclose,Listclose,kexam,Listexam,Kpart,
     &               Listpart,Ipar7Ch8,Imark7)

         DO 35 k = 1,kpart
            id = Listpart(k)           
            n1 = NPB7(id,1)
            n2 = NPB7(id,2) 
            DO 350 np = n1,n2
              nn = nn + 1
              XT(nn) = XN(np)
              YT(nn) = YN(np)
              GT(nn) = GN(np)
              IT(nn) = np
350         CONTINUE
35       CONTINUE

C_____________________
        LEVEL = 8

        CALL near_far(Nmax8,ib,jb,r58,IC8,JC8,kexam,Listexam,
     &              kfar,Listfar,kclose,Listclose)

        DO 36 k =1,kfar
            kfp = kfp + 1
            id = Listfar(k)
            Xbox(kfp) = XC8(id)
            Ybox(kfp) = YC8(id)
            Prbox(kfp,0) = Pr8(id,0)
            Pibox(kfp,0) = Pi8(id,0)
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
36      CONTINUE


         CALL check_box(Nmax8,Kclose,Listclose,kexam,Listexam,Kpart,
     &               Listpart,Ipar8Ch9,Imark8)

         DO 37 k = 1,kpart    
            id = Listpart(k)           
            n1 = NPB8(id,1)
            n2 = NPB8(id,2) 
            DO 370 np = n1,n2
              nn = nn + 1
              XT(nn) = XN(np)
              YT(nn) = YN(np)
              GT(nn) = GN(np)
              IT(nn) = np
370         CONTINUE
37       CONTINUE

C_____________________
        LEVEL = 9

        CALL near_far(Nmax9,ib,jb,r59,IC9,JC9,kexam,Listexam,
     &              kfar,Listfar,kclose,Listclose)

        DO k =1,kfar
            kfp = kfp + 1
            id = Listfar(k)
            Xbox(kfp) = XC9(id)
            Ybox(kfp) = YC9(id)
            Prbox(kfp,0) = Pr9(id,0)
            Pibox(kfp,0) = Pi9(id,0)
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
         enddo


         DO k = 1,kclose   ! All close boxes are now childless    
            id = Listclose(k)           
            n1 = NPB9(id,1)
            n2 = NPB9(id,2) 
            DO np = n1,n2
              nn = nn + 1
              XT(nn) = XN(np)
              YT(nn) = YN(np)
              GT(nn) = GN(np)
              IT(nn) = np
           enddo
         enddo


        IF(nn.GT.np_max)WRITE(*,*)'error in int_chless5p',nn
        IF(kfp.GT.nbox_max)WRITE(*,*)'error in int_chless5b',kfp
201     DO 351 n = nb1,nb2
            xnn = XN(n)
            ynn = YN(n)
            gnn = GN(n)
            CALL int_part2(gnn,xnn,ynn,up2,vp2,gp2,nn)
            CALL int_part_box(xnn,ynn,ubox,vbox,kfp)
            UU(n) = UU(n) + (up2 + ubox)*dyopiinv
            VV(n) = VV(n) + (vp2 + vbox)*dyopiinv
            gdiff(n) = gdiff(n) + gp2
351     CONTINUE

20      CONTINUE

        RETURN
        END   
