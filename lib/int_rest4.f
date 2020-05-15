c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        SUBROUTINE  INT_REST4(kp4)

c  Same as int_rest2 for level 4 boxes.

        implicit none

        include 'tree_tmp.h'
        include 'main_dim.h'
        include 'part.h'
        include 'tree9.h'

        integer limpar
        real x0,y0
        COMMON/GEOM/X0,Y0,Limpar

        integer kp4

        integer Listfar(Nhlp),Listclose(Nhlp),Listexam(Nhlp)
        integer Listpart(Nhlp),kb,ib,jb,ipar,jpar,i,kexam
        integer n4,k,id,n1,n2,np,kbb,kclose,kfar,kpart
        real dyopiinv,r31,r32,r33,r41,r42,r43,r44,xb,yb
c---------------------------------------------------------------------

        dyopiinv=1./(8.*atan(1.))

        r31 = 0.25
        r32 = 0.50
        r33 = 1.0
        r41 = 0.125
        r42 = 0.25
        r43 = 0.50
        r44 = 1.0

        DO 20 kb = 1,kp4       ! All boxes Childless & Parents
          ib = IC4(kb)
          jb = JC4(kb)
          xb = XC4(kb)
          yb = YC4(kb)
          ipar = (xb-X0)/ds3 + 1
          jpar = (yb-Y0)/ds3 + 1
          do 1 i=1,kp1
            kexam=kp1
            Listexam(i)=Liststart(i)
1         continue

          CALL near_far(Nmax1,ipar,jpar,r31,IC1,JC1,kexam,Listexam,
     &                kfar,Listfar,Kclose,Listclose)

        CALL check_box(Nmax1,kclose,Listclose,kexam,Listexam,kpart
     &              ,Listpart,Ipar1Ch2,Imark1)               !NT

          CALL near_far(Nmax1,ib,jb,r41,IC1,JC1,kpart,Listpart,
     &                kfar,Listfar,Kclose,Listclose)

          n4 = 0
          DO  21 k = 1,kfar
             id = Listfar(k)
             n1 = NPB1(id,1)
             n2 = NPB1(id,2)
             DO 210 np = n1,n2
                n4 = n4 + 1
                XT(n4) = XN(np)
                YT(n4) = YN(np)
                GT(n4) = GN(np)
210          CONTINUE
21         CONTINUE

          CALL near_far(Nmax2,ipar,jpar,r32,IC2,JC2,kexam,Listexam,
     &                kfar,Listfar,Kclose,Listclose)

        CALL check_box(Nmax2,kclose,Listclose,kexam,Listexam,kpart
     &              ,Listpart,Ipar2Ch3,Imark2)                 ! NT

          CALL near_far(Nmax2,ib,jb,r42,IC2,JC2,kpart,Listpart,
     &                kfar,Listfar,Kclose,Listclose)

          DO  22 k = 1,kfar
             id = Listfar(k)
             n1 = NPB2(id,1)
             n2 = NPB2(id,2)
             DO 220 np = n1,n2
                n4 = n4 + 1
                XT(n4) = XN(np)
                YT(n4) = YN(np)
                GT(n4) = GN(np)
220          CONTINUE
22         CONTINUE

        CALL near_far(Nmax3,ipar,jpar,r33,IC3,JC3,Kexam,Listexam,
     &               kfar,Listfar,kclose,Listclose)

        CALL check_box(Nmax3,kclose,Listclose,
     &             kexam,Listexam,kpart,Listpart,Ipar3Ch4,Imark3)

        CALL near_far(Nmax3,ib,jb,r43,IC3,JC3,Kpart,Listpart,
     &               kfar,Listfar,kclose,Listclose)  

          DO  23 k = 1,kfar
             id = Listfar(k)
             n1 = NPB3(id,1)
             n2 = NPB3(id,2)
             DO 230 np = n1,n2
                n4 = n4 + 1
                XT(n4) = XN(np)
                YT(n4) = YN(np)
                GT(n4) = GN(np)
230          CONTINUE
23         CONTINUE

          IF (n4.EQ.0)GOTO 88
          if (n4.gt.np_max) write(*,*)'error in rest4b',n4
          CALL int_box_part(Nmax4,kb,xb,yb,n4,Br4,Bi4)

88      CALL near_far(Nmax4,ib,jb,r44,IC4,JC4,kexam,Listexam,
     &              kfar,Listfar,Kclose,Listclose)

C*CDIR$SHORTLOOP 
           DO 25 kbb = 1,kfar
             id = Listfar(kbb)
             Xbox(kbb) = XC4(id)
             Ybox(kbb) = YC4(id)
             Prbox(kbb,0) = Pr4(id,0)
             Pibox(kbb,0) = Pi4(id,0)
             Prbox(kbb,1) = Pr4(id,1)
             Pibox(kbb,1) = Pi4(id,1)
             Prbox(kbb,2) = Pr4(id,2)
             Pibox(kbb,2) = Pi4(id,2)
             Prbox(kbb,3) = Pr4(id,3)
             Pibox(kbb,3) = Pi4(id,3)
             Prbox(kbb,4) = Pr4(id,4)
             Pibox(kbb,4) = Pi4(id,4)
             Prbox(kbb,5) = Pr4(id,5)
             Pibox(kbb,5) = Pi4(id,5)
             Prbox(kbb,6) = Pr4(id,6)
             Pibox(kbb,6) = Pi4(id,6)
             Prbox(kbb,7) = Pr4(id,7)
             Pibox(kbb,7) = Pi4(id,7)
25        CONTINUE

          if (kfar.gt.nbox_max) write(*,*)'error in rest4',kbb
          CALL int_box(Nmax4,kb,xb,yb,kfar,Br4,Bi4)

20      CONTINUE
        RETURN 
        END                                                                    
