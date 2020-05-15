c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        SUBROUTINE  INT_REST7(kp7)

c  Same as int_rest2 for level 7 boxes.

        implicit none

        include 'tree_tmp.h'
        include 'main_dim.h'
        include 'part.h'
        include 'tree9.h'

        integer limpar
        real x0,y0
        COMMON/GEOM/X0,Y0,Limpar

        integer kp7

        integer Listfar(Nhlp),Listclose(Nhlp),Listexam(Nhlp)
        integer Listpart(Nhlp),kb,ib,jb,ipar,jpar,i,kexam
        integer n4,k,id,n1,n2,np,kbb,kclose,kfar,kpart
        real xb,yb,dyopiinv
        real r61,r62,r63,r64,r65,r66,r71,r72,r73,r74,r75,r76,r77
c---------------------------------------------------------------------

        dyopiinv=1./(8.*atan(1.))

        r61 = 0.03125
        r62 = 0.06250
        r63 = 0.12500
        r64 = 0.25000
        r65 = 0.50000
        r66 = 1.0
        r71 = 0.015625
        r72 = 0.03125
        r73 = 0.06250
        r74 = 0.12500
        r75 = 0.25000
        r76 = 0.50000
        r77 = 1.0

        DO 20 kb = 1,kp7       ! All boxes Childless & Parents
          ib = IC7(kb)
          jb = JC7(kb)
          xb = XC7(kb)
          yb = YC7(kb)
          ipar = (xb-X0)/ds6 + 1
          jpar = (yb-Y0)/ds6 + 1
          do 1 i=1,kp1
            kexam=kp1
            Listexam(i)=Liststart(i)
1         continue
          CALL near_far(Nmax1,ipar,jpar,r61,IC1,JC1,kexam,Listexam,
     &                kfar,Listfar,Kclose,Listclose)

        CALL check_box(Nmax1,kclose,Listclose,kexam,Listexam,kpart
     &              ,Listpart,Ipar1Ch2,Imark1)               !NT

          CALL near_far(Nmax1,ib,jb,r71,IC1,JC1,kpart,Listpart,
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

          CALL near_far(Nmax2,ipar,jpar,r62,IC2,JC2,kexam,Listexam,
     &                kfar,Listfar,Kclose,Listclose)

        CALL check_box(Nmax2,kclose,Listclose,kexam,Listexam,kpart
     &              ,Listpart,Ipar2Ch3,Imark2)                 ! NT

          CALL near_far(Nmax2,ib,jb,r72,IC2,JC2,kpart,Listpart,
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

          CALL near_far(Nmax3,ipar,jpar,r63,IC3,JC3,kexam,Listexam,
     &                kfar,Listfar,Kclose,Listclose)

        CALL check_box(Nmax3,kclose,Listclose,kexam,Listexam,kpart
     &              ,Listpart,Ipar3Ch4,Imark3)                 ! NT
                                                
          CALL near_far(Nmax3,ib,jb,r73,IC3,JC3,kpart,Listpart,
     &                kfar,Listfar,Kclose,Listclose)

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

          CALL near_far(Nmax4,ipar,jpar,r64,IC4,JC4,kexam,Listexam,
     &                kfar,Listfar,Kclose,Listclose)

        CALL check_box(Nmax4,kclose,Listclose,kexam,Listexam,kpart
     &              ,Listpart,Ipar4Ch5,Imark4)                 ! NT
                                                
          CALL near_far(Nmax4,ib,jb,r74,IC4,JC4,kpart,Listpart,
     &                kfar,Listfar,Kclose,Listclose)

          DO  24 k = 1,kfar
             id = Listfar(k)
             n1 = NPB4(id,1)
             n2 = NPB4(id,2)
             DO 240 np = n1,n2
                n4 = n4 + 1
                XT(n4) = XN(np)
                YT(n4) = YN(np)
                GT(n4) = GN(np)
240          CONTINUE
24         CONTINUE                                                        

          CALL near_far(Nmax5,ipar,jpar,r65,IC5,JC5,kexam,Listexam,
     &                kfar,Listfar,Kclose,Listclose)

        CALL check_box(Nmax5,kclose,Listclose,kexam,Listexam,kpart
     &              ,Listpart,Ipar5Ch6,Imark5)                 ! NT
                                                
          CALL near_far(Nmax5,ib,jb,r75,IC5,JC5,kpart,Listpart,
     &                kfar,Listfar,Kclose,Listclose)

          DO  25 k = 1,kfar
             id = Listfar(k)
             n1 = NPB5(id,1)
             n2 = NPB5(id,2)
             DO 250 np = n1,n2
                n4 = n4 + 1
                XT(n4) = XN(np)
                YT(n4) = YN(np)
                GT(n4) = GN(np)
250          CONTINUE
25         CONTINUE                                                        


        CALL near_far(Nmax6,ipar,jpar,r66,IC6,JC6,Kexam,Listexam,
     &               kfar,Listfar,kclose,Listclose)

        CALL check_box(Nmax6,kclose,Listclose,
     &             kexam,Listexam,kpart,Listpart,Ipar6Ch7,Imark6)

        CALL near_far(Nmax6,ib,jb,r76,IC6,JC6,Kpart,Listpart,
     &               kfar,Listfar,kclose,Listclose)  

          DO  26 k = 1,kfar
             id = Listfar(k)
             n1 = NPB6(id,1)
             n2 = NPB6(id,2)
             DO 260 np = n1,n2
                n4 = n4 + 1
                XT(n4) = XN(np)
                YT(n4) = YN(np)
                GT(n4) = GN(np)
260          CONTINUE
26         CONTINUE

          IF (n4.EQ.0)GOTO 88
          if (n4.gt.np_max) write(*,*)'error in rest7b',n4
          CALL int_box_part(Nmax7,kb,xb,yb,n4,Br7,Bi7)

88      CALL near_far(Nmax7,ib,jb,r77,IC7,JC7,kexam,Listexam,
     &              kfar,Listfar,Kclose,Listclose)

C*CDIR$SHORTLOOP 
           DO 27 kbb = 1,kfar
             id = Listfar(kbb)
             Xbox(kbb) = XC7(id)
             Ybox(kbb) = YC7(id)

             Prbox(kbb,0) = Pr7(id,0)
             Pibox(kbb,0) = Pi7(id,0)
             Prbox(kbb,1) = Pr7(id,1)
             Pibox(kbb,1) = Pi7(id,1)
             Prbox(kbb,2) = Pr7(id,2)
             Pibox(kbb,2) = Pi7(id,2)
             Prbox(kbb,3) = Pr7(id,3)
             Pibox(kbb,3) = Pi7(id,3)
             Prbox(kbb,4) = Pr7(id,4)
             Pibox(kbb,4) = Pi7(id,4)
             Prbox(kbb,5) = Pr7(id,5)
             Pibox(kbb,5) = Pi7(id,5)
             Prbox(kbb,6) = Pr7(id,6)
             Pibox(kbb,6) = Pi7(id,6)
             Prbox(kbb,7) = Pr7(id,7)
             Pibox(kbb,7) = Pi7(id,7)
27        CONTINUE           

          if (kfar.gt.nbox_max) write(*,*)'error in rest7',kbb
          CALL int_box(Nmax7,kb,xb,yb,kfar,Br7,Bi7)

20      CONTINUE
        RETURN 
        END         
