c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        SUBROUTINE  INT_REST9(kp9)

c  Same as int_rest2 for level 9 boxes.

        implicit none

        include 'tree_tmp.h'
        include 'main_dim.h'
        include 'part.h'
        include 'tree9.h'

        integer limpar
        real x0,y0
        COMMON/GEOM/X0,Y0,Limpar

        integer kp9

        integer Listfar(Nhlp),Listclose(Nhlp),Listexam(Nhlp)
        integer Listpart(Nhlp),kb,ib,jb,ipar,jpar,i,kexam
        integer n4,k,id,n1,n2,np,kbb,kclose,kfar,kpart
        real xb,yb,dyopiinv
        real r81,r82,r83,r84,r85,r86,r87,r88
        real r91,r92,r93,r94,r95,r96,r97,r98,r99
c---------------------------------------------------------------------

        dyopiinv=1./(8.*atan(1.))

        r81 = 0.0078125
        r82 = 0.015625
        r83 = 0.03125
        r84 = 0.06250
        r85 = 0.12500
        r86 = 0.25000
        r87 = 0.50000
        r88 = 1.0
        r91 = 0.00390625
        r92 = 0.0078125
        r93 = 0.015625
        r94 = 0.03125
        r95 = 0.06250
        r96 = 0.12500
        r97 = 0.25000
        r98 = 0.50000
        r99 = 1.0

        DO 20 kb = 1,kp9       ! All boxes Childless & Parents
          ib = IC9(kb)
          jb = JC9(kb)
          xb = XC9(kb)
          yb = YC9(kb)
          ipar = (xb-X0)/ds8 + 1
          jpar = (yb-Y0)/ds8 + 1
          do 1 i=1,kp1
            kexam=kp1
            Listexam(i)=Liststart(i)
1         continue
          CALL near_far(Nmax1,ipar,jpar,r81,IC1,JC1,kexam,Listexam,
     &                kfar,Listfar,Kclose,Listclose)

        CALL check_box(Nmax1,kclose,Listclose,kexam,Listexam,kpart
     &              ,Listpart,Ipar1Ch2,Imark1)               !NT

          CALL near_far(Nmax1,ib,jb,r91,IC1,JC1,kpart,Listpart,
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

          CALL near_far(Nmax2,ipar,jpar,r82,IC2,JC2,kexam,Listexam,
     &                kfar,Listfar,Kclose,Listclose)

        CALL check_box(Nmax2,kclose,Listclose,kexam,Listexam,kpart
     &              ,Listpart,Ipar2Ch3,Imark2)                 ! NT

          CALL near_far(Nmax2,ib,jb,r92,IC2,JC2,kpart,Listpart,
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

          CALL near_far(Nmax3,ipar,jpar,r83,IC3,JC3,kexam,Listexam,
     &                kfar,Listfar,Kclose,Listclose)

        CALL check_box(Nmax3,kclose,Listclose,kexam,Listexam,kpart
     &              ,Listpart,Ipar3Ch4,Imark3)                 ! NT
                                                
          CALL near_far(Nmax3,ib,jb,r93,IC3,JC3,kpart,Listpart,
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

          CALL near_far(Nmax4,ipar,jpar,r84,IC4,JC4,kexam,Listexam,
     &                kfar,Listfar,Kclose,Listclose)

        CALL check_box(Nmax4,kclose,Listclose,kexam,Listexam,kpart
     &              ,Listpart,Ipar4Ch5,Imark4)                 ! NT
                                                
          CALL near_far(Nmax4,ib,jb,r94,IC4,JC4,kpart,Listpart,
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

          CALL near_far(Nmax5,ipar,jpar,r85,IC5,JC5,kexam,Listexam,
     &                kfar,Listfar,Kclose,Listclose)

        CALL check_box(Nmax5,kclose,Listclose,kexam,Listexam,kpart
     &              ,Listpart,Ipar5Ch6,Imark5)                 ! NT
                                                
          CALL near_far(Nmax5,ib,jb,r95,IC5,JC5,kpart,Listpart,
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


        CALL near_far(Nmax6,ipar,jpar,r86,IC6,JC6,Kexam,Listexam,
     &               kfar,Listfar,kclose,Listclose)

        CALL check_box(Nmax6,kclose,Listclose,
     &             kexam,Listexam,kpart,Listpart,Ipar6Ch7,Imark6)

        CALL near_far(Nmax6,ib,jb,r96,IC6,JC6,Kpart,Listpart,
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


        CALL near_far(Nmax7,ipar,jpar,r87,IC7,JC7,Kexam,Listexam,
     &               kfar,Listfar,kclose,Listclose)

        CALL check_box(Nmax7,kclose,Listclose,
     &             kexam,Listexam,kpart,Listpart,Ipar7Ch8,Imark7)

        CALL near_far(Nmax7,ib,jb,r97,IC7,JC7,Kpart,Listpart,
     &               kfar,Listfar,kclose,Listclose)  

          DO  28 k = 1,kfar
             id = Listfar(k)
             n1 = NPB7(id,1)
             n2 = NPB7(id,2)
             DO 280 np = n1,n2
                n4 = n4 + 1
                XT(n4) = XN(np)
                YT(n4) = YN(np)
                GT(n4) = GN(np)
280          CONTINUE
28         CONTINUE

        CALL near_far(Nmax8,ipar,jpar,r88,IC8,JC8,Kexam,Listexam,
     &               kfar,Listfar,kclose,Listclose)

        CALL check_box(Nmax8,kclose,Listclose,
     &             kexam,Listexam,kpart,Listpart,Ipar8Ch9,Imark8)

        CALL near_far(Nmax8,ib,jb,r98,IC8,JC8,Kpart,Listpart,
     &               kfar,Listfar,kclose,Listclose)  

          DO  29 k = 1,kfar
             id = Listfar(k)
             n1 = NPB8(id,1)
             n2 = NPB8(id,2)
             DO 290 np = n1,n2
                n4 = n4 + 1
                XT(n4) = XN(np)
                YT(n4) = YN(np)
                GT(n4) = GN(np)
290          CONTINUE
 29       CONTINUE


          IF (n4.EQ.0)GOTO 88
          if (n4.gt.np_max) write(*,*)'error in rest9b',n4
          CALL int_box_part(Nmax9,kb,xb,yb,n4,Br9,Bi9)

88      CALL near_far(Nmax9,ib,jb,r99,IC9,JC9,kexam,Listexam,
     &              kfar,Listfar,Kclose,Listclose)

C*CDIR$SHORTLOOP 
           DO 27 kbb = 1,kfar
             id = Listfar(kbb)
             Xbox(kbb) = XC9(id)
             Ybox(kbb) = YC9(id)

             Prbox(kbb,0) = Pr9(id,0)
             Pibox(kbb,0) = Pi9(id,0)
             Prbox(kbb,1) = Pr9(id,1)
             Pibox(kbb,1) = Pi9(id,1)
             Prbox(kbb,2) = Pr9(id,2)
             Pibox(kbb,2) = Pi9(id,2)
             Prbox(kbb,3) = Pr9(id,3)
             Pibox(kbb,3) = Pi9(id,3)
             Prbox(kbb,4) = Pr9(id,4)
             Pibox(kbb,4) = Pi9(id,4)
             Prbox(kbb,5) = Pr9(id,5)
             Pibox(kbb,5) = Pi9(id,5)
             Prbox(kbb,6) = Pr9(id,6)
             Pibox(kbb,6) = Pi9(id,6)
             Prbox(kbb,7) = Pr9(id,7)
             Pibox(kbb,7) = Pi9(id,7)
27        CONTINUE           

          if (kfar.gt.nbox_max) write(*,*)'error in rest9',kbb
          CALL int_box(Nmax9,kb,xb,yb,kfar,Br9,Bi9)

20      CONTINUE
        RETURN 
        END         
