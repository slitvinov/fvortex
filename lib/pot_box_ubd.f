c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        SUBROUTINE  POT_BOX_UBD(xtest,ytest,kbox,ub)

C  This subroutine finds the velocity potential induced at (xtest,ytest) by
C  boxes in Xbox,Ybox.

        implicit none

        include 'tree_tmp.h'

        integer kbox
        real xtest,ytest,ub

        integer ks,level
        real t2,t3,t4,t5,t6,t7,t8,t9,xx,yy,x0,y0,r2inv,rpt,fpt,cr
        real c0,r1,c1,f1,r2,c2,f2,r3,c3,f3,r4,c4,f4
        real r5,c5,f5,r6,c6,f6,r7,c7,f7
c----------------------------------------------------------

        t2 = 1./2.
        t3 = 1./3.
        t4 = 1./4.
        t5 = 1./5.
        t6 = 1./6.
        t7 = 1./7.
        t8 = 1./8.
        t9 = 1./9.
        ub = 0.0

        DO 2 ks = 1,kbox
             xx = Xtest - Xbox(ks)
             yy = Ytest - Ybox(ks)
             x0 = xx
             y0 = yy
             r2inv = 1.0/(xx*xx + yy*yy)

             level = 0
             c0 = Prbox(ks,level) * ATAN2(y0,x0)

             level = 1
             rpt = Prbox(ks,level)
             fpt = -Pibox(ks,level)
             r1 =  xx*r2inv
             f1 = -yy*r2inv
             c1 = f1*rpt + r1*fpt

             level = 2
             rpt = Prbox(ks,level)
             fpt = -Pibox(ks,level)
             r2 = r1*r1 - f1*f1
             f2 = r1*f1 + f1*r1
             c2 = (f2*rpt + r2*fpt)*t2

             level = 3
             rpt = Prbox(ks,level)
             fpt = -Pibox(ks,level)
             r3 = r2*r1 - f2*f1
             f3 = r2*f1 + f2*r1
             c3 = (f3*rpt + r3*fpt)*t3

             level = 4
             rpt = Prbox(ks,level)
             fpt = -Pibox(ks,level)
             r4 = r3*r1 - f3*f1
             f4 = r3*f1 + f3*r1
             c4 = (f4*rpt + r4*fpt)*t4

             level = 5
             rpt = Prbox(ks,level)
             fpt = -Pibox(ks,level)
             r5 = r4*r1 - f4*f1
             f5 = r4*f1 + f4*r1
             c5 = (f5*rpt + r5*fpt)*t5

             level = 6
             rpt = Prbox(ks,level)
             fpt = -Pibox(ks,level)
             r6 = r5*r1 - f5*f1
             f6 = r5*f1 + f5*r1
             c6 = (f6*rpt + r6*fpt)*t6

             level = 7
             rpt = Prbox(ks,level)
             fpt = -Pibox(ks,level)
             r7 = r6*r1 - f6*f1
             f7 = r6*f1 + f6*r1
             c7 = (f7*rpt + r7*fpt)*t7

C   Sum all the terms in the series
             cr = c1 +c2 +c3 +c4 +c5 +c6 +c7
             cr = c0 - cr

C  Calculate the potential induced by the group "k" on panel "i"
              ub = ub + cr

2        CONTINUE

         RETURN
         END
