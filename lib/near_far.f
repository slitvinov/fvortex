c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        SUBROUTINE near_far(Nmax,ib,jb,r,IC,JC,kexam,Listexam,
     &                       kfar,Listfar,kclose,Listclose)

C  This  subroutine finds all the far & close boxes at a certain
C  level associated with a certain particle.

        implicit none

        include 'tree_tmp.h'

        integer nmax,ib,jb,ic(nmax),jc(nmax),kexam,kfar,kclose
        integer Listclose(Nhlp),Listexam(Nhlp),Listfar(Nhlp)
        real r

        integer k,ks
        real fi,fj,cr,si,sj
c------------------------------------------------------------------------------ 

        if (kexam.gt.nhlp) write(*,*)'error in near_far,',kexam

        fi = r*(ib-.5) + .5
        fj = r*(jb-.5) + .5
        cr = .500001*(1.+ r)
        kfar = 0
        kclose = 0

        DO 2 k = 1,kexam
           ks = Listexam(k)
           si = IC(ks)
           sj = JC(ks)
           IF( (ABS(fi-si).LT.cr).AND.(ABS(fj-sj).LT.cr) ) THEN
             kclose = kclose + 1
             Listclose(kclose) = ks        ! close
            ELSE
             kfar = kfar + 1
             Listfar(kfar) = ks            ! far away
            ENDIF
2        CONTINUE
         RETURN
         END
