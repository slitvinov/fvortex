c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       SUBROUTINE CHECK_BOX(Nmax1,kclose,Listclose,kexam,Listexam,
     &                      kpart,Listpart,Ipar1Ch2,Imark1)
C
C  This  subroutine examines the nearby boxes of a certain particle.
C  If they are parents then their children are placed in the list
C  *Listexam* so that they are eaxmined at the lower level.
C  If they are childless then their particles are placed in list
C  *Listpart* so that they interact directly with the particle.
C

       implicit none

       include 'tree_tmp.h'

       integer nmax1,kclose,kexam,kpart
       integer Listclose(Nhlp),Listexam(Nhlp),Listpart(Nhlp)
       integer Ipar1Ch2(Nmax1,4),Imark1(Nmax1)

       integer k,ks,i,m,kcheck
       integer Icheck4(Nhlp)
c------------------------------------------------------

        if(kclose.gt.nhlp) write(*,*)'error in check_box,',kclose
        kexam = 0
        kpart = 0
        kcheck = 0

        DO 2 k = 1,kclose
           ks = Listclose(k)
           IF (Imark1(ks).EQ.0)THEN  ! childless
            kpart = kpart + 1
            Listpart(kpart) = ks
           ELSE
            kcheck = kcheck + 1
            Icheck4(kcheck) = ks
           ENDIF
2        CONTINUE

         DO 3 i=1,4
           DO 30 k = 1,Kcheck
             ks = Icheck4(k)
             m = Ipar1Ch2(ks,i)
             IF(m.NE.0)THEN     ! box not empty, examine at lower
               kexam = kexam + 1
               Listexam(kexam) = m
             ENDIF
30         CONTINUE
3        CONTINUE


         RETURN
         END
