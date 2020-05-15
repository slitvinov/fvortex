        SUBROUTINE  CONDIFF(Npart,islip,visc_rmax,istats)

C  This subroutine is the driver of an implementation of an O(N) method for
C  fast computation of velocities and diffusion of a field of vortex blobs
C  using a box-box scheme.

C*****************************************************************************
C  Coming in, data should be in XP,YP,GP,UU,VV.
C  At the end the particles have locations at XN,YN, GN                      *
C    and the initial velocity field is stored in: Uold,Vold.                 *
C  The new velocity field is placed in: UU,VV.                               *
C  The changed to the circulations due to diffusion are put in Gdiff.        *
C  The minimum number of particles for a box is given by Limpar.             *
C                                                                            *
C*****************************************************************************

        implicit none

        include 'main_dim.h'
        include 'part.h'
        include 'tree9.h'

        integer limpar
        real x0,y0
        COMMON/GEOM/X0,Y0,Limpar

        real visc_cutoff
        common/cutoff/visc_cutoff

        integer npart,islip,istats
        real visc_rmax

        integer kp2,kp3,kp4,kp5,kp6,kp7,kp8,kp9
        integer kchildless1,kchildless2,kchildless3
        integer kchildless4,kchildless5,kchildless6
        integer kchildless7,kchildless8
        integer kparent1,kparent2,kparent3,kparent4
        integer kparent5,kparent6,kparent7,kparent8
        integer i,j
        integer Ich2Par1(nmax2),Ich3Par2(nmax3),Ich4Par3(nmax4)
        integer Ich5Par4(nmax5),Ich6Par5(nmax6),Ich7par6(nmax7)
        integer Ich8par7(nmax8),Ich9par8(nmax9)
        real s0,xmin,xmax,ymin,ymax
        real gr1(nmax1,0:7),gi1(nmax1,0:7)
        real gr2(nmax2,0:7),gi2(nmax2,0:7)
        real gr3(nmax3,0:7),gi3(nmax3,0:7)
        real gr4(nmax4,0:7),gi4(nmax4,0:7)
        real gr5(nmax5,0:7),gi5(nmax5,0:7)
        real gr6(nmax6,0:7),gi6(nmax6,0:7)
        real gr7(nmax7,0:7),gi7(nmax7,0:7)
        real gr8(nmax8,0:7),gi8(nmax8,0:7)
        real gr9(nmax9,0:7),gi9(nmax9,0:7)
c-----------------------------------------------------------------------------

C_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.
C                                                                    _.
C         STAGE 0 : Zero all relevant arrays.                        _.
C_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.

        visc_cutoff=visc_rmax*visc_rmax
        CALL ZEROALL   ! handles common blocks

        kp1=0.
        kp2=0.
        kp3=0.
        kp4=0.
        kp5=0.
        kp6=0.
        kp7=0.
        kp8=0.
        kp9=0.

        kchildless1=0.
        kchildless2=0.
        kchildless3=0.
        kchildless4=0.
        kchildless5=0.
        kchildless6=0.
        kchildless7=0.
        kchildless8=0.

        kparent1=0.
        kparent2=0.
        kparent3=0.
        kparent4=0.
        kparent5=0.
        kparent6=0.
        kparent7=0.
        kparent8=0.

        do i=1,nmax2
           Ich2par1(i)=0.
        enddo
        do i=1,nmax3
           Ich3par2(i)=0.
        enddo
        do i=1,nmax4
           Ich4par3(i)=0.
        enddo
        do i=1,nmax5
           Ich5par4(i)=0.
        enddo
        do i=1,nmax6
           Ich6par5(i)=0.
        enddo
        do i=1,nmax7
           Ich7par6(i)=0.
        enddo
        do i=1,nmax8
           Ich8par7(i)=0.
        enddo
        do i=1,nmax9
           Ich9par8(i)=0.
        enddo

        do i=0,7
           do j=1,nmax1
              Gr1(j,i) = 0.0
              Gi1(j,i) = 0.0
           enddo
           do j=1,nmax2
              Gr2(j,i) = 0.0
              Gi2(j,i) = 0.0
           enddo
           do j=1,nmax3
              Gr3(j,i) = 0.0
              Gi3(j,i) = 0.0
           enddo
           do j=1,nmax4
              Gr4(j,i) = 0.0
              Gi4(j,i) = 0.0
           enddo
           do j=1,nmax5
              Gr5(j,i) = 0.0
              Gi5(j,i) = 0.0
           enddo
           do j=1,nmax6
              Gr6(j,i) = 0.0
              Gi6(j,i) = 0.0
           enddo
           do j=1,nmax7
              Gr7(j,i) = 0.0
              Gi7(j,i) = 0.0
           enddo
           do j=1,nmax8
              Gr8(j,i) = 0.0
              Gi8(j,i) = 0.0
           enddo
           do j=1,nmax9
              Gr9(j,i) = 0.0
              Gi9(j,i) = 0.0
           enddo
        enddo


C_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.
C                                                                    _.
C         STAGE 1 : Divide the domain containing particles           _.
C                   into a hierarchy of cells.                       _.
C_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.

        
C Determine the  region  of  the main square

        CALL BOX_DIM(Npart,Xmin,Xmax,Ymin,Ymax)
        S0 = MAX( ABS(Xmax-Xmin),ABS(Ymax-Ymin) )  ! Side of square
        X0 = xmin -0.01*s0                   ! Coords. of lower 
        Y0 = ymin -0.01*s0      ! left corner of square (origin)
        S0 = 1.02*s0

C  Level 1
C    Divide the domain into 4 squares and find the particles in them
         CALL BOX_1(Npart,s0,XC1,YC1,IC1,JC1,NPB1,ds1,
     &               kp1,Liststart)

C  Level 2 - divide level 1 boxes into four squares
        CALL MAKE_BOX(16,ds1,ds2,kp1,kp2,kparent1,kchildless1,
     &                IC1,JC1,NPB1,Iparent1,Imark1,Ipar1ch2,ich2par1,
     &                NPB2,IC2,JC2,XC2,YC2,Ichildless1)

C  Level 3
        IF(kp2.EQ.0)GOTO 1
        CALL MAKE_BOX(64,ds2,ds3,kp2,kp3,kparent2,kchildless2,
     &                IC2,JC2,NPB2,Iparent2,Imark2,Ipar2ch3,ich3par2,
     &                NPB3,IC3,JC3,XC3,YC3,Ichildless2)

C  Level 4
        IF(kp3.EQ.0)GOTO 1
        CALL MAKE_BOX(256,ds3,ds4,kp3,kp4,kparent3,kchildless3,
     &                IC3,JC3,NPB3,Iparent3,Imark3,Ipar3ch4,ich4par3,
     &                NPB4,IC4,JC4,XC4,YC4,Ichildless3)

C  Level 5
        IF(kp4.EQ.0)GOTO 1
        CALL MAKE_BOX(1024,ds4,ds5,kp4,kp5,kparent4,kchildless4,
     &                IC4,JC4,NPB4,Iparent4,Imark4,Ipar4ch5,ich5par4,
     &                NPB5,IC5,JC5,XC5,YC5,Ichildless4)

C  Level 6
        IF(kp5.EQ.0)GOTO 1
        CALL MAKE_BOX(4096,ds5,ds6,kp5,kp6,kparent5,kchildless5,
     &                IC5,JC5,NPB5,Iparent5,Imark5,Ipar5ch6,ich6par5,
     &                NPB6,IC6,JC6,XC6,YC6,Ichildless5)

C  Level 7 
        IF(kp6.EQ.0)GOTO 1
        CALL MAKE_BOX(16384,ds6,ds7,kp6,kp7,kparent6,kchildless6,
     &                IC6,JC6,NPB6,Iparent6,Imark6,Ipar6ch7,ich7par6,
     &                NPB7,IC7,JC7,XC7,YC7,Ichildless6)

C  Level 8 
        IF(kp7.EQ.0)GOTO 1
        CALL MAKE_BOX(65536,ds7,ds8,kp7,kp8,kparent7,kchildless7,
     &                IC7,JC7,NPB7,Iparent7,Imark7,Ipar7ch8,ich8par7,
     &                NPB8,IC8,JC8,XC8,YC8,Ichildless7)

C  Level 9 (finest boxes)
        IF(kp8.EQ.0)GOTO 1
        CALL MAKE_BOX(262144,ds8,ds9,kp8,kp9,kparent8,kchildless8,
     &                IC8,JC8,NPB8,Iparent8,Imark8,Ipar8ch9,ich9par8,
     &                NPB9,IC9,JC9,XC9,YC9,Ichildless8)


1       ds2 = 0.5*ds1
        ds3 = 0.5*ds2
        ds4 = 0.5*ds3
        ds5 = 0.5*ds4
        ds6 = 0.5*ds5
        ds7 = 0.5*ds6
        ds8 = 0.5*ds7
        ds9 = 0.5*ds8

C_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.
C                                                                             C
C         STAGE 2 : FORM THE MULTIPOLE EXPANSIONS  FOR EVERY  BOX             C
C                   AT EACH  LEVEL                                            C
C_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._C

C  Form the expansions of the childless boxes first as these are finest levels
 
C  Level 1 (Coarsest)
       IF (kchildless1.ne.0) then
          CALL exp_chdless(4,kchildless1,XC1,YC1,Ichildless1,
     &                     Npb1,Pr1,Pi1)
        endif

C  Level 2
        If(kchildless2.ne.0) then
          CALL exp_chdless(16,kchildless2,XC2,YC2,Ichildless2,
     &                     Npb2,Pr2,Pi2)
        endif

C  Level 3
        If(kchildless3.ne.0) then
          CALL exp_chdless(64,kchildless3,XC3,YC3,Ichildless3,
     &                     Npb3,Pr3,Pi3)
        endif

C  Level 4
        If(kchildless4.ne.0) then
          CALL exp_chdless(256,kchildless4,XC4,YC4,Ichildless4,
     &                     Npb4,Pr4,Pi4)
        endif

C  Level 5
        If(kchildless5.ne.0) then
          CALL exp_chdless(1024,kchildless5,XC5,YC5,Ichildless5,
     &                     Npb5,Pr5,Pi5)
        endif

C  Level 6
        If(kchildless6.ne.0) then
          CALL exp_chdless(4096,kchildless6,XC6,YC6,Ichildless6,
     &                     Npb6,Pr6,Pi6)
        endif

C  Level 7
        If(kchildless7.ne.0) then
          CALL exp_chdless(16384,kchildless7,XC7,YC7,Ichildless7,
     &                     Npb7,Pr7,Pi7)
        endif

C  Level 8
        If(kchildless8.ne.0) then
          CALL exp_chdless(65536,kchildless8,XC8,YC8,Ichildless8,
     &                     Npb8,Pr8,Pi8)
        endif

C  Level 9  (finest)
        If(kp9.ne.0) CALL childless9(kp9)



C  Form the expansions of the parent boxes now

C  Level 8
        IF(kparent8.ne.0) then
        CALL CH_TO_PAR(262144,ds9,kparent8,Iparent8,Ipar8Ch9,Pr9,Pi9,
     &             Pr8,Pi8,Gr8,Gi8)
        endif

C  Level 7
        IF(kparent7.ne.0) then
        CALL CH_TO_PAR(65536,ds8,kparent7,Iparent7,Ipar7Ch8,Pr8,Pi8,
     &             Pr7,Pi7,Gr7,Gi7)
        endif

C  Level 6
        IF(kparent6.ne.0) then
        CALL CH_TO_PAR(16384,ds7,kparent6,Iparent6,Ipar6Ch7,Pr7,Pi7,
     &             Pr6,Pi6,Gr6,Gi6)
        endif

C  Level 5
        IF(kparent5.ne.0) then
        CALL CH_TO_PAR(4096,ds6,kparent5,Iparent5,Ipar5Ch6,Pr6,Pi6,
     &             Pr5,Pi5,Gr5,Gi5)
        endif

C  Level 4
        IF(kparent4.ne.0) then
        CALL CH_TO_PAR(1024,ds5,kparent4,Iparent4,Ipar4Ch5,Pr5,Pi5,
     &             Pr4,Pi4,Gr4,Gi4)
        endif

C  Level 3
        IF(kparent3.ne.0) then
        CALL CH_TO_PAR(256,ds4,kparent3,Iparent3,Ipar3Ch4,Pr4,Pi4,
     &             Pr3,Pi3,Gr3,Gi3)
        endif

C  Level 2
        IF(kparent2.ne.0) then
        CALL CH_TO_PAR(64,ds3,kparent2,Iparent2,Ipar2Ch3,Pr3,Pi3,
     &             Pr2,Pi2,Gr2,Gi2)
        endif

C  Level 1
        IF(kparent1.ne.0) then
        CALL CH_TO_PAR(16,ds2,kparent1,Iparent1,Ipar1Ch2,Pr2,Pi2,
     &             Pr1,Pi1,Gr1,Gi1)
        endif

c-- stop here if only building the interaction tree
        IF(islip.EQ.0) RETURN 

        do i=1,nvort
           UU(i) = 0.
           VV(i) = 0.
           Gdiff(i) = 0.
        enddo

C_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.
C                                                                             C
C         STAGE 3 : Now allow the particles and boxes to interact,            C
C                   finding the induced velocities and circulation exchange   C
C_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._C

107     format(f8.4,2x,f8.4,2x,f8.4)

        if(istats.eq.1) then
          write(*,*)'************TREE STATS*************'
          write(*,*)'Level 1: total boxes=',kp1,
     &              ', childless=',kchildless1 
          write(*,*)'Level 2: total boxes=',kp2,
     &              ', childless=',kchildless2 
          write(*,*)'Level 3: total boxes=',kp3,
     &              ', childless=',kchildless3 
          write(*,*)'Level 4: total boxes=',kp4,
     &              ', childless=',kchildless4 
          write(*,*)'Level 5: total boxes=',kp5,
     &              ', childless=',kchildless5 
          write(*,*)'Level 6: total boxes=',kp6,
     &              ', childless=',kchildless6 
          write(*,*)'Level 7: total boxes=',kp7,
     &              ', childless=',kchildless7
          write(*,*)'Level 8: total boxes=',kp8,
     &              ', childless=',kchildless8
          write(*,*)'Level 9: total boxes=',kp9,
     &              ', childless=',kp9
        endif

c Level 1
        if(kchildless1.ne.0) CALL INT_CHLESS1(kchildless1)

C 2nd level
        if(kchildless2.ne.0) CALL INT_CHLESS2(kchildless2)
        if(kp2.ne.0) CALL INT_REST2(kp2)

C 3rd level
        if(kchildless3.ne.0) CALL INT_CHLESS3(kp2,kchildless3)
        if(kp3.ne.0) CALL INT_REST3(kp3)

C 4th level
        if(kchildless4.ne.0) CALL INT_CHLESS4(kp3,kchildless4)
        if(kp4.ne.0) CALL INT_REST4(kp4)

C 5th level
        if(kchildless5.ne.0) CALL INT_CHLESS5(kp4,kchildless5)
        if(kp5.ne.0) CALL INT_REST5(kp5)

C 6th level
        if(kchildless6.ne.0) CALL INT_CHLESS6(kp5,kchildless6)
        if(kp6.ne.0) CALL INT_REST6(kp6)

C 7th level 
        if(kchildless7.ne.0) CALL INT_CHLESS7(kp6,kchildless7)
        if(kp7.ne.0) CALL INT_REST7(kp7) 

C 8th level 
        if(kchildless8.ne.0) CALL INT_CHLESS8(kp7,kchildless8)
        if(kp8.ne.0) CALL INT_REST8(kp8) 

C 9th level (finest)
        if(kp9.ne.0) CALL INT_CHLESS9(kp8,kp9)
        if(kp9.ne.0) CALL INT_REST9(kp9) 

C_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.
C                                                                             C
C         STAGE 4 : TRANSFER THE EXPANSIONS OF BOXES FROM                     C
C                   COARSER TO FINER LEVELS                                   C
C                                                                             C
C_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._C


C*** Level 2 -> Level 3 

        if(kp3.ne.0) then
        CALL par_to_ch(16,ds3,kp3,br2,bi2,br3,bi3,ich3par2,ipar2ch3)
        endif

C*** Level 3 -> Level 4

        if(kp4.ne.0) then
        CALL par_to_ch(64,ds4,kp4,br3,bi3,br4,bi4,ich4par3,ipar3ch4)
        endif

C*** Level 4 -> Level 5 

        if(kp5.ne.0) then
        CALL par_to_ch(256,ds5,kp5,br4,bi4,br5,bi5,ich5par4,ipar4ch5)
        endif

C*** Level 5 -> Level 6

        if(kp6.ne.0) then
        CALL par_to_ch(1024,ds6,kp6,br5,bi5,br6,bi6,ich6par5,ipar5ch6)
        endif

C*** Level 6 -> Level 7

        if(kp7.ne.0) then
        CALL par_to_ch(4096,ds7,kp7,br6,bi6,br7,bi7,ich7par6,ipar6ch7)
        endif

C*** Level 7 -> Level 8

        if(kp8.ne.0) then
        CALL par_to_ch(16384,ds8,kp8,br7,bi7,br8,bi8,ich8par7,ipar7ch8)
        endif

C*** Level 8 -> Level 9

        if(kp9.ne.0) then
        CALL par_to_ch(65536,ds9,kp9,br8,bi8,br9,bi9,ich9par8,ipar8ch9)
        endif

C_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.
C                                                                             C
C         STAGE 5 : FOR EACH PARTCLE IN A CHILDLESS BOX                       C
C                   COMPUTE THE INDUCED VELOCITY FROM THE BOX EXPANSIONS      C
C                                                                             C
C_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._C
C*** Level 2
        if(kchildless2.ne.0) then
          CALL box_to_part(16,kchildless2,Ichildless2,xc2,yc2,
     &                  npb2,br2,bi2)
        endif
                                                                               
C*** Level 3
        if(kchildless3.ne.0) then
          CALL box_to_part(64,kchildless3,Ichildless3,xc3,yc3,
     &                  npb3,br3,bi3)
        endif

C*** Level 4
        if(kchildless4.ne.0) then
          CALL box_to_part(256,kchildless4,Ichildless4,xc4,yc4,
     &                  npb4,br4,bi4)
        endif

C*** Level 5
        if(kchildless5.ne.0) then
          CALL box_to_part(1024,kchildless5,Ichildless5,xc5,yc5,
     &                  npb5,br5,bi5)
        endif

C*** Level 6
        if(kchildless6.ne.0) then
          CALL box_to_part(4096,kchildless6,Ichildless6,xc6,yc6,
     &                  npb6,br6,bi6)
        endif

C*** Level 7
        if(kchildless7.ne.0) then
          CALL box_to_part(16384,kchildless7,Ichildless7,xc7,yc7,
     &                  npb7,br7,bi7)
        endif

C*** Level 8
        if(kchildless8.ne.0) then
          CALL box_to_part(65536,kchildless8,Ichildless8,xc8,yc8,
     &                  npb8,br8,bi8)
        endif

C*** Level 9
        if(kp9.ne.0) then
          CALL box9_to_part(262144,kp9,xc9,yc9,npb9,br9,bi9)
        endif

        RETURN
        END
