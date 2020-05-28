subroutine condiff(npart, islip, visc_rmax, istats)

!  This subroutine is the driver of an implementation of an O(N) method for
!  fast computation of velocities and diffusion of a field of vortex blobs
!  using a box-box scheme.

!*****************************************************************************
!  Coming in, data should be in XP,YP,GP,UU,VV.
!  At the end the particles have locations at XN,YN, GN                      *
!    and the initial velocity field is stored in: Uold,Vold.                 *
!  The new velocity field is placed in: UU,VV.                               *
!  The changed to the circulations due to diffusion are put in Gdiff.        *
!  The minimum number of particles for a box is given by limpar.             *
!                                                                            *
!*****************************************************************************

   implicit none

   include 'main_dim.h'
   include 'part.h'
   include 'tree9.h'

   integer :: limpar
   real :: x0, y0
   common/geom/x0, y0, limpar

   real :: visc_cutoff
   common/cutoff/visc_cutoff

   integer :: npart, islip, istats
   real :: visc_rmax

   integer :: kp2, kp3, kp4, kp5, kp6, kp7, kp8, kp9
   integer :: kchildless1, kchildless2, kchildless3
   integer :: kchildless4, kchildless5, kchildless6
   integer :: kchildless7, kchildless8
   integer :: kparent1, kparent2, kparent3, kparent4
   integer :: kparent5, kparent6, kparent7, kparent8
   integer :: i, j
   integer :: Ich2Par1(nmax2), Ich3Par2(nmax3), Ich4Par3(nmax4)
   integer :: Ich5Par4(nmax5), Ich6Par5(nmax6), Ich7par6(nmax7)
   integer :: Ich8par7(nmax8), Ich9par8(nmax9)
   real :: s0, xmin, xmax, ymin, ymax
   real :: gr1(nmax1, 0:7), gi1(nmax1, 0:7)
   real :: gr2(nmax2, 0:7), gi2(nmax2, 0:7)
   real :: gr3(nmax3, 0:7), gi3(nmax3, 0:7)
   real :: gr4(nmax4, 0:7), gi4(nmax4, 0:7)
   real :: gr5(nmax5, 0:7), gi5(nmax5, 0:7)
   real :: gr6(nmax6, 0:7), gi6(nmax6, 0:7)
   real :: gr7(nmax7, 0:7), gi7(nmax7, 0:7)
   real :: gr8(nmax8, 0:7), gi8(nmax8, 0:7)
   real :: gr9(nmax9, 0:7), gi9(nmax9, 0:7)
!-----------------------------------------------------------------------------

!_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.
!                                                                    _.
!         STAGE 0 : Zero all relevant arrays.                        _.
!_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.

   visc_cutoff = visc_rmax*visc_rmax
   call zeroall   ! handles common blocks

   kp1 = 0.
   kp2 = 0.
   kp3 = 0.
   kp4 = 0.
   kp5 = 0.
   kp6 = 0.
   kp7 = 0.
   kp8 = 0.
   kp9 = 0.

   kchildless1 = 0.
   kchildless2 = 0.
   kchildless3 = 0.
   kchildless4 = 0.
   kchildless5 = 0.
   kchildless6 = 0.
   kchildless7 = 0.
   kchildless8 = 0.

   kparent1 = 0.
   kparent2 = 0.
   kparent3 = 0.
   kparent4 = 0.
   kparent5 = 0.
   kparent6 = 0.
   kparent7 = 0.
   kparent8 = 0.

   do i = 1, nmax2
      Ich2par1(i) = 0.
   enddo
   do i = 1, nmax3
      Ich3par2(i) = 0.
   enddo
   do i = 1, nmax4
      Ich4par3(i) = 0.
   enddo
   do i = 1, nmax5
      Ich5par4(i) = 0.
   enddo
   do i = 1, nmax6
      Ich6par5(i) = 0.
   enddo
   do i = 1, nmax7
      Ich7par6(i) = 0.
   enddo
   do i = 1, nmax8
      Ich8par7(i) = 0.
   enddo
   do i = 1, nmax9
      Ich9par8(i) = 0.
   enddo

   do i = 0, 7
      do j = 1, nmax1
         Gr1(j, i) = 0.0
         Gi1(j, i) = 0.0
      enddo
      do j = 1, nmax2
         Gr2(j, i) = 0.0
         Gi2(j, i) = 0.0
      enddo
      do j = 1, nmax3
         Gr3(j, i) = 0.0
         Gi3(j, i) = 0.0
      enddo
      do j = 1, nmax4
         Gr4(j, i) = 0.0
         Gi4(j, i) = 0.0
      enddo
      do j = 1, nmax5
         Gr5(j, i) = 0.0
         Gi5(j, i) = 0.0
      enddo
      do j = 1, nmax6
         Gr6(j, i) = 0.0
         Gi6(j, i) = 0.0
      enddo
      do j = 1, nmax7
         Gr7(j, i) = 0.0
         Gi7(j, i) = 0.0
      enddo
      do j = 1, nmax8
         Gr8(j, i) = 0.0
         Gi8(j, i) = 0.0
      enddo
      do j = 1, nmax9
         Gr9(j, i) = 0.0
         Gi9(j, i) = 0.0
      enddo
   enddo

!_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.
!                                                                    _.
!         STAGE 1 : Divide the domain containing particles           _.
!                   into a hierarchy of cells.                       _.
!_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.

! Determine the  region  of  the main square

   call box_dim(npart, Xmin, xmax, ymin, ymax)
   s0 = max(abs(xmax - Xmin), abs(ymax - ymin))  ! Side of square
   x0 = xmin - 0.01*s0                   ! Coords. of lower
   y0 = ymin - 0.01*s0      ! left corner of square (origin)
   s0 = 1.02*s0

!  Level 1
!    Divide the domain into 4 squares and find the particles in them
   call box_1(npart, s0, xc1, yc1, ic1, jc1, npb1, ds1, &
              kp1, liststart)

!  Level 2 - divide level 1 boxes into four squares
   call make_box(16, ds1, ds2, kp1, kp2, kparent1, kchildless1, &
                 ic1, jc1, npb1, Iparent1, Imark1, Ipar1ch2, ich2par1, &
                 npb2, ic2, jc2, xc2, yc2, Ichildless1)

!  Level 3
   if (kp2 == 0) goto 1
   call make_box(64, ds2, ds3, kp2, kp3, kparent2, kchildless2, &
                 ic2, jc2, npb2, Iparent2, Imark2, Ipar2ch3, ich3par2, &
                 npb3, ic3, jc3, xc3, yc3, Ichildless2)

!  Level 4
   if (kp3 == 0) goto 1
   call make_box(256, ds3, ds4, kp3, kp4, kparent3, kchildless3, &
                 ic3, jc3, npb3, Iparent3, Imark3, Ipar3ch4, ich4par3, &
                 npb4, ic4, jc4, xc4, yc4, Ichildless3)

!  Level 5
   if (kp4 == 0) goto 1
   call make_box(1024, ds4, ds5, kp4, kp5, kparent4, kchildless4, &
                 ic4, jc4, npb4, Iparent4, Imark4, Ipar4ch5, ich5par4, &
                 npb5, ic5, jc5, xc5, yc5, Ichildless4)

!  Level 6
   if (kp5 == 0) goto 1
   call make_box(4096, ds5, ds6, kp5, kp6, kparent5, kchildless5, &
                 ic5, jc5, npb5, Iparent5, Imark5, Ipar5ch6, ich6par5, &
                 npb6, ic6, jc6, xc6, yc6, Ichildless5)

!  Level 7
   if (kp6 == 0) goto 1
   call make_box(16384, ds6, ds7, kp6, kp7, kparent6, kchildless6, &
                 ic6, jc6, npb6, Iparent6, Imark6, Ipar6ch7, ich7par6, &
                 npb7, ic7, jc7, xc7, yc7, Ichildless6)

!  Level 8
   if (kp7 == 0) goto 1
   call make_box(65536, ds7, ds8, kp7, kp8, kparent7, kchildless7, &
                 ic7, jc7, npb7, Iparent7, Imark7, Ipar7ch8, ich8par7, &
                 npb8, ic8, jc8, xc8, yc8, Ichildless7)

!  Level 9 (finest boxes)
   if (kp8 == 0) goto 1
   call make_box(262144, ds8, ds9, kp8, kp9, kparent8, kchildless8, &
                 ic8, jc8, npb8, Iparent8, Imark8, Ipar8ch9, ich9par8, &
                 npb9, ic9, jc9, xc9, yc9, Ichildless8)

1  ds2 = 0.5*ds1
   ds3 = 0.5*ds2
   ds4 = 0.5*ds3
   ds5 = 0.5*ds4
   ds6 = 0.5*ds5
   ds7 = 0.5*ds6
   ds8 = 0.5*ds7
   ds9 = 0.5*ds8

!_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.
!                                                                             C
!         STAGE 2 : FORM THE MULTIPOLE EXPANSIONS  FOR EVERY  BOX             C
!                   AT EACH  LEVEL                                            C
!_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._C

!  Form the expansions of the childless boxes first as these are finest levels

!  Level 1 (Coarsest)
   if (kchildless1 /= 0) then
      call exp_chdless(4, kchildless1, xc1, yc1, Ichildless1, &
                       Npb1, Pr1, Pi1)
   endif

!  Level 2
   If (kchildless2 /= 0) then
      call exp_chdless(16, kchildless2, xc2, yc2, Ichildless2, &
                       Npb2, Pr2, Pi2)
   endif

!  Level 3
   If (kchildless3 /= 0) then
      call exp_chdless(64, kchildless3, xc3, yc3, Ichildless3, &
                       Npb3, Pr3, Pi3)
   endif

!  Level 4
   If (kchildless4 /= 0) then
      call exp_chdless(256, kchildless4, xc4, yc4, Ichildless4, &
                       Npb4, Pr4, Pi4)
   endif

!  Level 5
   If (kchildless5 /= 0) then
      call exp_chdless(1024, kchildless5, xc5, yc5, Ichildless5, &
                       Npb5, Pr5, Pi5)
   endif

!  Level 6
   If (kchildless6 /= 0) then
      call exp_chdless(4096, kchildless6, xc6, yc6, Ichildless6, &
                       Npb6, Pr6, Pi6)
   endif

!  Level 7
   If (kchildless7 /= 0) then
      call exp_chdless(16384, kchildless7, xc7, yc7, Ichildless7, &
                       Npb7, Pr7, Pi7)
   endif

!  Level 8
   If (kchildless8 /= 0) then
      call exp_chdless(65536, kchildless8, xc8, yc8, Ichildless8, &
                       Npb8, Pr8, Pi8)
   endif

!  Level 9  (finest)
   If (kp9 /= 0) call childless9(kp9)

!  Form the expansions of the parent boxes now

!  Level 8
   if (kparent8 /= 0) then
      call ch_to_par(262144, ds9, kparent8, Iparent8, Ipar8Ch9, Pr9, Pi9, &
                     Pr8, Pi8, Gr8, Gi8)
   endif

!  Level 7
   if (kparent7 /= 0) then
      call ch_to_par(65536, ds8, kparent7, Iparent7, Ipar7Ch8, Pr8, Pi8, &
                     Pr7, Pi7, Gr7, Gi7)
   endif

!  Level 6
   if (kparent6 /= 0) then
      call ch_to_par(16384, ds7, kparent6, Iparent6, Ipar6Ch7, Pr7, Pi7, &
                     Pr6, Pi6, Gr6, Gi6)
   endif

!  Level 5
   if (kparent5 /= 0) then
      call ch_to_par(4096, ds6, kparent5, Iparent5, Ipar5Ch6, Pr6, Pi6, &
                     Pr5, Pi5, Gr5, Gi5)
   endif

!  Level 4
   if (kparent4 /= 0) then
      call ch_to_par(1024, ds5, kparent4, Iparent4, Ipar4Ch5, Pr5, Pi5, &
                     Pr4, Pi4, Gr4, Gi4)
   endif

!  Level 3
   if (kparent3 /= 0) then
      call ch_to_par(256, ds4, kparent3, Iparent3, Ipar3Ch4, Pr4, Pi4, &
                     Pr3, Pi3, Gr3, Gi3)
   endif

!  Level 2
   if (kparent2 /= 0) then
      call ch_to_par(64, ds3, kparent2, Iparent2, Ipar2Ch3, Pr3, Pi3, &
                     Pr2, Pi2, Gr2, Gi2)
   endif

!  Level 1
   if (kparent1 /= 0) then
      call ch_to_par(16, ds2, kparent1, Iparent1, Ipar1Ch2, Pr2, Pi2, &
                     Pr1, Pi1, Gr1, Gi1)
   endif

!-- stop here if only building the interaction tree
   if (islip == 0) return

   do i = 1, nvort
      uu(i) = 0.
      vv(i) = 0.
      Gdiff(i) = 0.
   enddo

!_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.
!                                                                             C
!         STAGE 3 : Now allow the particles and boxes to interact,            C
!                   finding the induced velocities and circulation exchange   C
!_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._C

107 format(f8.4, 2x, f8.4, 2x, f8.4)

   if (istats == 1) then
      write (*, *) '************TREE STATS*************'
      write (*, *) 'Level 1: total boxes=', kp1, &
         ', childless=', kchildless1
      write (*, *) 'Level 2: total boxes=', kp2, &
         ', childless=', kchildless2
      write (*, *) 'Level 3: total boxes=', kp3, &
         ', childless=', kchildless3
      write (*, *) 'Level 4: total boxes=', kp4, &
         ', childless=', kchildless4
      write (*, *) 'Level 5: total boxes=', kp5, &
         ', childless=', kchildless5
      write (*, *) 'Level 6: total boxes=', kp6, &
         ', childless=', kchildless6
      write (*, *) 'Level 7: total boxes=', kp7, &
         ', childless=', kchildless7
      write (*, *) 'Level 8: total boxes=', kp8, &
         ', childless=', kchildless8
      write (*, *) 'Level 9: total boxes=', kp9, &
         ', childless=', kp9
   endif

! Level 1
   if (kchildless1 /= 0) call int_chless1(kchildless1)

! 2nd level
   if (kchildless2 /= 0) call int_chless2(kchildless2)
   if (kp2 /= 0) call int_rest2(kp2)

! 3rd level
   if (kchildless3 /= 0) call int_chless3(kp2, kchildless3)
   if (kp3 /= 0) call int_rest3(kp3)

! 4th level
   if (kchildless4 /= 0) call int_chless4(kp3, kchildless4)
   if (kp4 /= 0) call int_rest4(kp4)

! 5th level
   if (kchildless5 /= 0) call int_chless5(kp4, kchildless5)
   if (kp5 /= 0) call int_rest5(kp5)

! 6th level
   if (kchildless6 /= 0) call int_chless6(kp5, kchildless6)
   if (kp6 /= 0) call int_rest6(kp6)

! 7th level
   if (kchildless7 /= 0) call int_chless7(kp6, kchildless7)
   if (kp7 /= 0) call int_rest7(kp7)

! 8th level
   if (kchildless8 /= 0) call int_chless8(kp7, kchildless8)
   if (kp8 /= 0) call int_rest8(kp8)

! 9th level (finest)
   if (kp9 /= 0) call int_chless9(kp8, kp9)
   if (kp9 /= 0) call int_rest9(kp9)

!_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.
!                                                                             C
!         STAGE 4 : TRANSFER THE EXPANSIONS OF BOXES FROM                     C
!                   COARSER TO FINER LEVELS                                   C
!                                                                             C
!_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._C

!*** Level 2 -> Level 3

   if (kp3 /= 0) then
      call par_to_ch(16, ds3, kp3, br2, bi2, br3, bi3, ich3par2, ipar2ch3)
   endif

!*** Level 3 -> Level 4

   if (kp4 /= 0) then
      call par_to_ch(64, ds4, kp4, br3, bi3, br4, bi4, ich4par3, ipar3ch4)
   endif

!*** Level 4 -> Level 5

   if (kp5 /= 0) then
      call par_to_ch(256, ds5, kp5, br4, bi4, br5, bi5, ich5par4, ipar4ch5)
   endif

!*** Level 5 -> Level 6

   if (kp6 /= 0) then
      call par_to_ch(1024, ds6, kp6, br5, bi5, br6, bi6, ich6par5, ipar5ch6)
   endif

!*** Level 6 -> Level 7

   if (kp7 /= 0) then
      call par_to_ch(4096, ds7, kp7, br6, bi6, br7, bi7, ich7par6, ipar6ch7)
   endif

!*** Level 7 -> Level 8

   if (kp8 /= 0) then
      call par_to_ch(16384, ds8, kp8, br7, bi7, br8, bi8, ich8par7, ipar7ch8)
   endif

!*** Level 8 -> Level 9

   if (kp9 /= 0) then
      call par_to_ch(65536, ds9, kp9, br8, bi8, br9, bi9, ich9par8, ipar8ch9)
   endif

!_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.
!                                                                             C
!         STAGE 5 : FOR EACH PARTCLE IN A CHILDLESS BOX                       C
!                   COMPUTE THE INDUCED VELOCITY FROM THE BOX EXPANSIONS      C
!                                                                             C
!_._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._C
!*** Level 2
   if (kchildless2 /= 0) then
      call box_to_part(16, kchildless2, Ichildless2, xc2, yc2, &
                       npb2, br2, bi2)
   endif

!*** Level 3
   if (kchildless3 /= 0) then
      call box_to_part(64, kchildless3, Ichildless3, xc3, yc3, &
                       npb3, br3, bi3)
   endif

!*** Level 4
   if (kchildless4 /= 0) then
      call box_to_part(256, kchildless4, Ichildless4, xc4, yc4, &
                       npb4, br4, bi4)
   endif

!*** Level 5
   if (kchildless5 /= 0) then
      call box_to_part(1024, kchildless5, Ichildless5, xc5, yc5, &
                       npb5, br5, bi5)
   endif

!*** Level 6
   if (kchildless6 /= 0) then
      call box_to_part(4096, kchildless6, Ichildless6, xc6, yc6, &
                       npb6, br6, bi6)
   endif

!*** Level 7
   if (kchildless7 /= 0) then
      call box_to_part(16384, kchildless7, Ichildless7, xc7, yc7, &
                       npb7, br7, bi7)
   endif

!*** Level 8
   if (kchildless8 /= 0) then
      call box_to_part(65536, kchildless8, Ichildless8, xc8, yc8, &
                       npb8, br8, bi8)
   endif

!*** Level 9
   if (kp9 /= 0) then
      call box9_to_part(262144, kp9, xc9, yc9, npb9, br9, bi9)
   endif

   return
end subroutine
