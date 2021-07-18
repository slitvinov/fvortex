      subroutine zeroall
!-----------------------------------------------------------------------
!     This subroutine zeroes the various arrays used in identifying the tree

!     This subroutine zeroes the various arrays used in identifying the tree

      include 'tree_tmp.h'
      include 'main_dim.h'
      include 'tree9.h'

      integer i, j
!-----------------------------------------------------------------------

      do i = 1, nbox_max
         Xbox(i) = 0.0
         Ybox(i) = 0.0
      enddo
      do i = 0, 7
         do j = 1, nbox_max
            Prbox(j, i) = 0.0
            Pibox(j, i) = 0.0
         enddo
      enddo
      do i = 1, np_max
         xt(i) = 0.0
         yt(i) = 0.0
         gt(i) = 0.0
         it(i) = 0
      enddo

      do i = 1, nmax1
         xc1(i) = 0.0
         yc1(i) = 0.0
         npb1(i, 1) = 0
         npb1(i, 2) = 0
         ic1(i) = 0
         ichildless1(i) = 0
         iparent1(i) = 0
         imark1(i) = 0
         liststart(i) = 0
      enddo
      do i = 1, nmax2
         xc2(i) = 0.0
         yc2(i) = 0.0
         npb2(i, 1) = 0
         npb2(i, 2) = 0
         ic2(i) = 0
         jc2(i) = 0
         ichildless2(i) = 0
         iparent2(i) = 0
         imark2(i) = 0
      enddo
      do i = 1, nmax3
         xc3(i) = 0.0
         yc3(i) = 0.0
         npb3(i, 1) = 0
         npb3(i, 2) = 0
         ic3(i) = 0
         jc3(i) = 0
         ichildless3(i) = 0
         iparent3(i) = 0
         imark3(i) = 0
      enddo
      do i = 1, nmax4
         xc4(i) = 0.0
         yc4(i) = 0.0
         npb4(i, 1) = 0
         npb4(i, 2) = 0
         ic4(i) = 0
         jc4(i) = 0
         ichildless4(i) = 0
         iparent4(i) = 0
         imark4(i) = 0
      enddo
      do i = 1, nmax5
         xc5(i) = 0.0
         yc5(i) = 0.0
         npb5(i, 1) = 0
         npb5(i, 2) = 0
         ic5(i) = 0
         jc5(i) = 0
         ichildless5(i) = 0
         iparent5(i) = 0
         imark5(i) = 0
      enddo
      do i = 1, nmax6
         xc6(i) = 0.0
         yc6(i) = 0.0
         npb6(i, 1) = 0
         npb6(i, 2) = 0
         ic6(i) = 0
         jc6(i) = 0
         ichildless6(i) = 0
         iparent6(i) = 0
         imark6(i) = 0
      enddo
      do i = 1, nmax7
         xc7(i) = 0.0
         yc7(i) = 0.0
         npb7(i, 1) = 0
         npb7(i, 2) = 0
         ic7(i) = 0
         jc7(i) = 0
         ichildless7(i) = 0
         iparent7(i) = 0
         imark7(i) = 0
      enddo
      do i = 1, nmax8
         xc8(i) = 0.0
         yc8(i) = 0.0
         npb8(i, 1) = 0
         npb8(i, 2) = 0
         ic8(i) = 0
         jc8(i) = 0
         ichildless8(i) = 0
         iparent8(i) = 0
         imark8(i) = 0
      enddo
      do i = 1, nmax9
         xc9(i) = 0.0
         yc9(i) = 0.0
         npb9(i, 1) = 0
         npb9(i, 2) = 0
         ic9(i) = 0
         jc9(i) = 0
      enddo

      do i = 1, 4
         do j = 1, nmax1
            ipar1ch2(j, i) = 0
         enddo
         do j = 1, nmax2
            ipar2ch3(j, i) = 0
         enddo
         do j = 1, nmax3
            ipar3ch4(j, i) = 0
         enddo
         do j = 1, nmax4
            ipar4ch5(j, i) = 0
         enddo
         do j = 1, nmax5
            ipar5ch6(j, i) = 0
         enddo
         do j = 1, nmax6
            ipar6ch7(j, i) = 0
         enddo
         do j = 1, nmax7
            ipar7ch8(j, i) = 0
         enddo
         do j = 1, nmax8
            ipar8ch9(j, i) = 0
         enddo
      enddo

      do i = 0, 7
         do j = 1, nmax1
            Pr1(j, i) = 0.0
            Pi1(j, i) = 0.0
         enddo
         do j = 1, nmax2
            Pr2(j, i) = 0.0
            Pi2(j, i) = 0.0
         enddo
         do j = 1, nmax3
            Pr3(j, i) = 0.0
            Pi3(j, i) = 0.0
         enddo
         do j = 1, nmax4
            Pr4(j, i) = 0.0
            Pi4(j, i) = 0.0
         enddo
         do j = 1, nmax5
            Pr5(j, i) = 0.0
            Pi5(j, i) = 0.0
         enddo
         do j = 1, nmax6
            Pr6(j, i) = 0.0
            Pi6(j, i) = 0.0
         enddo
         do j = 1, nmax7
            Pr7(j, i) = 0.0
            Pi7(j, i) = 0.0
         enddo
         do j = 1, nmax8
            Pr8(j, i) = 0.0
            Pi8(j, i) = 0.0
         enddo
         do j = 1, nmax9
            Pr9(j, i) = 0.0
            Pi9(j, i) = 0.0
         enddo
      enddo
      do i = 1, 7
         do j = 1, nmax1
            Br1(j, i) = 0.0
            Bi1(j, i) = 0.0
         enddo
         do j = 1, nmax2
            Br2(j, i) = 0.0
            Bi2(j, i) = 0.0
         enddo
         do j = 1, nmax3
            Br3(j, i) = 0.0
            Bi3(j, i) = 0.0
         enddo
         do j = 1, nmax4
            Br4(j, i) = 0.0
            Bi4(j, i) = 0.0
         enddo
         do j = 1, nmax5
            Br5(j, i) = 0.0
            Bi5(j, i) = 0.0
         enddo
         do j = 1, nmax6
            Br6(j, i) = 0.0
            Bi6(j, i) = 0.0
         enddo
         do j = 1, nmax7
            Br7(j, i) = 0.0
            Bi7(j, i) = 0.0
         enddo
         do j = 1, nmax8
            Br8(j, i) = 0.0
            Bi8(j, i) = 0.0
         enddo
         do j = 1, nmax9
            Br9(j, i) = 0.0
            Bi9(j, i) = 0.0
         enddo
      enddo

      return
      end
