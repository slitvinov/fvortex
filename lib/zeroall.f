c-----------------------------------------------------------------------   
c This subroutine zeroes the various arrays used in identifying the tree

        SUBROUTINE ZEROALL

c This subroutine zeroes the various arrays used in identifying the tree

        include 'tree_tmp.h'
        include 'main_dim.h'
        include 'tree9.h'

        integer i,j
c-----------------------------------------------------------------------   

        do i=1,nbox_max    
           Xbox(i) = 0.0
           Ybox(i) = 0.0
        enddo
        do i=0,7
           do j=1,nbox_max
              Prbox(j,i) = 0.0
              Pibox(j,i) = 0.0
           enddo
        enddo
        do i=1,np_max
           XT(i) = 0.0
           YT(i) = 0.0
           GT(i) = 0.0
           IT(i) = 0
        enddo

        do i=1,nmax1
           XC1(i) = 0.0
           YC1(i) = 0.0
           NPB1(i,1) = 0
           NPB1(i,2) = 0
           IC1(i) = 0
           Ichildless1(i) = 0
           IPARENT1(i) = 0
           Imark1(i) = 0
           LISTSTART(i) = 0
        enddo
        do i=1,nmax2
           XC2(i) = 0.0
           YC2(i) = 0.0
           NPB2(i,1) = 0
           NPB2(i,2) = 0
           IC2(i) = 0
           JC2(i) = 0
           Ichildless2(i) = 0
           IPARENT2(i) = 0
           Imark2(i) = 0
        enddo
        do i=1,nmax3
           XC3(i) = 0.0
           YC3(i) = 0.0
           NPB3(i,1) = 0
           NPB3(i,2) = 0
           IC3(i) = 0
           JC3(i) = 0
           Ichildless3(i) = 0
           IPARENT3(i) = 0
           Imark3(i) = 0
        enddo
        do i=1,nmax4
           XC4(i) = 0.0
           YC4(i) = 0.0
           NPB4(i,1) = 0
           NPB4(i,2) = 0
           IC4(i) = 0
           JC4(i) = 0
           Ichildless4(i) = 0
           IPARENT4(i) = 0
           Imark4(i) = 0
        enddo
        do i=1,nmax5
           XC5(i) = 0.0
           YC5(i) = 0.0
           NPB5(i,1) = 0
           NPB5(i,2) = 0
           IC5(i) = 0
           JC5(i) = 0
           Ichildless5(i) = 0
           IPARENT5(i) = 0
           Imark5(i) = 0
        enddo
        do i=1,nmax6
           XC6(i) = 0.0
           YC6(i) = 0.0
           NPB6(i,1) = 0
           NPB6(i,2) = 0
           IC6(i) = 0
           JC6(i) = 0
           Ichildless6(i) = 0
           IPARENT6(i) = 0
           Imark6(i) = 0
        enddo
        do i=1,nmax7
           XC7(i) = 0.0
           YC7(i) = 0.0
           NPB7(i,1) = 0
           NPB7(i,2) = 0
           IC7(i) = 0
           JC7(i) = 0
           Ichildless7(i) = 0
           IPARENT7(i) = 0
           Imark7(i) = 0
        enddo
        do i=1,nmax8
           XC8(i) = 0.0
           YC8(i) = 0.0
           NPB8(i,1) = 0
           NPB8(i,2) = 0
           IC8(i) = 0
           JC8(i) = 0
           Ichildless8(i) = 0
           IPARENT8(i) = 0
           Imark8(i) = 0
        enddo
        do i=1,nmax9
           XC9(i) = 0.0
           YC9(i) = 0.0
           NPB9(i,1) = 0
           NPB9(i,2) = 0
           IC9(i) = 0
           JC9(i) = 0
        enddo

        do i=1,4
           do j=1,nmax1
              IPAR1CH2(j,i) = 0
           enddo
           do j=1,nmax2
              IPAR2CH3(j,i) = 0
           enddo
           do j=1,nmax3
              IPAR3CH4(j,i) = 0
           enddo
           do j=1,nmax4
              IPAR4CH5(j,i) = 0
           enddo
           do j=1,nmax5
              IPAR5CH6(j,i) = 0
           enddo
           do j=1,nmax6
              IPAR6CH7(j,i) = 0
           enddo
           do j=1,nmax7
              IPAR7CH8(j,i) = 0
           enddo
           do j=1,nmax8
              IPAR8CH9(j,i) = 0
           enddo
        enddo


        do i=0,7
           do j=1,nmax1
              Pr1(j,i) = 0.0
              Pi1(j,i) = 0.0
           enddo
           do j=1,nmax2
              Pr2(j,i) = 0.0
              Pi2(j,i) = 0.0
           enddo
           do j=1,nmax3
              Pr3(j,i) = 0.0
              Pi3(j,i) = 0.0
           enddo
           do j=1,nmax4
              Pr4(j,i) = 0.0
              Pi4(j,i) = 0.0
           enddo
           do j=1,nmax5
              Pr5(j,i) = 0.0
              Pi5(j,i) = 0.0
           enddo
           do j=1,nmax6
              Pr6(j,i) = 0.0
              Pi6(j,i) = 0.0
           enddo
           do j=1,nmax7
              Pr7(j,i) = 0.0
              Pi7(j,i) = 0.0
           enddo
           do j=1,nmax8
              Pr8(j,i) = 0.0
              Pi8(j,i) = 0.0
           enddo
           do j=1,nmax9
              Pr9(j,i) = 0.0
              Pi9(j,i) = 0.0
           enddo
        enddo
        do i=1,7
           do j=1,nmax1
              Br1(j,i) = 0.0
              Bi1(j,i) = 0.0
           enddo
           do j=1,nmax2
              Br2(j,i) = 0.0
              Bi2(j,i) = 0.0
           enddo
           do j=1,nmax3
              Br3(j,i) = 0.0
              Bi3(j,i) = 0.0
           enddo
           do j=1,nmax4
              Br4(j,i) = 0.0
              Bi4(j,i) = 0.0
           enddo
           do j=1,nmax5
              Br5(j,i) = 0.0
              Bi5(j,i) = 0.0
           enddo
           do j=1,nmax6
              Br6(j,i) = 0.0
              Bi6(j,i) = 0.0
           enddo
           do j=1,nmax7
              Br7(j,i) = 0.0
              Bi7(j,i) = 0.0
           enddo
           do j=1,nmax8
              Br8(j,i) = 0.0
              Bi8(j,i) = 0.0
           enddo
           do j=1,nmax9
              Br9(j,i) = 0.0
              Bi9(j,i) = 0.0
           enddo
        enddo

        RETURN
        END
