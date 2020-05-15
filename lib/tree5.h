        integer nmax1,nmax2,nmax3,nmax4,nmax5
        PARAMETER (Nmax1 = 4, Nmax2 = 16,Nmax3 = 64, Nmax4 = 256,
     &             Nmax5 = 1024)

        integer kp1
        real ds1,ds2,ds3,ds4,ds5
        COMMON/TIMES/kp1,ds1,ds2,ds3,ds4,ds5

        real XC1(4),YC1(4),XC2(16),YC2(16),XC3(64),YC3(64),
     &       XC4(256),YC4(256),XC5(1024),YC5(1024)
        COMMON/CENTRE/xc1,yc1,xc2,yc2,xc3,yc3,xc4,yc4,xc5,yc5

        integer NPB1(4,2),IC1(4),JC1(4),NPB2(16,2),IC2(16),JC2(16),
     &       NPB3(64,2),IC3(64),JC3(64),NPB4(256,2),IC4(256),JC4(256),
     &       NPB5(1024,2),IC5(1024),JC5(1024)
        COMMON/INDEX11/npb1,ic1,jc1,npb2,ic2,jc2,npb3,ic3,jc3,
     &                 npb4,ic4,jc4,npb5,ic5,jc5

        integer IPAR1CH2(4,4),IPAR2CH3(16,4),IPAR3CH4(64,4),
     &       IPAR4CH5(256,4)
        COMMON/INDEX22/IPAR1CH2,IPAR2CH3,IPAR3CH4,
     &                 IPAR4CH5

        integer Ichildless4(256),Iparent4(256),Imark4(256)
     &       ,Ichildless3(64),Iparent3(64),Imark3(64)
     &       ,Ichildless2(16),Iparent2(16),Imark2(16)
     &       ,Ichildless1(4),Iparent1(4),Imark1(4)
     &       ,Liststart(4)
        COMMON/INDEX33/Ichildless4,Iparent4,Imark4
     &                ,Ichildless3,Iparent3,Imark3
     &                ,Ichildless2,Iparent2,Imark2
     &                ,Ichildless1,Iparent1,Imark1
     &                ,Liststart

         real PR5(1024,0:7),PI5(1024,0:7),
     &        PR4(256,0:7) ,PI4(256,0:7),PR3(64,0:7)  ,PI3(64,0:7),
     &        PR2(16,0:7)  ,PI2(16,0:7),PR1(4,0:7)   ,PI1(4,0:7)
         COMMON/POLES/pr5,pi5,pr4,pi4,pr3,pi3,pr2,pi2,pr1,pi1

         real BR5(1024,7),BI5(1024,7),BR4(256,7) ,BI4(256,7),
     &        BR3(64,7),BI3(64,7),BR2(16,7),BI2(16,7),BR1(4,7),BI1(4,7)
         COMMON/BOXEXP/br5,bi5,br4,bi4,br3,bi3,br2,bi2,br1,bi1




