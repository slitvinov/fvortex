        integer nmax1,nmax2,nmax3,nmax4,nmax5,nmax6,nmax7,nmax8
        PARAMETER (Nmax1 = 4, Nmax2 = 16,Nmax3 = 64, Nmax4 = 256,
     &             Nmax5 = 1024, Nmax6 = 4096, Nmax7 = 16384,
     &             Nmax8 = 65536)

        integer kp1
        real ds1,ds2,ds3,ds4,ds5,ds6,ds7,ds8
        COMMON/TIMES/kp1,ds1,ds2,ds3,ds4,ds5,ds6,ds7,ds8

        real XC1(4),YC1(4),XC2(16),YC2(16),XC3(64),YC3(64),
     &       XC4(256),YC4(256),XC5(1024),YC5(1024),
     &       XC6(4096),YC6(4096),XC7(16384),YC7(16384), 
     &       XC8(65536),YC8(65536)   
        COMMON/CENTRE/xc1,yc1,xc2,yc2,xc3,yc3,xc4,yc4,xc5,yc5,
     &                xc6,yc6,xc7,yc7,xc8,yc8

        integer NPB1(4,2),IC1(4),JC1(4),NPB2(16,2),IC2(16),JC2(16),
     &       NPB3(64,2),IC3(64),JC3(64),NPB4(256,2),IC4(256),JC4(256),
     &       NPB5(1024,2),IC5(1024),JC5(1024),
     &       NPB6(4096,2),IC6(4096),JC6(4096), 
     &       NPB7(16384,2),IC7(16384),JC7(16384),    
     &       NPB8(65536,2),IC8(65536),JC8(65536)    
        COMMON/INDEX11/npb1,ic1,jc1,npb2,ic2,jc2,npb3,ic3,jc3,
     &                 npb4,ic4,jc4,npb5,ic5,jc5,npb6,ic6,jc6,
     &                 npb7,ic7,jc7,npb8,ic8,jc8

        integer IPAR1CH2(4,4),IPAR2CH3(16,4),IPAR3CH4(64,4),
     &       IPAR4CH5(256,4),IPAR5CH6(1024,4),IPAR6CH7(4096,4),
     &       IPAR7CH8(16384,4)
        COMMON/INDEX22/IPAR1CH2,IPAR2CH3,IPAR3CH4,
     &                 IPAR4CH5,IPAR5CH6,IPAR6CH7,IPAR7CH8

        integer Ichildless7(16384),Iparent7(16384),Imark7(16384)
     &       ,Ichildless6(4096),Iparent6(4096),Imark6(4096)
     &       ,Ichildless5(1024),Iparent5(1024),Imark5(1024)
     &       ,Ichildless4(256),Iparent4(256),Imark4(256)
     &       ,Ichildless3(64),Iparent3(64),Imark3(64)
     &       ,Ichildless2(16),Iparent2(16),Imark2(16)
     &       ,Ichildless1(4),Iparent1(4),Imark1(4)
     &       ,Liststart(4)
        COMMON/INDEX33/Ichildless7,Iparent7,Imark7
     &                ,Ichildless6,Iparent6,Imark6
     &                ,Ichildless5,Iparent5,Imark5
     &                ,Ichildless4,Iparent4,Imark4
     &                ,Ichildless3,Iparent3,Imark3
     &                ,Ichildless2,Iparent2,Imark2
     &                ,Ichildless1,Iparent1,Imark1
     &                ,Liststart

         real PR8(65536,0:7),PI8(65536,0:7),
     &        PR7(16384,0:7),PI7(16384,0:7),
     &        PR6(4096,0:7),PI6(4096,0:7),PR5(1024,0:7),PI5(1024,0:7),
     &        PR4(256,0:7) ,PI4(256,0:7),PR3(64,0:7)  ,PI3(64,0:7),
     &        PR2(16,0:7)  ,PI2(16,0:7),PR1(4,0:7)   ,PI1(4,0:7)
         COMMON/POLES/pr8,pi8,pr7,pi7,pr6,pi6,pr5,pi5,pr4,pi4,pr3,pi3,
     &                pr2,pi2,pr1,pi1

         real BR8(65536,7),BI8(65536,7),
     &        BR7(16384,7),BI7(16384,7),BR6(4096,7),BI6(4096,7),
     &        BR5(1024,7),BI5(1024,7),BR4(256,7) ,BI4(256,7),
     &        BR3(64,7),BI3(64,7),BR2(16,7),BI2(16,7),BR1(4,7),BI1(4,7)
         COMMON/BOXEXP/br8,bi8,br7,bi7,br6,bi6,br5,bi5,br4,bi4,br3,bi3,
     &                 br2,bi2,br1,bi1




