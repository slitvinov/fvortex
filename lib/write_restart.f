c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        subroutine write_restart(ivalue,time,np,s2,ovrlp,nmin,radin,
     &          nvort,xp,yp,gp,
     &          last_x,last_u,last_udot,last_y,last_v,last_vdot,
     &          last_th,last_w,last_wdot)

c  Write the files needed to restart the simulation.
c  Values for u and v are freestream velocity, thus (-) body vel.

        implicit none

        integer ivalue,np,nmin,nvort
        real time,s2,ovrlp,radin,xp(nvort),yp(nvort),gp(nvort)
        real last_x,last_u,last_udot,last_y,last_v,last_vdot
        real last_th,last_w,last_wdot
        character value
c----------------------------------------------------------------------

        value=char(ivalue+48)
        
        open(1,file='params'//value//'.cont',status='new')
        write(1,*)time
        write(1,*)np
        write(1,*)s2
        write(1,*)ovrlp
        write(1,*)nmin
        write(1,*)radin
        write(1,*)last_x,last_u,last_udot
        write(1,*)last_y,last_v,last_vdot
        write(1,*)last_th,last_w,last_wdot
        close(1)

        open(1,file='x'//value//'.cont',status='new',form='unformatted')
        write(1)xp
        close(1)

        open(1,file='y'//value//'.cont',status='new',form='unformatted')
        write(1)yp
        close(1)

        open(1,file='g'//value//'.cont',status='new',form='unformatted')
        write(1)gp
        close(1)

        return
        end



