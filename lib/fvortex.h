subroutine box_1(npart,&
     s0,&
     xc1,&
     yc1,&
     ic1,&
     jc1,&
     npb1,&
     ds1,&
     kp1,&
     liststart)
  implicit none
  integer npart
  integer kp1
  integer, dimension(4) ic1
  integer, dimension(4) jc1
  integer, dimension(4, 2) npb1
  integer, dimension(4) liststart
  real s0
  real ds1
  real, dimension(4) xc1
  real, dimension(4) yc1
end subroutine

subroutine box_dim(npart,&
     xmin,&
     xmax,&
     ymin,&
     ymax)
  implicit none
  integer npart
  real xmax
  real xmin
  real ymax
  real ymin
end subroutine

subroutine condiff(npart,&
     islip,&
     visc_rmax,&
     istats)
  implicit none
  integer npart
  integer islip
  integer istats
  real visc_rmax
end subroutine

subroutine diagnos()
  implicit none
end subroutine

subroutine gaussian()
  implicit none
end subroutine

subroutine make_box(nmax,&
     ds1,&
     ds2,&
     kp1,&
     kp2,&
     kparent1,&
     kchildless1,&
     ic1,&
     jc1,&
     npb1,&
     iparent1,&
     imark1,&
     ipar1ch2,&
     ich2par1,&
     npb2,&
     ic2,&
     jc2,&
     xc2,&
     yc2,&
     ichildless1)
  implicit none
  integer nmax
  integer kp1
  integer kp2
  integer kparent1
  integer kchildless1
  integer, dimension(nmax/4) ic1
  integer, dimension(nmax/4) jc1
  integer, dimension(nmax/4, 2) npb1
  integer, dimension(nmax/4) iparent1
  integer, dimension(nmax/4) imark1
  integer, dimension(nmax/4, 4) ipar1ch2
  integer, dimension(nmax, 2) npb2
  integer, dimension(nmax) ich2par1
  integer, dimension(nmax) ic2
  integer, dimension(nmax) jc2
  integer, dimension(nmax/4) ichildless1
  real ds1
  real ds2
  real, dimension(nmax) xc2
  real, dimension(nmax) yc2
end subroutine

subroutine mv_ab(irk)
  implicit none
  integer irk
end subroutine

subroutine mv_eul()
  implicit none
end subroutine

subroutine mv_rk(visc_rmax)
  implicit none
  real visc_rmax
end subroutine

subroutine read_restart(time,&
     np,&
     s2,&
     ovrlp,&
     nvort,&
     xp,&
     yp,&
     gp)
  implicit none
  integer np
  integer nvort
  real time
  real s2
  real ovrlp
  real, dimension(nvort) xp
  real, dimension(nvort) yp
  real, dimension(nvort) gp
end subroutine

subroutine remesh()
  implicit none
end subroutine

subroutine vel_ext(tm)
  implicit none
  real tm
end subroutine

subroutine vort_field(iframe)
  implicit none
  integer iframe
end subroutine

subroutine write_restart(time,&
     np,&
     s2,&
     ovrlp,&
     nvort,&
     xp,&
     yp,&
     gp)
  implicit none
  integer np
  integer nvort
  real time
  real s2
  real ovrlp
  real, dimension(nvort) xp
  real, dimension(nvort) yp
  real, dimension(nvort) gp
end subroutine

