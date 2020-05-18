integer :: nhlp, np_max, nbox_max
parameter(Nhlp=5000000)
parameter(np_max=3450000, nbox_max=60001)
integer :: it(np_max)
real :: Xbox(nbox_max), Ybox(nbox_max), Prbox(nbox_max, 0:7)
real :: Pibox(nbox_max, 0:7), xt(np_max), yt(np_max), gt(np_max)
common/tempor/Xbox, Ybox, Prbox, Pibox, xt, yt, gt, it
