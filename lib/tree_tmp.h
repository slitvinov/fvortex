integer :: nhlp, np_max, nbox_max
parameter(Nhlp=5000000)
parameter(np_max=1450000, nbox_max=60001)
integer :: IT(np_max)
real :: Xbox(nbox_max), Ybox(nbox_max), Prbox(nbox_max, 0:7)
real :: Pibox(nbox_max, 0:7), XT(np_max), YT(np_max), GT(np_max)
COMMON/TEMPOR/Xbox, Ybox, Prbox, Pibox, XT, YT, GT, IT
