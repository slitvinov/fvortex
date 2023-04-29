      integer nhlp
      integer np_max
      integer nbox_max
      parameter(Nhlp=5000000)
      parameter(np_max=3450000, nbox_max=60001)
      integer it(np_max)
      real Xbox(nbox_max)
      real Ybox(nbox_max)
      real Prbox(nbox_max,0:7)
      real Pibox(nbox_max,0:7)
      real xt(np_max)
      real yt(np_max)
      real gt(np_max)
      common/tempor/Xbox, Ybox, Prbox, Pibox, xt, yt, gt, it
