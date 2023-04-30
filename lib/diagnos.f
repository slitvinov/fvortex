      subroutine diagnos(iframe)

C Calculates the linear/angular impulse and circulation of the flow.
C Differentiation of the impulse will give drag and lift.

      include 'main_dim.h'
      include 'part.h'

      character raw*(2 + 8 + 4)
      character xdmf*(2 + 8 + 6)
      integer i
      integer iframe
      integer n
      integer np
      real ang
      real circ
      real dt
      real g
      real gnu
      real ovrlp
      real s2
      real time
      real x
      real xmom
      real y
      real ymom

      common/params/n, time, dt
      common/part/np, s2, ovrlp, gnu

      write (raw, '(A, I8.8, A)') 'p.', iframe, '.raw'
      open (1, file = raw, status = 'REPLACE', form = 'UNFORMATTED', err
     $     = 101, access = 'DIRECT', recl = 12)
      xmom = 0.
      ymom = 0.
      ang = 0.
      do 2 i = 1, np
         x = xp(i)
         y = yp(i)
         g = gp(i)
         circ = circ + g
         xmom = xmom - g*y
         ymom = ymom + g*x
         ang = ang + 0.5*(x*x + y*y + s2)*g
         write (1, rec = i) x, y, g
    2 continue
      close (1)

      write (xdmf, '(A, I8.8, A)') 'p.', iframe, '.xdmf2'
      open (1, file=xdmf, status='REPLACE')
      write(1, '(A)') '<?xml version="1.0" encoding="UTF-8"?>'
      write(1, '(A)') '<Xdmf Version="2.0">'
      write(1, '(A)') ' <Domain>'
      write(1, '(A)') '  <Grid>'
      write(1, '(A)') '   <Topology'
      write(1, '(A)') '     TopologyType="Polyvertex">'
      write(1, '(A)') '   </Topology>'
      write(1, '(A)') '   <Geometry'
      write(1, '(A)') '     GeometryType="XY">'
      write(1, '(A)') '    <DataItem'
      write(1, '(A)') '      ItemType="HyperSlab"'
      write(1, '(    ''      Dimensions="'', I8, I8, ''">'')') np, 2
      write(1, '(A)') '     <DataItem'
      write(1, '(A)') '       Dimensions="3 2"'
      write(1, '(A)') '       Format="XML">'
      write(1, '(    ''        '', I8, I8)') 0, 0
      write(1, '(    ''        '', I8, I8)') 1, 1
      write(1, '(    ''        '', I8, I8)') np, 2
      write(1, '(A)') '      </DataItem>'
      write(1, '(A)') '     <DataItem'
      write(1, '(    ''       Dimensions="'', I8, I8, ''"'')') np, 3
      write(1, '(A)') '       Format="Binary">'
      write(1, '(    ''        '', A)') raw
      write(1, '(A)') '     </DataItem>'
      write(1, '(A)') '    </DataItem>'
      write(1, '(A)') '   </Geometry>'
      write(1, '(A)') '  <Attribute'
      write(1, '(A)') '    Name="circ">'
      write(1, '(A)') '   <DataItem'
      write(1, '(A)') '     ItemType="HyperSlab"'
      write(1, '(    ''     Dimensions="'', I8, ''">'')') np
      write(1, '(A)') '    <DataItem'
      write(1, '(A)') '      Dimensions="3"'
      write(1, '(A)') '      Format="XML">'
      write(1, '(    ''       '', 3I8)') 2, 3, np
      write(1, '(A)') '    </DataItem>'
      write(1, '(A)') '    <DataItem'
      write(1, '(    ''      Dimensions="'', I8, ''"'')') np
      write(1, '(A)') '      Format="Binary">'
      write(1, '(    ''       '', A)') raw
      write(1, '(A)') '    </DataItem>'
      write(1, '(A)') '   </DataItem>'
      write(1, '(A)') '  </Attribute>'
      write(1, '(A)') '  </Grid>'
      write(1, '(A)') ' </Domain>'
      write(1, '(A)') '</Xdmf>'
      close(1)

      write (10, '(SP, 2P, E23.16, 4(1X, E23.16))') time, xmom, ymom,
     $     Circ, ang
      close(10)

      return
 101  write (*, '(''fvortex: error: fail to write output'')')
      stop 1
      end
