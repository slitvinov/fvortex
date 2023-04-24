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
      real Time
      real x
      real xmom
      real y
      real ymom

      common/params/n, Time, dt
      common/part/Np, s2, ovrlp, gnu

      write (raw, '(A, I8.8, A)') 'p.', iframe, '.raw'
      open (1, file=raw, status='REPLACE', access='STREAM')
      xmom = 0.
      ymom = 0.
      ang = 0.
      do 2 i = 1, Np
         x = xp(i)
         y = yp(i)
         g = gp(i)
         circ = circ + g
         xmom = xmom - g*y
         ymom = ymom + g*x
         ang = ang + 0.5*(x*x + y*y + s2)*g
         write (1) x, y, g
    2 end do
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
      write(1, '(    ''      Dimensions="'', I8, I8, ''">'')') Np, 2
      write(1, '(A)') '     <DataItem'
      write(1, '(A)') '       Dimensions="3 2"'
      write(1, '(A)') '       Format="XML">'
      write(1, '(    ''        '', I8, I8)') 0, 0
      write(1, '(    ''        '', I8, I8)') 1, 1
      write(1, '(    ''        '', I8, I8)') Np, 2
      write(1, '(A)') '      </DataItem>'
      write(1, '(A)') '     <DataItem'
      write(1, '(    ''       Dimensions="'', I8, I8, ''"'')') Np, 3
      write(1, '(A)') '       Format="Binary">'
      write(1, '(    ''        '', A)') raw
      write(1, '(A)') '     </DataItem>'
      write(1, '(A)') '    </DataItem>'
      write(1, '(A)') '   </Geometry>'
      write(1, '(A)') '  <Attribute'
      write(1, '(A)') '    Name="circ">'
      write(1, '(A)') '   <DataItem'
      write(1, '(A)') '     ItemType="HyperSlab"'
      write(1, '(    ''     Dimensions="'', I8, ''">'')') Np
      write(1, '(A)') '    <DataItem'
      write(1, '(A)') '      Dimensions="3"'
      write(1, '(A)') '      Format="XML">'
      write(1, '(    ''       '', 3I8)') 2, 3, Np
      write(1, '(A)') '    </DataItem>'
      write(1, '(A)') '    <DataItem'
      write(1, '(    ''      Dimensions="'', I8, ''"'')') Np
      write(1, '(A)') '      Format="Binary">'
      write(1, '(    ''       '', A)') raw
      write(1, '(A)') '    </DataItem>'
      write(1, '(A)') '   </DataItem>'
      write(1, '(A)') '  </Attribute>'
      write(1, '(A)') '  </Grid>'
      write(1, '(A)') ' </Domain>'
      write(1, '(A)') '</Xdmf>'
      close(1)

      write (10, 100) Time, xmom
      write (11, 100) Time, ymom
      write (12, 100) Time, Circ
      close(10)
      close(11)
      close(12)

  100 format(f10.4, 2x, e13.6)
      end
