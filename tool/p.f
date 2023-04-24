      program main
      integer Np
      character partfile*256
      Np = 32149
      partfile = 'p.00000000.raw'
      write(*, '(A)') '<?xml version="1.0" encoding="UTF-8"?>'
      write(*, '(A)') '<Xdmf Version="2.0">'
      write(*, '(A)') '  <Domain>'
      write(*, '(A)') '    <Grid>'
      write(*, '(A)') '      <Topology'
      write(*, '(A)') '    TopologyType="Polyvertex">'
      write(*, '(A)') '      </Topology>'
      write(*, '(A)') '      <Geometry'
      write(*, '(A)') '    GeometryType="XY">'
      write(*, '(A)') '  <DataItem'
      write(*, '(A)') '      ItemType="HyperSlab"'
      write(*, '(''      Dimensions="'', I8, I8, ''">'')') Np, 2
      write(*, '(A)') '    <DataItem'
      write(*, '(A)') '        Dimensions="3 2"'
      write(*, '(A)') '        Format="XML">'
      write(*, '(    ''      '', I8, I8)') 0, 0
      write(*, '(    ''      '', I8, I8)') 1, 1
      write(*, '(    ''      '', I8, I8)') Np, 2
      write(*, '(A)') '    </DataItem>'
      write(*, '(A)') '   <DataItem'
      write(*, '(    ''       Dimensions="'', I8, I8, ''">'')') Np, 3
      write(*, '(A)') '        Format="Binary">'
      write(*, '(A)') '      p.00000000.raw'
      write(*, '(A)') '    </DataItem>'
      write(*, '(A)') '  </DataItem>'
      write(*, '(A)') '      </Geometry>'
      write(*, '(A)') '      <Attribute'
      write(*, '(A)') '    Name="circ">'
      write(*, '(A)') '  <DataItem'
      write(*, '(A)') '      ItemType="HyperSlab"'
      write(*, '(    ''      Dimensions="'', I8, ''"'')') Np
      write(*, '(A)') '    <DataItem'
      write(*, '(A)') '        Dimensions="3"'
      write(*, '(A)') '        Format="XML">'
      write(*, '(    ''      '', 3I8)') 2, 3, 3 * Np
      write(*, '(A)') '    </DataItem>'
      write(*, '(A)') '    <DataItem'
      write(*, '(    ''       Dimensions="'', I8, ''"'')') Np
      write(*, '(A)') '       Format="Binary">'
      write(*, '(    ''       '', A)') partfile
      write(*, '(A)') '    </DataItem>'
      write(*, '(A)') '  </DataItem>'
      write(*, '(A)') '      </Attribute>'
      write(*, '(A)') '    </Grid>'
      write(*, '(A)') '  </Domain>'
      write(*, '(A)') '</Xdmf>'
      end
