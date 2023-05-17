#include <stdio.h>
#include "def.h"

void diagnos_(int *);

extern struct {
  int np;
  float s2;
  float ovrlp;
  float gnu;
} part_;

extern struct {
  int n;
  float time;
  float dt;
} params_;

extern struct {
  float xp[nvort];
  float yp[nvort];
  float gp[nvort];
} vort1_;

int main() {
  int i;
  int iframe;
  part_.np = 10;
  part_.s2 = 1e-4;
  params_.time = 0;
  iframe = 0;
  for (i = 0; i < part_.np; i++) {
    vort1_.xp[i] = i;
    vort1_.yp[i] = 10 * i;
    vort1_.gp[i] = 100 * i;
  }
  diagnos_(&iframe);
}

/*
../../tool/def ../../lib/*.[fh] > def.h
c99 -g3 diagnos.c ../../lib/libfvortex.a -lgfortran
*/
