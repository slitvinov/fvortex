#include <stdio.h>

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
  float xp[5000000];
  float yp[5000000];
  float gp[5000000];
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
c99 diagnos.c ../../lib/libfvortex.a -lgfortran
*/
