#!/bin/sh

# clone and build petsc (specific tag)
git clone --depth 1 -b $PETSC_VERSION https://gitlab.com/petsc/petsc.git $PETSC_DIR
cd $PETSC_DIR
#./configure MAKE=/usr/bin/gmake CC=mpiicc CFLAGS="-g -O0 -fPIC" AR=/usr/bin/ar ARFLAGS=cr CXX=mpiicpc CXXFLAGS="-g -O0" FFLAGS="-g -O0" FC=mpiifort F77=mpiifort FCFLAGS="-g -O0" --disable-shared --with-pm=hydra --with-hwloc=embedded --enable-fast=no --enable-error-messages=all --with-device=ch3:sock --enable-g=meminit
./configure PETSC_ARCH=petsc-arch \
--with-cc=mpiicc --COPTFLAGS='-g -O0' \
--with-cxx=mpiicpc --CXXOPTFLAGS='-g -O0' \
--with-fc=mpiifort --FOPTFLAGS='-g -O0' \
--with-pm=hydra --with-hwloc=embedded --enable-fast=no --enable-error-messages=all --with-device=ch3:sock --enable-g=meminit \
--with-clanguage=c --with-debug=1 --with-shared-libraries=0 --download-hdf5 --download-metis --download-parmetis --download-hypre --download-hdf5-fortran-bindings=yes
make
rm -Rf petsc-arch/externalpackages
