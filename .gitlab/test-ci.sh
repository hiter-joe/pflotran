#!/bin/sh

# these paths were set in the Docker pflotran/mpich4:ubuntu22 image
MPICH_DIR=/scratch/mpich-4.1
MPICH_INSTALL_DIR=$MPICH_DIR/install

mkdir -p $PETSC_DIR
cd $PETSC_DIR
pwd
wget https://gitlab.com/petsc/petsc/-/archive/v3.19.3/petsc-v3.19.3.tar.gz
tar -xzvf petsc-v3.19.3.tar.gz
echo 'hello'
pwd
ls
