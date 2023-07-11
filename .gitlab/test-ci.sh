#!/bin/sh

# these paths were set in the Docker pflotran/mpich4:ubuntu22 image
MPICH_DIR=/scratch/mpich-4.1
MPICH_INSTALL_DIR=$MPICH_DIR/install

mkdir -p $PETSC_DIR && cd $PETSC_DIR
wget https://gitlab.com/petsc/petsc/-/archive/main/petsc-main.tar.gz && tar -xzvf petsc-main.tar.gz
pwd
ls

