#!/bin/sh

# these paths were set in the Docker pflotran/mpich4:ubuntu22 image
MPICH_DIR=/scratch/mpich-4.1
MPICH_INSTALL_DIR=$MPICH_DIR/install


echo 'here0'
wget https://www.mpich.org/static/downloads/4.1/mpich-4.1.tar.gz
tar -xzvf mpich-4.1.tar.gz
echo 'here1'
pwd
ls
