#!/bin/sh

cd src/pflotran
make clean
make -j4 pflotran_rxn

