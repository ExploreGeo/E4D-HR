#!/usr/bin/python3
if __name__ == '__main__':
  import sys
  import os
  sys.path.insert(0, os.path.abspath('config'))
  import configure
  configure_options = [
    '--with-blaslapack-dir=/opt/intel/oneapi/mkl/2025.0',
    '--with-cc=mpiicc -cc=icx',
    '--with-cxx=mpiicpc -cxx=icpx',
    '--with-debugging=0',
    '--with-fc=mpiifort -fc=ifx',
    'COPTFLAGS=-O2 -march=native -mtune=native',
    'CXXOPTFLAGS=-O2 -march=native -mtune=native',
    'FOPTFLAGS=-O2 -march=native -mtune=native',
    'PETSC_ARCH=arch-linux-c-opt',
  ]
  configure.petsc_configure(configure_options)
