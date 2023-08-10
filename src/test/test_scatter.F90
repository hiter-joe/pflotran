program test
#include "petsc/finclude/petscvec.h"
  use petscvec
  implicit none

  PetscMPIInt :: size
  PetscMPIInt :: rank
  
  Vec :: vecseq
  Vec :: vecmpi
  IS :: is
  VecScatter :: scatter
  PetscViewer :: viewer
  
  PetscInt :: n
  PetscErrorCode :: ierr

  call PetscInitialize(PETSC_NULL_CHARACTER, ierr)
  call MPI_Comm_size(PETSC_COMM_WORLD,size,ierr)
  call MPI_Comm_rank(PETSC_COMM_WORLD,rank,ierr)

  if (rank == 0) print *, 'Beginning of Fortran90 test program'

  n = 18000

  call VecCreateSeq(PETSC_COMM_SELF,n,vecseq,ierr);CHKERRQ(ierr) 
  call VecCreateMPI(PETSC_COMM_WORLD,PETSC_DETERMINE,n,vecmpi, &
                    ierr);CHKERRQ(ierr) 

  call ISCreateStride(PETSC_COMM_WORLD,n,0,1,is,ierr);CHKERRQ(ierr)
  call VecScatterCreate(vecseq,is,vecmpi,PETSC_NULL_IS,scatter, &
                        ierr);CHKERRQ(ierr)
  call ISDestroy(is,ierr);CHKERRQ(ierr)

  if (rank == 0) print *, 'End of Fortran90 test program'
 
  call PetscFinalize(ierr)
 
end program test
