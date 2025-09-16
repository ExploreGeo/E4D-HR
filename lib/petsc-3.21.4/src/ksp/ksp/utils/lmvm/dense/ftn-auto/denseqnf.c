#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* denseqn.c */
/* Fortran interface file */

/*
* This file was generated automatically by bfort from the C source
* file.  
 */

#ifdef PETSC_USE_POINTER_CONVERSION
#if defined(__cplusplus)
extern "C" { 
#endif 
extern void *PetscToPointer(void*);
extern int PetscFromPointer(void *);
extern void PetscRmPointer(void*);
#if defined(__cplusplus)
} 
#endif 

#else

#define PetscToPointer(a) (*(PetscFortranAddr *)(a))
#define PetscFromPointer(a) (PetscFortranAddr)(a)
#define PetscRmPointer(a)
#endif

#include "petscksp.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcreatelmvmdqn_ MATCREATELMVMDQN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreatelmvmdqn_ matcreatelmvmdqn
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcreatelmvmdbfgs_ MATCREATELMVMDBFGS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreatelmvmdbfgs_ matcreatelmvmdbfgs
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcreatelmvmddfp_ MATCREATELMVMDDFP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreatelmvmddfp_ matcreatelmvmddfp
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matlmvmdensesettype_ MATLMVMDENSESETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matlmvmdensesettype_ matlmvmdensesettype
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matcreatelmvmdqn_(MPI_Fint * comm,PetscInt *n,PetscInt *N,Mat *B, int *__ierr)
{
*__ierr = MatCreateLMVMDQN(
	MPI_Comm_f2c(*(comm)),*n,*N,B);
}
PETSC_EXTERN void  matcreatelmvmdbfgs_(MPI_Fint * comm,PetscInt *n,PetscInt *N,Mat *B, int *__ierr)
{
*__ierr = MatCreateLMVMDBFGS(
	MPI_Comm_f2c(*(comm)),*n,*N,B);
}
PETSC_EXTERN void  matcreatelmvmddfp_(MPI_Fint * comm,PetscInt *n,PetscInt *N,Mat *B, int *__ierr)
{
*__ierr = MatCreateLMVMDDFP(
	MPI_Comm_f2c(*(comm)),*n,*N,B);
}
PETSC_EXTERN void  matlmvmdensesettype_(Mat B,MatLMVMDenseType *type, int *__ierr)
{
*__ierr = MatLMVMDenseSetType(
	(Mat)PetscToPointer((B) ),*type);
}
#if defined(__cplusplus)
}
#endif
