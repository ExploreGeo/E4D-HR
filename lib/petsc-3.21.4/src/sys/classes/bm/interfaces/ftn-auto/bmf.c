#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* bm.c */
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

#include "petscbm.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscbenchsetup_ PETSCBENCHSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscbenchsetup_ petscbenchsetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscbenchrun_ PETSCBENCHRUN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscbenchrun_ petscbenchrun
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscbenchsetfromoptions_ PETSCBENCHSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscbenchsetfromoptions_ petscbenchsetfromoptions
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscbenchsetup_(PetscBench bm, int *__ierr)
{
*__ierr = PetscBenchSetUp(
	(PetscBench)PetscToPointer((bm) ));
}
PETSC_EXTERN void  petscbenchrun_(PetscBench bm, int *__ierr)
{
*__ierr = PetscBenchRun(
	(PetscBench)PetscToPointer((bm) ));
}
PETSC_EXTERN void  petscbenchsetfromoptions_(PetscBench bm, int *__ierr)
{
*__ierr = PetscBenchSetFromOptions(
	(PetscBench)PetscToPointer((bm) ));
}
#if defined(__cplusplus)
}
#endif
