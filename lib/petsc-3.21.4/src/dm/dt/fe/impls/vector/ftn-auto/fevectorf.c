#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* fevector.c */
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

#include "petscfe.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfecreatevector_ PETSCFECREATEVECTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfecreatevector_ petscfecreatevector
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscfecreatevector_(PetscFE scalar_fe,PetscInt *num_copies,PetscBool *interleave_basis,PetscBool *interleave_components,PetscFE *vector_fe, int *__ierr)
{
*__ierr = PetscFECreateVector(
	(PetscFE)PetscToPointer((scalar_fe) ),*num_copies,*interleave_basis,*interleave_components,vector_fe);
}
#if defined(__cplusplus)
}
#endif
