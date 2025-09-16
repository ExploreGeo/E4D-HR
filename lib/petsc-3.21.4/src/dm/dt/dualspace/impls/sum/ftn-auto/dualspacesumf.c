#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dualspacesum.c */
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
#define petscdualspacesumgetnumsubspaces_ PETSCDUALSPACESUMGETNUMSUBSPACES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacesumgetnumsubspaces_ petscdualspacesumgetnumsubspaces
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacesumsetnumsubspaces_ PETSCDUALSPACESUMSETNUMSUBSPACES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacesumsetnumsubspaces_ petscdualspacesumsetnumsubspaces
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacesumgetconcatenate_ PETSCDUALSPACESUMGETCONCATENATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacesumgetconcatenate_ petscdualspacesumgetconcatenate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacesumsetconcatenate_ PETSCDUALSPACESUMSETCONCATENATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacesumsetconcatenate_ petscdualspacesumsetconcatenate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacesumgetsubspace_ PETSCDUALSPACESUMGETSUBSPACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacesumgetsubspace_ petscdualspacesumgetsubspace
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacesumsetsubspace_ PETSCDUALSPACESUMSETSUBSPACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacesumsetsubspace_ petscdualspacesumsetsubspace
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacesumsetinterleave_ PETSCDUALSPACESUMSETINTERLEAVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacesumsetinterleave_ petscdualspacesumsetinterleave
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacesumgetinterleave_ PETSCDUALSPACESUMGETINTERLEAVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacesumgetinterleave_ petscdualspacesumgetinterleave
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacecreatesum_ PETSCDUALSPACECREATESUM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacecreatesum_ petscdualspacecreatesum
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscdualspacesumgetnumsubspaces_(PetscDualSpace sp,PetscInt *numSumSpaces, int *__ierr)
{
*__ierr = PetscDualSpaceSumGetNumSubspaces(
	(PetscDualSpace)PetscToPointer((sp) ),numSumSpaces);
}
PETSC_EXTERN void  petscdualspacesumsetnumsubspaces_(PetscDualSpace sp,PetscInt *numSumSpaces, int *__ierr)
{
*__ierr = PetscDualSpaceSumSetNumSubspaces(
	(PetscDualSpace)PetscToPointer((sp) ),*numSumSpaces);
}
PETSC_EXTERN void  petscdualspacesumgetconcatenate_(PetscDualSpace sp,PetscBool *concatenate, int *__ierr)
{
*__ierr = PetscDualSpaceSumGetConcatenate(
	(PetscDualSpace)PetscToPointer((sp) ),concatenate);
}
PETSC_EXTERN void  petscdualspacesumsetconcatenate_(PetscDualSpace sp,PetscBool *concatenate, int *__ierr)
{
*__ierr = PetscDualSpaceSumSetConcatenate(
	(PetscDualSpace)PetscToPointer((sp) ),*concatenate);
}
PETSC_EXTERN void  petscdualspacesumgetsubspace_(PetscDualSpace sp,PetscInt *s,PetscDualSpace *subsp, int *__ierr)
{
*__ierr = PetscDualSpaceSumGetSubspace(
	(PetscDualSpace)PetscToPointer((sp) ),*s,subsp);
}
PETSC_EXTERN void  petscdualspacesumsetsubspace_(PetscDualSpace sp,PetscInt *s,PetscDualSpace subsp, int *__ierr)
{
*__ierr = PetscDualSpaceSumSetSubspace(
	(PetscDualSpace)PetscToPointer((sp) ),*s,
	(PetscDualSpace)PetscToPointer((subsp) ));
}
PETSC_EXTERN void  petscdualspacesumsetinterleave_(PetscDualSpace sp,PetscBool *interleave_basis,PetscBool *interleave_components, int *__ierr)
{
*__ierr = PetscDualSpaceSumSetInterleave(
	(PetscDualSpace)PetscToPointer((sp) ),*interleave_basis,*interleave_components);
}
PETSC_EXTERN void  petscdualspacesumgetinterleave_(PetscDualSpace sp,PetscBool *interleave_basis,PetscBool *interleave_components, int *__ierr)
{
*__ierr = PetscDualSpaceSumGetInterleave(
	(PetscDualSpace)PetscToPointer((sp) ),interleave_basis,interleave_components);
}
PETSC_EXTERN void  petscdualspacecreatesum_(PetscInt *numSubspaces, PetscDualSpace subspaces[],PetscBool *concatenate,PetscDualSpace *sumSpace, int *__ierr)
{
*__ierr = PetscDualSpaceCreateSum(*numSubspaces,subspaces,*concatenate,sumSpace);
}
#if defined(__cplusplus)
}
#endif
