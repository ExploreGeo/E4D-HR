#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* tr.c */
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

#include "petscsnes.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesnewtontrsetnormtype_ SNESNEWTONTRSETNORMTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesnewtontrsetnormtype_ snesnewtontrsetnormtype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesnewtontrsetqntype_ SNESNEWTONTRSETQNTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesnewtontrsetqntype_ snesnewtontrsetqntype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesnewtontrsetfallbacktype_ SNESNEWTONTRSETFALLBACKTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesnewtontrsetfallbacktype_ snesnewtontrsetfallbacktype
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  snesnewtontrsetnormtype_(SNES snes,NormType *norm, int *__ierr)
{
*__ierr = SNESNewtonTRSetNormType(
	(SNES)PetscToPointer((snes) ),*norm);
}
PETSC_EXTERN void  snesnewtontrsetqntype_(SNES snes,SNESNewtonTRQNType *use, int *__ierr)
{
*__ierr = SNESNewtonTRSetQNType(
	(SNES)PetscToPointer((snes) ),*use);
}
PETSC_EXTERN void  snesnewtontrsetfallbacktype_(SNES snes,SNESNewtonTRFallbackType *ftype, int *__ierr)
{
*__ierr = SNESNewtonTRSetFallbackType(
	(SNES)PetscToPointer((snes) ),*ftype);
}
#if defined(__cplusplus)
}
#endif
