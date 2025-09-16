#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* axpy.c */
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

#include "petscmat.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define mataxpy_ MATAXPY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define mataxpy_ mataxpy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matshift_ MATSHIFT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matshift_ matshift
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matdiagonalset_ MATDIAGONALSET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matdiagonalset_ matdiagonalset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define mataypx_ MATAYPX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define mataypx_ mataypx
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matfilter_ MATFILTER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matfilter_ matfilter
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  mataxpy_(Mat Y,PetscScalar *a,Mat X,MatStructure *str, int *__ierr)
{
*__ierr = MatAXPY(
	(Mat)PetscToPointer((Y) ),*a,
	(Mat)PetscToPointer((X) ),*str);
}
PETSC_EXTERN void  matshift_(Mat Y,PetscScalar *a, int *__ierr)
{
*__ierr = MatShift(
	(Mat)PetscToPointer((Y) ),*a);
}
PETSC_EXTERN void  matdiagonalset_(Mat Y,Vec D,InsertMode *is, int *__ierr)
{
*__ierr = MatDiagonalSet(
	(Mat)PetscToPointer((Y) ),
	(Vec)PetscToPointer((D) ),*is);
}
PETSC_EXTERN void  mataypx_(Mat Y,PetscScalar *a,Mat X,MatStructure *str, int *__ierr)
{
*__ierr = MatAYPX(
	(Mat)PetscToPointer((Y) ),*a,
	(Mat)PetscToPointer((X) ),*str);
}
PETSC_EXTERN void  matfilter_(Mat A,PetscReal *tol,PetscBool *compress,PetscBool *keep, int *__ierr)
{
*__ierr = MatFilter(
	(Mat)PetscToPointer((A) ),*tol,*compress,*keep);
}
#if defined(__cplusplus)
}
#endif
