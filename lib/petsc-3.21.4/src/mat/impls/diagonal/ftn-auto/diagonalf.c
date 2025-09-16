#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* diagonal.c */
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
#define matdiagonalgetdiagonal_ MATDIAGONALGETDIAGONAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matdiagonalgetdiagonal_ matdiagonalgetdiagonal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matdiagonalrestorediagonal_ MATDIAGONALRESTOREDIAGONAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matdiagonalrestorediagonal_ matdiagonalrestorediagonal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matdiagonalgetinversediagonal_ MATDIAGONALGETINVERSEDIAGONAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matdiagonalgetinversediagonal_ matdiagonalgetinversediagonal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matdiagonalrestoreinversediagonal_ MATDIAGONALRESTOREINVERSEDIAGONAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matdiagonalrestoreinversediagonal_ matdiagonalrestoreinversediagonal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcreatediagonal_ MATCREATEDIAGONAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreatediagonal_ matcreatediagonal
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matdiagonalgetdiagonal_(Mat A,Vec *diag, int *__ierr)
{
*__ierr = MatDiagonalGetDiagonal(
	(Mat)PetscToPointer((A) ),diag);
}
PETSC_EXTERN void  matdiagonalrestorediagonal_(Mat A,Vec *diag, int *__ierr)
{
*__ierr = MatDiagonalRestoreDiagonal(
	(Mat)PetscToPointer((A) ),diag);
}
PETSC_EXTERN void  matdiagonalgetinversediagonal_(Mat A,Vec *inv_diag, int *__ierr)
{
*__ierr = MatDiagonalGetInverseDiagonal(
	(Mat)PetscToPointer((A) ),inv_diag);
}
PETSC_EXTERN void  matdiagonalrestoreinversediagonal_(Mat A,Vec *inv_diag, int *__ierr)
{
*__ierr = MatDiagonalRestoreInverseDiagonal(
	(Mat)PetscToPointer((A) ),inv_diag);
}
PETSC_EXTERN void  matcreatediagonal_(Vec diag,Mat *J, int *__ierr)
{
*__ierr = MatCreateDiagonal(
	(Vec)PetscToPointer((diag) ),J);
}
#if defined(__cplusplus)
}
#endif
