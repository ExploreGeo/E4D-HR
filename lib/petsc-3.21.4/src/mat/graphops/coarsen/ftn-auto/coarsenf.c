#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* coarsen.c */
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
#define matcoarsenapply_ MATCOARSENAPPLY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcoarsenapply_ matcoarsenapply
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcoarsensetadjacency_ MATCOARSENSETADJACENCY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcoarsensetadjacency_ matcoarsensetadjacency
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcoarsensetstrictaggs_ MATCOARSENSETSTRICTAGGS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcoarsensetstrictaggs_ matcoarsensetstrictaggs
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcoarsendestroy_ MATCOARSENDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcoarsendestroy_ matcoarsendestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcoarsensetfromoptions_ MATCOARSENSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcoarsensetfromoptions_ matcoarsensetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcoarsensetmaximumiterations_ MATCOARSENSETMAXIMUMITERATIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcoarsensetmaximumiterations_ matcoarsensetmaximumiterations
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcoarsensetstrengthindex_ MATCOARSENSETSTRENGTHINDEX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcoarsensetstrengthindex_ matcoarsensetstrengthindex
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcoarsensetthreshold_ MATCOARSENSETTHRESHOLD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcoarsensetthreshold_ matcoarsensetthreshold
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcoarsencreate_ MATCOARSENCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcoarsencreate_ matcoarsencreate
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matcoarsenapply_(MatCoarsen coarser, int *__ierr)
{
*__ierr = MatCoarsenApply(
	(MatCoarsen)PetscToPointer((coarser) ));
}
PETSC_EXTERN void  matcoarsensetadjacency_(MatCoarsen agg,Mat adj, int *__ierr)
{
*__ierr = MatCoarsenSetAdjacency(
	(MatCoarsen)PetscToPointer((agg) ),
	(Mat)PetscToPointer((adj) ));
}
PETSC_EXTERN void  matcoarsensetstrictaggs_(MatCoarsen agg,PetscBool *str, int *__ierr)
{
*__ierr = MatCoarsenSetStrictAggs(
	(MatCoarsen)PetscToPointer((agg) ),*str);
}
PETSC_EXTERN void  matcoarsendestroy_(MatCoarsen *agg, int *__ierr)
{
*__ierr = MatCoarsenDestroy(agg);
}
PETSC_EXTERN void  matcoarsensetfromoptions_(MatCoarsen coarser, int *__ierr)
{
*__ierr = MatCoarsenSetFromOptions(
	(MatCoarsen)PetscToPointer((coarser) ));
}
PETSC_EXTERN void  matcoarsensetmaximumiterations_(MatCoarsen coarse,PetscInt *n, int *__ierr)
{
*__ierr = MatCoarsenSetMaximumIterations(
	(MatCoarsen)PetscToPointer((coarse) ),*n);
}
PETSC_EXTERN void  matcoarsensetstrengthindex_(MatCoarsen coarse,PetscInt *n,PetscInt idx[], int *__ierr)
{
*__ierr = MatCoarsenSetStrengthIndex(
	(MatCoarsen)PetscToPointer((coarse) ),*n,idx);
}
PETSC_EXTERN void  matcoarsensetthreshold_(MatCoarsen coarse,PetscReal *b, int *__ierr)
{
*__ierr = MatCoarsenSetThreshold(
	(MatCoarsen)PetscToPointer((coarse) ),*b);
}
PETSC_EXTERN void  matcoarsencreate_(MPI_Fint * comm,MatCoarsen *newcrs, int *__ierr)
{
*__ierr = MatCoarsenCreate(
	MPI_Comm_f2c(*(comm)),newcrs);
}
#if defined(__cplusplus)
}
#endif
