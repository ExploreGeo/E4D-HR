#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* networkview.c */
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

#include "petscdmnetwork.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkviewsetshowranks_ DMNETWORKVIEWSETSHOWRANKS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkviewsetshowranks_ dmnetworkviewsetshowranks
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkviewsetshowglobal_ DMNETWORKVIEWSETSHOWGLOBAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkviewsetshowglobal_ dmnetworkviewsetshowglobal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkviewsetshowvertices_ DMNETWORKVIEWSETSHOWVERTICES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkviewsetshowvertices_ dmnetworkviewsetshowvertices
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkviewsetshownumbering_ DMNETWORKVIEWSETSHOWNUMBERING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkviewsetshownumbering_ dmnetworkviewsetshownumbering
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkviewsetviewranks_ DMNETWORKVIEWSETVIEWRANKS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkviewsetviewranks_ dmnetworkviewsetviewranks
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmnetworkviewsetshowranks_(DM dm,PetscBool *showranks, int *__ierr)
{
*__ierr = DMNetworkViewSetShowRanks(
	(DM)PetscToPointer((dm) ),*showranks);
}
PETSC_EXTERN void  dmnetworkviewsetshowglobal_(DM dm,PetscBool *showglobal, int *__ierr)
{
*__ierr = DMNetworkViewSetShowGlobal(
	(DM)PetscToPointer((dm) ),*showglobal);
}
PETSC_EXTERN void  dmnetworkviewsetshowvertices_(DM dm,PetscBool *showvertices, int *__ierr)
{
*__ierr = DMNetworkViewSetShowVertices(
	(DM)PetscToPointer((dm) ),*showvertices);
}
PETSC_EXTERN void  dmnetworkviewsetshownumbering_(DM dm,PetscBool *shownumbering, int *__ierr)
{
*__ierr = DMNetworkViewSetShowNumbering(
	(DM)PetscToPointer((dm) ),*shownumbering);
}
PETSC_EXTERN void  dmnetworkviewsetviewranks_(DM dm,IS viewranks, int *__ierr)
{
*__ierr = DMNetworkViewSetViewRanks(
	(DM)PetscToPointer((dm) ),
	(IS)PetscToPointer((viewranks) ));
}
#if defined(__cplusplus)
}
#endif
