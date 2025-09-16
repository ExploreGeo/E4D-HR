#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* pcis.c */
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

#include "petscpc.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcissetusestiffnessscaling_ PCISSETUSESTIFFNESSSCALING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcissetusestiffnessscaling_ pcissetusestiffnessscaling
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcissetsubdomaindiagonalscaling_ PCISSETSUBDOMAINDIAGONALSCALING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcissetsubdomaindiagonalscaling_ pcissetsubdomaindiagonalscaling
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcissetsubdomainscalingfactor_ PCISSETSUBDOMAINSCALINGFACTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcissetsubdomainscalingfactor_ pcissetsubdomainscalingfactor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcissetup_ PCISSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcissetup_ pcissetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcisreset_ PCISRESET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcisreset_ pcisreset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcisinitialize_ PCISINITIALIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcisinitialize_ pcisinitialize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcisapplyschur_ PCISAPPLYSCHUR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcisapplyschur_ pcisapplyschur
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcisscatterarrayntovecb_ PCISSCATTERARRAYNTOVECB
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcisscatterarrayntovecb_ pcisscatterarrayntovecb
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcisapplyinvschur_ PCISAPPLYINVSCHUR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcisapplyinvschur_ pcisapplyinvschur
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  pcissetusestiffnessscaling_(PC pc,PetscBool *use, int *__ierr)
{
*__ierr = PCISSetUseStiffnessScaling(
	(PC)PetscToPointer((pc) ),*use);
}
PETSC_EXTERN void  pcissetsubdomaindiagonalscaling_(PC pc,Vec scaling_factors, int *__ierr)
{
*__ierr = PCISSetSubdomainDiagonalScaling(
	(PC)PetscToPointer((pc) ),
	(Vec)PetscToPointer((scaling_factors) ));
}
PETSC_EXTERN void  pcissetsubdomainscalingfactor_(PC pc,PetscScalar *scal, int *__ierr)
{
*__ierr = PCISSetSubdomainScalingFactor(
	(PC)PetscToPointer((pc) ),*scal);
}
PETSC_EXTERN void  pcissetup_(PC pc,PetscBool *computematrices,PetscBool *computesolvers, int *__ierr)
{
*__ierr = PCISSetUp(
	(PC)PetscToPointer((pc) ),*computematrices,*computesolvers);
}
PETSC_EXTERN void  pcisreset_(PC pc, int *__ierr)
{
*__ierr = PCISReset(
	(PC)PetscToPointer((pc) ));
}
PETSC_EXTERN void  pcisinitialize_(PC pc, int *__ierr)
{
*__ierr = PCISInitialize(
	(PC)PetscToPointer((pc) ));
}
PETSC_EXTERN void  pcisapplyschur_(PC pc,Vec v,Vec vec1_B,Vec vec2_B,Vec vec1_D,Vec vec2_D, int *__ierr)
{
*__ierr = PCISApplySchur(
	(PC)PetscToPointer((pc) ),
	(Vec)PetscToPointer((v) ),
	(Vec)PetscToPointer((vec1_B) ),
	(Vec)PetscToPointer((vec2_B) ),
	(Vec)PetscToPointer((vec1_D) ),
	(Vec)PetscToPointer((vec2_D) ));
}
PETSC_EXTERN void  pcisscatterarrayntovecb_(PC pc,PetscScalar *array_N,Vec v_B,InsertMode *imode,ScatterMode *smode, int *__ierr)
{
*__ierr = PCISScatterArrayNToVecB(
	(PC)PetscToPointer((pc) ),array_N,
	(Vec)PetscToPointer((v_B) ),*imode,*smode);
}
PETSC_EXTERN void  pcisapplyinvschur_(PC pc,Vec b,Vec x,Vec vec1_N,Vec vec2_N, int *__ierr)
{
*__ierr = PCISApplyInvSchur(
	(PC)PetscToPointer((pc) ),
	(Vec)PetscToPointer((b) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((vec1_N) ),
	(Vec)PetscToPointer((vec2_N) ));
}
#if defined(__cplusplus)
}
#endif
