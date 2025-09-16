#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* bncg.c */
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

#include "petsctao.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taobncggettype_ TAOBNCGGETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taobncggettype_ taobncggettype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taobncgsettype_ TAOBNCGSETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taobncgsettype_ taobncgsettype
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  taobncggettype_(Tao tao,TaoBNCGType *type, int *__ierr)
{
*__ierr = TaoBNCGGetType(
	(Tao)PetscToPointer((tao) ),
	(TaoBNCGType* )PetscToPointer((type) ));
}
PETSC_EXTERN void  taobncgsettype_(Tao tao,TaoBNCGType *type, int *__ierr)
{
*__ierr = TaoBNCGSetType(
	(Tao)PetscToPointer((tao) ),*type);
}
#if defined(__cplusplus)
}
#endif
