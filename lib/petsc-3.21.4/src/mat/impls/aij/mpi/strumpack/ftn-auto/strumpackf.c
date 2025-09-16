#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* strumpack.c */
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
#define matstrumpacksetreordering_ MATSTRUMPACKSETREORDERING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpacksetreordering_ matstrumpacksetreordering
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpackgetreordering_ MATSTRUMPACKGETREORDERING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpackgetreordering_ matstrumpackgetreordering
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpacksetcolperm_ MATSTRUMPACKSETCOLPERM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpacksetcolperm_ matstrumpacksetcolperm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpackgetcolperm_ MATSTRUMPACKGETCOLPERM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpackgetcolperm_ matstrumpackgetcolperm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpacksetgpu_ MATSTRUMPACKSETGPU
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpacksetgpu_ matstrumpacksetgpu
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpackgetgpu_ MATSTRUMPACKGETGPU
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpackgetgpu_ matstrumpackgetgpu
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpacksetcompression_ MATSTRUMPACKSETCOMPRESSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpacksetcompression_ matstrumpacksetcompression
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpackgetcompression_ MATSTRUMPACKGETCOMPRESSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpackgetcompression_ matstrumpackgetcompression
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpacksetcompreltol_ MATSTRUMPACKSETCOMPRELTOL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpacksetcompreltol_ matstrumpacksetcompreltol
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpackgetcompreltol_ MATSTRUMPACKGETCOMPRELTOL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpackgetcompreltol_ matstrumpackgetcompreltol
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpacksetcompabstol_ MATSTRUMPACKSETCOMPABSTOL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpacksetcompabstol_ matstrumpacksetcompabstol
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpackgetcompabstol_ MATSTRUMPACKGETCOMPABSTOL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpackgetcompabstol_ matstrumpackgetcompabstol
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpacksetcompleafsize_ MATSTRUMPACKSETCOMPLEAFSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpacksetcompleafsize_ matstrumpacksetcompleafsize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpackgetcompleafsize_ MATSTRUMPACKGETCOMPLEAFSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpackgetcompleafsize_ matstrumpackgetcompleafsize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpacksetgeometricnxyz_ MATSTRUMPACKSETGEOMETRICNXYZ
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpacksetgeometricnxyz_ matstrumpacksetgeometricnxyz
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpacksetgeometriccomponents_ MATSTRUMPACKSETGEOMETRICCOMPONENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpacksetgeometriccomponents_ matstrumpacksetgeometriccomponents
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpacksetgeometricwidth_ MATSTRUMPACKSETGEOMETRICWIDTH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpacksetgeometricwidth_ matstrumpacksetgeometricwidth
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpacksetcompminsepsize_ MATSTRUMPACKSETCOMPMINSEPSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpacksetcompminsepsize_ matstrumpacksetcompminsepsize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpackgetcompminsepsize_ MATSTRUMPACKGETCOMPMINSEPSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpackgetcompminsepsize_ matstrumpackgetcompminsepsize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpacksetcomplossyprecision_ MATSTRUMPACKSETCOMPLOSSYPRECISION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpacksetcomplossyprecision_ matstrumpacksetcomplossyprecision
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpackgetcomplossyprecision_ MATSTRUMPACKGETCOMPLOSSYPRECISION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpackgetcomplossyprecision_ matstrumpackgetcomplossyprecision
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpacksetcompbutterflylevels_ MATSTRUMPACKSETCOMPBUTTERFLYLEVELS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpacksetcompbutterflylevels_ matstrumpacksetcompbutterflylevels
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpackgetcompbutterflylevels_ MATSTRUMPACKGETCOMPBUTTERFLYLEVELS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpackgetcompbutterflylevels_ matstrumpackgetcompbutterflylevels
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matstrumpacksetreordering_(Mat F,MatSTRUMPACKReordering *reordering, int *__ierr)
{
*__ierr = MatSTRUMPACKSetReordering(
	(Mat)PetscToPointer((F) ),*reordering);
}
PETSC_EXTERN void  matstrumpackgetreordering_(Mat F,MatSTRUMPACKReordering *reordering, int *__ierr)
{
*__ierr = MatSTRUMPACKGetReordering(
	(Mat)PetscToPointer((F) ),reordering);
}
PETSC_EXTERN void  matstrumpacksetcolperm_(Mat F,PetscBool *cperm, int *__ierr)
{
*__ierr = MatSTRUMPACKSetColPerm(
	(Mat)PetscToPointer((F) ),*cperm);
}
PETSC_EXTERN void  matstrumpackgetcolperm_(Mat F,PetscBool *cperm, int *__ierr)
{
*__ierr = MatSTRUMPACKGetColPerm(
	(Mat)PetscToPointer((F) ),cperm);
}
PETSC_EXTERN void  matstrumpacksetgpu_(Mat F,PetscBool *gpu, int *__ierr)
{
*__ierr = MatSTRUMPACKSetGPU(
	(Mat)PetscToPointer((F) ),*gpu);
}
PETSC_EXTERN void  matstrumpackgetgpu_(Mat F,PetscBool *gpu, int *__ierr)
{
*__ierr = MatSTRUMPACKGetGPU(
	(Mat)PetscToPointer((F) ),gpu);
}
PETSC_EXTERN void  matstrumpacksetcompression_(Mat F,MatSTRUMPACKCompressionType *comp, int *__ierr)
{
*__ierr = MatSTRUMPACKSetCompression(
	(Mat)PetscToPointer((F) ),*comp);
}
PETSC_EXTERN void  matstrumpackgetcompression_(Mat F,MatSTRUMPACKCompressionType *comp, int *__ierr)
{
*__ierr = MatSTRUMPACKGetCompression(
	(Mat)PetscToPointer((F) ),comp);
}
PETSC_EXTERN void  matstrumpacksetcompreltol_(Mat F,PetscReal *rtol, int *__ierr)
{
*__ierr = MatSTRUMPACKSetCompRelTol(
	(Mat)PetscToPointer((F) ),*rtol);
}
PETSC_EXTERN void  matstrumpackgetcompreltol_(Mat F,PetscReal *rtol, int *__ierr)
{
*__ierr = MatSTRUMPACKGetCompRelTol(
	(Mat)PetscToPointer((F) ),rtol);
}
PETSC_EXTERN void  matstrumpacksetcompabstol_(Mat F,PetscReal *atol, int *__ierr)
{
*__ierr = MatSTRUMPACKSetCompAbsTol(
	(Mat)PetscToPointer((F) ),*atol);
}
PETSC_EXTERN void  matstrumpackgetcompabstol_(Mat F,PetscReal *atol, int *__ierr)
{
*__ierr = MatSTRUMPACKGetCompAbsTol(
	(Mat)PetscToPointer((F) ),atol);
}
PETSC_EXTERN void  matstrumpacksetcompleafsize_(Mat F,PetscInt *leaf_size, int *__ierr)
{
*__ierr = MatSTRUMPACKSetCompLeafSize(
	(Mat)PetscToPointer((F) ),*leaf_size);
}
PETSC_EXTERN void  matstrumpackgetcompleafsize_(Mat F,PetscInt *leaf_size, int *__ierr)
{
*__ierr = MatSTRUMPACKGetCompLeafSize(
	(Mat)PetscToPointer((F) ),leaf_size);
}
PETSC_EXTERN void  matstrumpacksetgeometricnxyz_(Mat F,PetscInt *nx,PetscInt *ny,PetscInt *nz, int *__ierr)
{
*__ierr = MatSTRUMPACKSetGeometricNxyz(
	(Mat)PetscToPointer((F) ),*nx,*ny,*nz);
}
PETSC_EXTERN void  matstrumpacksetgeometriccomponents_(Mat F,PetscInt *nc, int *__ierr)
{
*__ierr = MatSTRUMPACKSetGeometricComponents(
	(Mat)PetscToPointer((F) ),*nc);
}
PETSC_EXTERN void  matstrumpacksetgeometricwidth_(Mat F,PetscInt *w, int *__ierr)
{
*__ierr = MatSTRUMPACKSetGeometricWidth(
	(Mat)PetscToPointer((F) ),*w);
}
PETSC_EXTERN void  matstrumpacksetcompminsepsize_(Mat F,PetscInt *min_sep_size, int *__ierr)
{
*__ierr = MatSTRUMPACKSetCompMinSepSize(
	(Mat)PetscToPointer((F) ),*min_sep_size);
}
PETSC_EXTERN void  matstrumpackgetcompminsepsize_(Mat F,PetscInt *min_sep_size, int *__ierr)
{
*__ierr = MatSTRUMPACKGetCompMinSepSize(
	(Mat)PetscToPointer((F) ),min_sep_size);
}
PETSC_EXTERN void  matstrumpacksetcomplossyprecision_(Mat F,PetscInt *lossy_prec, int *__ierr)
{
*__ierr = MatSTRUMPACKSetCompLossyPrecision(
	(Mat)PetscToPointer((F) ),*lossy_prec);
}
PETSC_EXTERN void  matstrumpackgetcomplossyprecision_(Mat F,PetscInt *lossy_prec, int *__ierr)
{
*__ierr = MatSTRUMPACKGetCompLossyPrecision(
	(Mat)PetscToPointer((F) ),lossy_prec);
}
PETSC_EXTERN void  matstrumpacksetcompbutterflylevels_(Mat F,PetscInt *bfly_lvls, int *__ierr)
{
*__ierr = MatSTRUMPACKSetCompButterflyLevels(
	(Mat)PetscToPointer((F) ),*bfly_lvls);
}
PETSC_EXTERN void  matstrumpackgetcompbutterflylevels_(Mat F,PetscInt *bfly_lvls, int *__ierr)
{
*__ierr = MatSTRUMPACKGetCompButterflyLevels(
	(Mat)PetscToPointer((F) ),bfly_lvls);
}
#if defined(__cplusplus)
}
#endif
