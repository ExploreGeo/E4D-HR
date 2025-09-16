#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* logstate.c */
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

#include "petsclog.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogstatecreate_ PETSCLOGSTATECREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogstatecreate_ petsclogstatecreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogstatedestroy_ PETSCLOGSTATEDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogstatedestroy_ petsclogstatedestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogstatestagepush_ PETSCLOGSTATESTAGEPUSH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogstatestagepush_ petsclogstatestagepush
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogstatestagepop_ PETSCLOGSTATESTAGEPOP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogstatestagepop_ petsclogstatestagepop
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogstategetcurrentstage_ PETSCLOGSTATEGETCURRENTSTAGE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogstategetcurrentstage_ petsclogstategetcurrentstage
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogstateeventsetcollective_ PETSCLOGSTATEEVENTSETCOLLECTIVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogstateeventsetcollective_ petsclogstateeventsetcollective
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogstatestagesetactive_ PETSCLOGSTATESTAGESETACTIVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogstatestagesetactive_ petsclogstatestagesetactive
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogstatestagegetactive_ PETSCLOGSTATESTAGEGETACTIVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogstatestagegetactive_ petsclogstatestagegetactive
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogstateeventsetactive_ PETSCLOGSTATEEVENTSETACTIVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogstateeventsetactive_ petsclogstateeventsetactive
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogstateeventsetactiveall_ PETSCLOGSTATEEVENTSETACTIVEALL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogstateeventsetactiveall_ petsclogstateeventsetactiveall
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogstateclasssetactive_ PETSCLOGSTATECLASSSETACTIVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogstateclasssetactive_ petsclogstateclasssetactive
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogstateclasssetactiveall_ PETSCLOGSTATECLASSSETACTIVEALL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogstateclasssetactiveall_ petsclogstateclasssetactiveall
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogstateeventgetactive_ PETSCLOGSTATEEVENTGETACTIVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogstateeventgetactive_ petsclogstateeventgetactive
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogstategeteventfromname_ PETSCLOGSTATEGETEVENTFROMNAME
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogstategeteventfromname_ petsclogstategeteventfromname
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogstategetclassfromclassid_ PETSCLOGSTATEGETCLASSFROMCLASSID
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogstategetclassfromclassid_ petsclogstategetclassfromclassid
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogstategetnumevents_ PETSCLOGSTATEGETNUMEVENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogstategetnumevents_ petsclogstategetnumevents
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogstategetnumstages_ PETSCLOGSTATEGETNUMSTAGES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogstategetnumstages_ petsclogstategetnumstages
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogstategetnumclasses_ PETSCLOGSTATEGETNUMCLASSES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogstategetnumclasses_ petsclogstategetnumclasses
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogstateeventgetinfo_ PETSCLOGSTATEEVENTGETINFO
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogstateeventgetinfo_ petsclogstateeventgetinfo
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogstatestagegetinfo_ PETSCLOGSTATESTAGEGETINFO
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogstatestagegetinfo_ petsclogstatestagegetinfo
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogstateclassregister_ PETSCLOGSTATECLASSREGISTER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogstateclassregister_ petsclogstateclassregister
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogstateclassgetinfo_ PETSCLOGSTATECLASSGETINFO
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogstateclassgetinfo_ petsclogstateclassgetinfo
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petsclogstatecreate_(PetscLogState *state, int *__ierr)
{
*__ierr = PetscLogStateCreate(
	(PetscLogState* )PetscToPointer((state) ));
}
PETSC_EXTERN void  petsclogstatedestroy_(PetscLogState *state, int *__ierr)
{
*__ierr = PetscLogStateDestroy(
	(PetscLogState* )PetscToPointer((state) ));
}
PETSC_EXTERN void  petsclogstatestagepush_(PetscLogState *state,PetscLogStage *stage, int *__ierr)
{
*__ierr = PetscLogStateStagePush(*state,*stage);
}
PETSC_EXTERN void  petsclogstatestagepop_(PetscLogState *state, int *__ierr)
{
*__ierr = PetscLogStateStagePop(*state);
}
PETSC_EXTERN void  petsclogstategetcurrentstage_(PetscLogState *state,PetscLogStage *current, int *__ierr)
{
*__ierr = PetscLogStateGetCurrentStage(*state,current);
}
PETSC_EXTERN void  petsclogstateeventsetcollective_(PetscLogState *state,PetscLogEvent *event,PetscBool *collective, int *__ierr)
{
*__ierr = PetscLogStateEventSetCollective(*state,*event,*collective);
}
PETSC_EXTERN void  petsclogstatestagesetactive_(PetscLogState *state,PetscLogStage *stage,PetscBool *isActive, int *__ierr)
{
*__ierr = PetscLogStateStageSetActive(*state,*stage,*isActive);
}
PETSC_EXTERN void  petsclogstatestagegetactive_(PetscLogState *state,PetscLogStage *stage,PetscBool *isActive, int *__ierr)
{
*__ierr = PetscLogStateStageGetActive(*state,*stage,isActive);
}
PETSC_EXTERN void  petsclogstateeventsetactive_(PetscLogState *state,PetscLogStage *stage,PetscLogEvent *event,PetscBool *isActive, int *__ierr)
{
*__ierr = PetscLogStateEventSetActive(*state,*stage,*event,*isActive);
}
PETSC_EXTERN void  petsclogstateeventsetactiveall_(PetscLogState *state,PetscLogEvent *event,PetscBool *isActive, int *__ierr)
{
*__ierr = PetscLogStateEventSetActiveAll(*state,*event,*isActive);
}
PETSC_EXTERN void  petsclogstateclasssetactive_(PetscLogState *state,PetscLogStage *stage,PetscClassId *classid,PetscBool *isActive, int *__ierr)
{
*__ierr = PetscLogStateClassSetActive(*state,*stage,*classid,*isActive);
}
PETSC_EXTERN void  petsclogstateclasssetactiveall_(PetscLogState *state,PetscClassId *classid,PetscBool *isActive, int *__ierr)
{
*__ierr = PetscLogStateClassSetActiveAll(*state,*classid,*isActive);
}
PETSC_EXTERN void  petsclogstateeventgetactive_(PetscLogState *state,PetscLogStage *stage,PetscLogEvent *event,PetscBool *isActive, int *__ierr)
{
*__ierr = PetscLogStateEventGetActive(*state,*stage,*event,isActive);
}
PETSC_EXTERN void  petsclogstategeteventfromname_(PetscLogState *state, char name[],PetscLogEvent *event, int *__ierr, int cl0)
{
*__ierr = PetscLogStateGetEventFromName(*state,name,event);
}
PETSC_EXTERN void  petsclogstategetclassfromclassid_(PetscLogState *state,PetscClassId *classid,PetscLogClass *clss, int *__ierr)
{
*__ierr = PetscLogStateGetClassFromClassId(*state,*classid,
	(PetscLogClass* )PetscToPointer((clss) ));
}
PETSC_EXTERN void  petsclogstategetnumevents_(PetscLogState *state,PetscInt *numEvents, int *__ierr)
{
*__ierr = PetscLogStateGetNumEvents(*state,numEvents);
}
PETSC_EXTERN void  petsclogstategetnumstages_(PetscLogState *state,PetscInt *numStages, int *__ierr)
{
*__ierr = PetscLogStateGetNumStages(*state,numStages);
}
PETSC_EXTERN void  petsclogstategetnumclasses_(PetscLogState *state,PetscInt *numClasses, int *__ierr)
{
*__ierr = PetscLogStateGetNumClasses(*state,numClasses);
}
PETSC_EXTERN void  petsclogstateeventgetinfo_(PetscLogState *state,PetscLogEvent *event,PetscLogEventInfo *info, int *__ierr)
{
*__ierr = PetscLogStateEventGetInfo(*state,*event,
	(PetscLogEventInfo* )PetscToPointer((info) ));
}
PETSC_EXTERN void  petsclogstatestagegetinfo_(PetscLogState *state,PetscLogStage *stage,PetscLogStageInfo *info, int *__ierr)
{
*__ierr = PetscLogStateStageGetInfo(*state,*stage,
	(PetscLogStageInfo* )PetscToPointer((info) ));
}
PETSC_EXTERN void  petsclogstateclassregister_(PetscLogState *state, char name[],PetscClassId *id,PetscLogClass *logclass, int *__ierr, int cl0)
{
*__ierr = PetscLogStateClassRegister(*state,name,*id,
	(PetscLogClass* )PetscToPointer((logclass) ));
}
PETSC_EXTERN void  petsclogstateclassgetinfo_(PetscLogState *state,PetscLogClass *clss,PetscLogClassInfo *info, int *__ierr)
{
*__ierr = PetscLogStateClassGetInfo(*state,*clss,
	(PetscLogClassInfo* )PetscToPointer((info) ));
}
#if defined(__cplusplus)
}
#endif
