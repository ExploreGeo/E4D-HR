#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* loghandler.c */
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

#include "petscsys.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscloghandlercreate_ PETSCLOGHANDLERCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscloghandlercreate_ petscloghandlercreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscloghandlerdestroy_ PETSCLOGHANDLERDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscloghandlerdestroy_ petscloghandlerdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscloghandlersetstate_ PETSCLOGHANDLERSETSTATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscloghandlersetstate_ petscloghandlersetstate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscloghandlergetstate_ PETSCLOGHANDLERGETSTATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscloghandlergetstate_ petscloghandlergetstate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscloghandlereventbegin_ PETSCLOGHANDLEREVENTBEGIN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscloghandlereventbegin_ petscloghandlereventbegin
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscloghandlereventend_ PETSCLOGHANDLEREVENTEND
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscloghandlereventend_ petscloghandlereventend
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscloghandlereventsync_ PETSCLOGHANDLEREVENTSYNC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscloghandlereventsync_ petscloghandlereventsync
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscloghandlerobjectcreate_ PETSCLOGHANDLEROBJECTCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscloghandlerobjectcreate_ petscloghandlerobjectcreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscloghandlerobjectdestroy_ PETSCLOGHANDLEROBJECTDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscloghandlerobjectdestroy_ petscloghandlerobjectdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscloghandlerstagepush_ PETSCLOGHANDLERSTAGEPUSH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscloghandlerstagepush_ petscloghandlerstagepush
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscloghandlerstagepop_ PETSCLOGHANDLERSTAGEPOP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscloghandlerstagepop_ petscloghandlerstagepop
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscloghandlerview_ PETSCLOGHANDLERVIEW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscloghandlerview_ petscloghandlerview
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscloghandlersetlogactions_ PETSCLOGHANDLERSETLOGACTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscloghandlersetlogactions_ petscloghandlersetlogactions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscloghandlersetlogobjects_ PETSCLOGHANDLERSETLOGOBJECTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscloghandlersetlogobjects_ petscloghandlersetlogobjects
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscloghandlergetnumobjects_ PETSCLOGHANDLERGETNUMOBJECTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscloghandlergetnumobjects_ petscloghandlergetnumobjects
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscloghandlereventdeactivatepush_ PETSCLOGHANDLEREVENTDEACTIVATEPUSH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscloghandlereventdeactivatepush_ petscloghandlereventdeactivatepush
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscloghandlereventdeactivatepop_ PETSCLOGHANDLEREVENTDEACTIVATEPOP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscloghandlereventdeactivatepop_ petscloghandlereventdeactivatepop
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscloghandlereventspause_ PETSCLOGHANDLEREVENTSPAUSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscloghandlereventspause_ petscloghandlereventspause
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscloghandlereventsresume_ PETSCLOGHANDLEREVENTSRESUME
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscloghandlereventsresume_ petscloghandlereventsresume
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscloghandlerdump_ PETSCLOGHANDLERDUMP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscloghandlerdump_ petscloghandlerdump
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscloghandlerstagesetvisible_ PETSCLOGHANDLERSTAGESETVISIBLE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscloghandlerstagesetvisible_ petscloghandlerstagesetvisible
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscloghandlerstagegetvisible_ PETSCLOGHANDLERSTAGEGETVISIBLE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscloghandlerstagegetvisible_ petscloghandlerstagegetvisible
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscloghandlercreate_(MPI_Fint * comm,PetscLogHandler *handler, int *__ierr)
{
*__ierr = PetscLogHandlerCreate(
	MPI_Comm_f2c(*(comm)),
	(PetscLogHandler* )PetscToPointer((handler) ));
}
PETSC_EXTERN void  petscloghandlerdestroy_(PetscLogHandler *handler, int *__ierr)
{
*__ierr = PetscLogHandlerDestroy(
	(PetscLogHandler* )PetscToPointer((handler) ));
}
PETSC_EXTERN void  petscloghandlersetstate_(PetscLogHandler *h,PetscLogState *state, int *__ierr)
{
*__ierr = PetscLogHandlerSetState(*h,*state);
}
PETSC_EXTERN void  petscloghandlergetstate_(PetscLogHandler *h,PetscLogState *state, int *__ierr)
{
*__ierr = PetscLogHandlerGetState(*h,
	(PetscLogState* )PetscToPointer((state) ));
}
PETSC_EXTERN void  petscloghandlereventbegin_(PetscLogHandler *h,PetscLogEvent *e,PetscObject *o1,PetscObject *o2,PetscObject *o3,PetscObject *o4, int *__ierr)
{
*__ierr = PetscLogHandlerEventBegin(*h,*e,*o1,*o2,*o3,*o4);
}
PETSC_EXTERN void  petscloghandlereventend_(PetscLogHandler *h,PetscLogEvent *e,PetscObject *o1,PetscObject *o2,PetscObject *o3,PetscObject *o4, int *__ierr)
{
*__ierr = PetscLogHandlerEventEnd(*h,*e,*o1,*o2,*o3,*o4);
}
PETSC_EXTERN void  petscloghandlereventsync_(PetscLogHandler *h,PetscLogEvent *e,MPI_Fint * comm, int *__ierr)
{
*__ierr = PetscLogHandlerEventSync(*h,*e,
	MPI_Comm_f2c(*(comm)));
}
PETSC_EXTERN void  petscloghandlerobjectcreate_(PetscLogHandler *h,PetscObject *obj, int *__ierr)
{
*__ierr = PetscLogHandlerObjectCreate(*h,*obj);
}
PETSC_EXTERN void  petscloghandlerobjectdestroy_(PetscLogHandler *h,PetscObject *obj, int *__ierr)
{
*__ierr = PetscLogHandlerObjectDestroy(*h,*obj);
}
PETSC_EXTERN void  petscloghandlerstagepush_(PetscLogHandler *h,PetscLogStage *stage, int *__ierr)
{
*__ierr = PetscLogHandlerStagePush(*h,*stage);
}
PETSC_EXTERN void  petscloghandlerstagepop_(PetscLogHandler *h,PetscLogStage *stage, int *__ierr)
{
*__ierr = PetscLogHandlerStagePop(*h,*stage);
}
PETSC_EXTERN void  petscloghandlerview_(PetscLogHandler *h,PetscViewer viewer, int *__ierr)
{
*__ierr = PetscLogHandlerView(*h,
	(PetscViewer)PetscToPointer((viewer) ));
}
PETSC_EXTERN void  petscloghandlersetlogactions_(PetscLogHandler *handler,PetscBool *flag, int *__ierr)
{
*__ierr = PetscLogHandlerSetLogActions(*handler,*flag);
}
PETSC_EXTERN void  petscloghandlersetlogobjects_(PetscLogHandler *handler,PetscBool *flag, int *__ierr)
{
*__ierr = PetscLogHandlerSetLogObjects(*handler,*flag);
}
PETSC_EXTERN void  petscloghandlergetnumobjects_(PetscLogHandler *handler,PetscInt *num_objects, int *__ierr)
{
*__ierr = PetscLogHandlerGetNumObjects(*handler,num_objects);
}
PETSC_EXTERN void  petscloghandlereventdeactivatepush_(PetscLogHandler *handler,PetscLogStage *stage,PetscLogEvent *event, int *__ierr)
{
*__ierr = PetscLogHandlerEventDeactivatePush(*handler,*stage,*event);
}
PETSC_EXTERN void  petscloghandlereventdeactivatepop_(PetscLogHandler *handler,PetscLogStage *stage,PetscLogEvent *event, int *__ierr)
{
*__ierr = PetscLogHandlerEventDeactivatePop(*handler,*stage,*event);
}
PETSC_EXTERN void  petscloghandlereventspause_(PetscLogHandler *handler, int *__ierr)
{
*__ierr = PetscLogHandlerEventsPause(*handler);
}
PETSC_EXTERN void  petscloghandlereventsresume_(PetscLogHandler *handler, int *__ierr)
{
*__ierr = PetscLogHandlerEventsResume(*handler);
}
PETSC_EXTERN void  petscloghandlerdump_(PetscLogHandler *handler, char sname[], int *__ierr, int cl0)
{
*__ierr = PetscLogHandlerDump(*handler,sname);
}
PETSC_EXTERN void  petscloghandlerstagesetvisible_(PetscLogHandler *handler,PetscLogStage *stage,PetscBool *isVisible, int *__ierr)
{
*__ierr = PetscLogHandlerStageSetVisible(*handler,*stage,*isVisible);
}
PETSC_EXTERN void  petscloghandlerstagegetvisible_(PetscLogHandler *handler,PetscLogStage *stage,PetscBool *isVisible, int *__ierr)
{
*__ierr = PetscLogHandlerStageGetVisible(*handler,*stage,isVisible);
}
#if defined(__cplusplus)
}
#endif
