!                   **********************
                    SUBROUTINE USER_CORFON
!                   **********************
!
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!
!history  Y AUDOUIN (LNHE)
!+        20/09/2018
!+        V8P0
!+
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE

      INTEGER I,K,IVAL2,NFILE,RECORD,IERR
      CHARACTER(LEN=PATH_LEN) :: NOMFILE
      DOUBLE PRECISION BID
      LOGICAL NOBFR
!
!-----------------------------------------------------------------------
!
! READING FRIC_ID FROM GEOMETRY FILE IF FRICTION DATA=YES
      RECORD = 0
      BID = 0.D0
      NOBFR = .FALSE.

!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
        CALL FIND_VARIABLE(T2D_FILES(T2DGEO)%FMT,T2D_FILES(T2DGEO)%LU,
     &                     'FRIC_ID         ',T1%R, MESH%NPOIN,IERR,
     &                     RECORD=RECORD,TIME_RECORD=BID)
        PRINT *, 'MODIFYING SECONDARY CHENNELS'
        DO I = 1,NPOIN
           IF (T1%R(I).EQ.3.D0) THEN
!              PRINT *, 'MODIFYING SECONDARY CHENNELS'
              ZF%R(I)=ZF%R(I)-3.D0
           ENDIF
        END DO       
        PRINT *, 'MODIFYING ENTRANCE'
        DO I = 1,NPOIN
           IF (Y(I).GE.3290497.75D0) THEN 
              IF (T1%R(I).EQ.1.D0) THEN
!              PRINT *, 'MODIFYING ENTRANCE'
                 ZF%R(I)=MAX(ZF%R(I)-19.D0,-30.D0)
              ELSEIF (T1%R(I).EQ.2.D0) THEN
!              PRINT *, 'MODIFYING ENTRANCE'
                 ZF%R(I)=MAX(ZF%R(I)-0.D0,-30.D0)
              ENDIF
           ELSEIF (Y(I).GE.3289096.5D0.AND.Y(I).LE.3290497.75D0) THEN
              IF (T1%R(I).EQ.1.D0) THEN
!              PRINT *, 'MODIFYING ENTRANCE'
                 ZF%R(I)=MAX(ZF%R(I)-19.D0,-30.D0)
              ELSEIF (T1%R(I).EQ.2.D0) THEN
!              PRINT *, 'MODIFYING ENTRANCE'
                 ZF%R(I)=MAX(ZF%R(I)+0.D0,-30.D0)
              ENDIF
           ENDIF
        END DO        

!
!-----------------------------------------------------------------------
!
      RETURN
      END
