!                 ***********************************
                  SUBROUTINE USER_SUSPENSION_CAE_GAIA
!                 ***********************************
!
     &  (DCLA,NPOIN,XMVS,CSTAEQ)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief    Computation of Cae by user
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     DCLA   Sediment grain diameter
!>@param[in]     NPOIN  Number of points
!>@param[in]     XMVS   Sediment density
!>@param[in,out] CSTAEQ Equilibrium sediment concentration
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_GAIA, ONLY:XWC,HN,TAUP,GRAV,XMVE,UNORM
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)       :: NPOIN
      DOUBLE PRECISION, INTENT(IN)       :: DCLA, XMVS
      TYPE(BIEF_OBJ),   INTENT(INOUT)    :: CSTAEQ
!
!      TYPE(BIEF_OBJ),   INTENT(IN)    ::  TAUP
!      DOUBLE PRECISION, INTENT(IN)    ::  GRAV,  XMVE,XWC
!      TYPE(BIEF_OBJ),   INTENT(IN)       :: HN,U2D,V2D

!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                            :: IPOIN
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!======================================================================!
!
      IF (DCLA.LE.6.D-5) THEN
!         PRINT *,'UD DCLA' ,DCLA, XWC(1)
      DO IPOIN=1,NPOIN
!         PRINT *,UNORM%R(IPOIN)-SQRT(U2D%R(IPOIN)**2+V2D%R(IPOIN)**2)
         !USTARTOT=SQRT(TAUP%R(IPOIN)/XMVE)
         CSTAEQ%R(IPOIN) = 7.04D-4*XMVS*
     &           (SQRT(TAUP%R(IPOIN)/XMVE)/XWC(1))**1.71D0
     &          *(UNORM%R(IPOIN)/SQRT(GRAV*HN%R(IPOIN)))**1.81D0
!      CSTAEQ%R(I) = XMVS*CSTAEQ%R(I)
      ENDDO
      ELSE
!         PRINT *,'SAND DCLA', DCLA, XWC(2)
      DO IPOIN=1,NPOIN
!         PRINT *,SQRT(TOBCW_MEAN%R(IPOIN)/XMVE)-SQRT(TAUP%R(IPOIN)/XMVE)
         CSTAEQ%R(IPOIN) = 7.04D-4*XMVS*
     &           (SQRT(TAUP%R(IPOIN)/XMVE)/XWC(2))**1.71D0
     &          *(UNORM%R(IPOIN)/SQRT(9.81D0*HN%R(IPOIN)))**1.81D0
      ENDDO
      ENDIF
!
!      DO IPOIN=1,NPOIN
!         CSTAEQ%R(IPOIN) = 0.D0
!      ENDDO
!
!
!  FOLLOWING LINES NEED TO BE COMMENTED OUT
!
c$$$      WRITE(LU,*) 'WITH SUSPENSION TRANSPORT FORMULA FOR ALL SANDS = 0
c$$$     &               SUSPENSION_CAE_USER_GAIA HAVE TO BE PROGRAMMED
c$$$     &               BY USER'
c$$$      CALL PLANTE(1)
c$$$      STOP
!
!======================================================================!
!======================================================================!
      RETURN
      END SUBROUTINE USER_SUSPENSION_CAE_GAIA
