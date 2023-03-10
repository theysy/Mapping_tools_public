C-----------------------------------------------------------------------
      SUBROUTINE STAND_ALONE(ANG,BC,SN,PROPS,STATEV,IOPT)
C     # THIS SUBROUTINE LAUNCHES A STAND-ALONE SIMULATION FOR GIVEN PARAMETERS.
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION SN(NTENS), PROPS(NPROPS), STATEV(NSTATV)
      DIMENSION DSTRAN(NDATA,NTENS), DFDS(NTENS), STRESS(NTENS)
      DIMENSION CE(NTENS,NTENS), CEP(NTENS,NTENS)
      COMMON /KUMAT/ NPROPS, NSTATV, NTENS, NDATA

C     [1] GENERATE STRAIN INRCREMENT
      CALL STRAIN_INC(BC,PROPS,SN,DSTRAN,IOPT)
C     [4] REFERENCE STRESS STATE
      CALL ROTMAT(ANG,SN,1.D0)
      CALL UMAT(PROPS,STATEV,SN,DSTRAN(1,:),CE)
      DFDS=STATEV(6:5+NTENS)
C     [5] RUN SIMULATION
      STRESS=0.D0
      STATEV=0.D0
      DO N=1,NDATA
          IF(STATEV(1).GT.0.D0) THEN
            CALL CORRECTION(ANG,DFDS,SN,STRESS,DSTRAN(N,:))
          END IF
          CALL UMAT(PROPS,STATEV,STRESS,DSTRAN(N,:),CEP)
      END DO
      RETURN
      END SUBROUTINE STAND_ALONE
C-----------------------------------------------------------------------
      SUBROUTINE STRAIN_INC(BC,PROPS,SN,DSTRAN,IOPT)
C     # THIS SUBROUTINE DEFINES THE STRAIN INCREMENT BASED ON
C       THE PRESCIRBED (1) BOUNDARY CONDITION AND (2) TIME INCREMENT.
C     # IOPT=1: BC=DISPLACEMENT
C           =2: BC=TRUE STRAIN
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION SN(NTENS), PROPS(NPROPS),DSTRAN(NDATA,NTENS)
      DIMENSION CE(NTENS,NTENS), SE(NTENS,NTENS)
      DIMENSION STRAN0(NDATA),STRAN(NDATA,NTENS)
      DIMENSION AUX(NTENS)
      COMMON /KUMAT/ NPROPS, NSTATV, NTENS, NDATA
C     [1]   TOTAL STRAIN
      CALL LINSPACE(0.D0,BC,SIZE(DSTRAN,1),STRAN0)
      IF(IOPT .EQ. 1) THEN
            STRAN(:,1)=LOG(1+STRAN0)
      ELSEIF(IOPT .EQ. 2) THEN
            STRAN(:,1)=STRAN0
      ELSE
            WRITE(*,*) '#ERROR: SUB. STRAIN_INC'
      END IF
C     [2]   ELASTIC STIFFNESS TENSOR
      CALL ELASTIC_STIFFNESS(PROPS, CE)
      CALL INVERSE(CE, NTENS, SE)
C     [3]   3D STRAIN TENSOR
      AUX=MATMUL(SE,SN)
      AUX=AUX/MAXVAL(DABS(AUX))
      DO I=1,NTENS
            STRAN(:,I)=AUX(I)*STRAN0
      END DO
C     [4]   STRAIN INCREMENT
      DSTRAN=0.D0
      DO I=2,NDATA
            DSTRAN(I,:)=STRAN(I,:)-STRAN(I-1,:)
      END DO
C
      RETURN
      END SUBROUTINE STRAIN_INC
C-----------------------------------------------------------------------
      SUBROUTINE PMAP(PROPS, STATEV, BC, IOPT)
C     # THIS SUBROUTINE GENERATES DATA FOR THE PRECISION MAP.
C     VALUE=SQRT(DFDS:DFDS)
      IMPLICIT REAL*8(A-H, O-Z)
      DIMENSION PROPS(NPROPS), STATEV(NSTATV)
      DIMENSION DFDS0(NTENS), DFDS1(NTENS), SN(NTENS), DUMMY1(NTENS)
      DIMENSION STATEV0(NSTATV), SIG0(NTENS), CEP(NTENS,NTENS),
     1          DSTRAN(NDATA,NTENS), DUMMY12(NTENS)
      DIMENSION XMAP_DATA(50), STAT_VAR(NDIM6), PSIG(2), YSIG(2)
C      DIMENSION STATEV1(NDATA,NSTATV), SIG1(NDATA,NTENS)
      CHARACTER :: FILEPATH*128, DEBUGPATH*128
      PARAMETER(PI=DACOS(-1.D0), RADIAN=DATAN(1.D0)/45.D0)
      COMMON /KUMAT/ NPROPS, NSTATV, NTENS, NDATA
      COMMON /KSIZE/ NDIM1, NDIM2, NDIM3, NDIM4, NDIM5, NDIM6, NDIM7

      CALL CPU_TIME(START)
C     [1]   RESULT FILE NAME
      FILEPATH='OUT\PMAP.CSV'
      DEBUGPATH='DEBUG\DEBUG.CSV'
C     [2]   DATA SIZE
      IRANGE1= 360; IRANGE2=45;
      XRANGE1= 360.D0; XRANGE2=90.D0;
      NSIZE=IRANGE1*NRANGE2
      IF(NTENS .EQ. 3) THEN
            NDI=2; NTENS2=NTENS+1
      ELSEIF(NTENS .EQ. 6) THEN
            NDI=3; NTENS2=NTENS
      END IF
C     [3]   INITIALIZATION
      INDX=0; NDIM0=6
      XMAP_DATA=0.D0
C     [4]   READ STATE VARIABLE
      CALL READ_STATEV(STATEV, STAT_VAR, SIG0) !STATEV -> STAT_VAR
C-----------------------------------------------------------------------
C     [3]   GENERATE DATA
      DO I=0, IRANGE1
      DO J=0, IRANGE2
C     [3.1] DEGREE -> RADIAN
            ANG1=FLOAT(I)*RADIAN
            ANG2=FLOAT(J)
C     [3.2] REFERENCE STRESS STATE
            SN=0.D0
            SN(1)= DCOS(ANG1)
            SN(2)= DSIN(ANG1)
            CALL ROTMAT(ANG2,SN,1.D0)
C     [3.3] REFERENCE GRADIENT
            DUMMY1=0.D0
            STATEV0=STATEV
            CALL UMAT(PROPS,STATEV0,SN,DUMMY1,CEP)
            DFDS0=STATEV0(NDIM0:NDIM0+NTENS-1)
C     [3.3] REFERENCE PRECISION VALUE
            CALL PMAP_VAL(DFDS0, XVAL0)
C     [3.3] STRAIN INCREMENT
            CALL STRAIN_INC(BC,PROPS,SN,DSTRAN,IOPT)
C     [3.3] RUN SIMULATION
            SIG0=0.D0
            STATEV0=STATEV
            DO N=1,NDATA
               IF(STATEV0(5).GT.0.D0) THEN
                  CALL CORRECTION(ANG2,DFDS0,SN,SIG0,DSTRAN(N,:))
               END IF
               CALL UMAT(PROPS,STATEV0,SIG0,DSTRAN(N,:),CEP)
            END DO
C     [3.4] GRADIENT OF YIELD SURFACE
            DFDS1=STATEV0(NDIM0:NDIM0+NTENS-1)
            CALL PMAP_VAL(DFDS1, XVAL1)
            XERROR=DABS(XVAL1-XVAL0)/XVAL0*100
C     [3.5] STRESS COORDINATES
            CALL EFF_SIG(STAT_VAR, SN, SIG_BAR)
            CALL PI_PLANE(SN/SIG_BAR,PSIG)
            YSIG=SN(1:2)/SIG_BAR
C     [3.5] WRITE DATA
            XMAP_DATA(1)= FLOAT(I)
            XMAP_DATA(2)= FLOAT(J)
            XMAP_DATA(3)= PSIG(1)
            XMAP_DATA(4)= PSIG(2)
            XMAP_DATA(5)= YSIG(1)
            XMAP_DATA(6)= YSIG(2)
            XMAP_DATA(7)= XVAL0
            XMAP_DATA(8)= XVAL1
            XMAP_DATA(9)= XERROR
            CALL WRITE_CSV(1,9,XMAP_DATA,FILEPATH, 1, 2)
      END DO
C     [3.6] VISUALIZE PROGRESS
            PERC=FLOAT(I)/FLOAT(IRANGE1)
            CALL PROGRESS(PERC,5)
      END DO
C-----------------------------------------------------------------------
C     [4]   CLOSE PMAP DATA
      XMAP_DATA=0.D0
      CALL WRITE_CSV(1,9,XMAP_DATA,FILEPATH, 1, 3)
C-----------------------------------------------------------------------
      CALL CPU_TIME(FINISH)
      WRITE(*,'(A)') '--------------------------------------'
      WRITE(*,'(2X,A)') 'PMAP COMPLETED!!'
      WRITE(*,'(2X,A)',ADVANCE='NO') 'ELAPSED TIME:'
      WRITE(*,'(2X,F10.4,2X,A)') FINISH-START, 'SECONDS'
      WRITE(*,'(A)') '--------------------------------------'
C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE PMAP
C-----------------------------------------------------------------------
      SUBROUTINE CMAP(PROPS, STATEV, BC, IOPT)
C     # THIS SUBROUTINE GENERATES DATA FOR THE PRECISION MAP.
C     VALUE=SQRT(DFDS:DFDS)
      IMPLICIT REAL*8(A-H, O-Z)
      DIMENSION PROPS(NPROPS), STATEV(NSTATV)
      DIMENSION DFDS0(NTENS), DFDS1(NTENS), SN(NTENS), DUMMY1(NTENS)
      DIMENSION STATEV0(NSTATV), SIG0(NTENS), CEP(NTENS,NTENS),
     1          DSTRAN(NDATA,NTENS), DUMMY12(NTENS)
      DIMENSION XMAP_DATA(50), STAT_VAR(NDIM6), PSIG(2), YSIG(2)
C      DIMENSION STATEV1(NDATA,NSTATV), SIG1(NDATA,NTENS)
      CHARACTER :: FILEPATH*128, DEBUGPATH*128
      PARAMETER(PI=DACOS(-1.D0), RADIAN=DATAN(1.D0)/45.D0)
      COMMON /KUMAT/ NPROPS, NSTATV, NTENS, NDATA
      COMMON /KSIZE/ NDIM1, NDIM2, NDIM3, NDIM4, NDIM5, NDIM6, NDIM7

      CALL CPU_TIME(START)
C     [1]   RESULT FILE NAME
      FILEPATH='OUT\CMAP.CSV'
      DEBUGPATH='DEBUG\DEBUG.CSV'
C     [2]   DATA SIZE
      IRANGE1= 360; IRANGE2=45;
      XRANGE1= 360.D0; XRANGE2=90.D0;
      NSIZE=IRANGE1*NRANGE2
      IF(NTENS .EQ. 3) THEN
            NDI=2; NTENS2=NTENS+1
      ELSEIF(NTENS .EQ. 6) THEN
            NDI=3; NTENS2=NTENS
      END IF
C     [3]   INITIALIZATION
      INDX=0; NDIM0=6
      XMAP_DATA=0.D0
C     [4]   READ STATE VARIABLE
      SIG0=0.D0
      CALL READ_STATEV(STATEV, STAT_VAR, SIG0) !STATEV -> STAT_VAR
C-----------------------------------------------------------------------
C     [3]   GENERATE DATA
      DO I=0, IRANGE1
      DO J=0, IRANGE2
C     [3.1] DEGREE -> RADIAN
            ANG1=FLOAT(I)*RADIAN
            ANG2=FLOAT(J)
C     [3.2] REFERENCE STRESS STATE
            SN=0.D0
            SN(1)= DCOS(ANG1)
            SN(2)= DSIN(ANG1)
            CALL ROTMAT(ANG2,SN,1.D0)
C     [3.3] REFERENCE GRADIENT
            DUMMY1=0.D0
            STATEV0=STATEV
            CALL UMAT(PROPS,STATEV0,SN,DUMMY1,CEP)
            DFDS0=STATEV0(NDIM0:NDIM0+NTENS-1)
C     [3.4] STRAIN INCREMENT
            CALL STRAIN_INC(BC,PROPS,SN,DSTRAN,IOPT)
C     [3.5] RUN SIMULATION
            SIG0=0.D0
            STATEV0=STATEV
            DO N=1,NDATA
               IF(STATEV0(5).GT.0.D0) THEN
                  CALL CORRECTION(ANG2,DFDS0,SN,SIG0,DSTRAN(N,:))
               END IF
               CALL UMAT(PROPS,STATEV0,SIG0,DSTRAN(N,:),CEP)
            END DO
C     [3.6] STRESS COORDINATES
            CALL EFF_SIG(STAT_VAR, SN, SIG_BAR)
            CALL PI_PLANE(SN/SIG_BAR,PSIG)
            YSIG=SN(1:2)/SIG_BAR
C     [3.7] WRITE DATA
            XMAP_DATA(1)= FLOAT(I)
            XMAP_DATA(2)= FLOAT(J)
            XMAP_DATA(3)= PSIG(1)
            XMAP_DATA(4)= PSIG(2)
            XMAP_DATA(5)= YSIG(1)
            XMAP_DATA(6)= YSIG(2)
            XMAP_DATA(7)= STATEV0(5) ! ALGORITHMIC ITERATIONS
            CALL WRITE_CSV(1,10,XMAP_DATA,FILEPATH, 1, 2)
      END DO
C     [3.6] VISUALIZE PROGRESS
            PERC=FLOAT(I)/FLOAT(IRANGE1)
            CALL PROGRESS(PERC,5)
      END DO
C-----------------------------------------------------------------------
C     [4]   CLOSE PMAP DATA
      XMAP_DATA=0.D0
      CALL WRITE_CSV(1,10,XMAP_DATA,FILEPATH, 1, 3)
C-----------------------------------------------------------------------
      CALL CPU_TIME(FINISH)
      WRITE(*,'(A)') '--------------------------------------'
      WRITE(*,'(2X,A)') 'CMAP COMPLETED!!'
      WRITE(*,'(2X,A)',ADVANCE='NO') 'ELAPSED TIME:'
      WRITE(*,'(2X,F10.4,2X,A)') FINISH-START, 'SECONDS'
      WRITE(*,'(A)') '--------------------------------------'
C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE CMAP
C-----------------------------------------------------------------------
      SUBROUTINE DMAP(PROPS, STATEV, BC, IOPT)
C     # THIS SUBROUTINE GENERATES DATA FOR THE PRECISION MAP.
C     VALUE=SQRT(DFDS:DFDS)
      IMPLICIT REAL*8(A-H, O-Z)
      DIMENSION PROPS(NPROPS), STATEV(NSTATV)
      DIMENSION DFDS0(NTENS), DFDS1(NTENS), SN(NTENS), DUMMY1(NTENS)
      DIMENSION STATEV0(NSTATV), SIG0(NTENS), CEP(NTENS,NTENS),
     1          DSTRAN(NDATA,NTENS), DUMMY12(NTENS)
      DIMENSION XMAP_DATA(50), STAT_VAR(NDIM6), PSIG(2), YSIG(2)
      DIMENSION STATEV1(NDATA,NSTATV), SIG1(NDATA,NTENS)
      CHARACTER :: FILEPATH*128, DEBUGPATH*128
      PARAMETER(PI=DACOS(-1.D0), RADIAN=DATAN(1.D0)/45.D0)
      COMMON /KUMAT/ NPROPS, NSTATV, NTENS, NDATA
      COMMON /KSIZE/ NDIM1, NDIM2, NDIM3, NDIM4, NDIM5, NDIM6, NDIM7
C-----------------------------------------------------------------------
      CALL CPU_TIME(START)
C     [1]   RESULT FILE NAME
      FILEPATH='OUT\DMAP.CSV'
      DEBUGPATH='DEBUG\DEBUG.CSV'
C     [2]   DATA SIZE
      IRANGE1= 360; IRANGE2=45;
      XRANGE1= 360.D0; XRANGE2=90.D0;
      NSIZE=IRANGE1*NRANGE2
      IF(NTENS .EQ. 3) THEN
            NDI=2; NTENS2=NTENS+1
      ELSEIF(NTENS .EQ. 6) THEN
            NDI=3; NTENS2=NTENS
      END IF
C     [3]   INITIALIZATION
      INDX=0; NDIM0=6
      XDATA=0.D0
C     [4]   READ STATE VARIABLE
      SIG0=0.D0
      CALL READ_STATEV(STATEV, STAT_VAR, SIG0) !STATEV -> STAT_VAR
C-----------------------------------------------------------------------
C     [3]   GENERATE DATA
      DO I=0, IRANGE1
      DO J=0, IRANGE2
C     [3.1] DEGREE -> RADIAN
            ANG1=FLOAT(I)*RADIAN
            ANG2=FLOAT(J)
C     [3.2] REFERENCE STRESS STATE
            SN=0.D0
            SN(1)= DCOS(ANG1)
            SN(2)= DSIN(ANG1)
            CALL ROTMAT(ANG2,SN,1.D0)
C     [3.3] REFERENCE GRADIENT
            DUMMY1=0.D0
            STATEV0=STATEV
            CALL UMAT(PROPS,STATEV0,SN,DUMMY1,CEP)
            DFDS0=STATEV0(NDIM0:NDIM0+NTENS-1)
C     [3.4] STRAIN INCREMENT
            CALL STRAIN_INC(BC,PROPS,SN,DSTRAN,IOPT)
C     [3.5] RUN SIMULATION
            SIG0=0.D0
            STATEV0=STATEV
            DO N=1,NDATA
               IF(STATEV0(1).GT.0.D0) THEN
                  CALL CORRECTION(ANG2,DFDS0,SN,SIG0,DSTRAN(N,:))
               END IF
               CALL UMAT(PROPS,STATEV0,SIG0,DSTRAN(N,:),CEP)
            END DO
C     [3.6] STRESS COORDINATES
            CALL EFF_SIG(STAT_VAR, SN, SIG_BAR)
            CALL PI_PLANE(SN/SIG_BAR,PSIG)
            YSIG=SN(1:2)/SIG_BAR
C     [3.7] WRITE DATA
            XMAP_DATA(1)= FLOAT(I)
            XMAP_DATA(2)= FLOAT(J)
            XMAP_DATA(3)= PSIG(1)
            XMAP_DATA(4)= PSIG(2)
            XMAP_DATA(5)= YSIG(1)
            XMAP_DATA(6)= YSIG(2)
            XMAP_DATA(7)= STATEV0(1) ! EFFECTIVE STRAIN INCREMENT
            XMAP_DATA(8)= STATEV0(2) ! EFFECTIVE PLASTIC STRAIN
            XMAP_DATA(9)= STATEV0(3) ! EFFECTIVE STRESS
            XMAP_DATA(10)=STATEV0(4) ! FLOW STRESS
            CALL WRITE_CSV(1,10,XMAP_DATA,FILEPATH, 2, 2)
      END DO
C     [3.8] VISUALIZE PROGRESS
            PERC=FLOAT(I)/FLOAT(IRANGE1)
            CALL PROGRESS(PERC,5)
      END DO
C-----------------------------------------------------------------------
C     [4]   CLOSE PMAP DATA
      XMAP_DATA=0.D0
      CALL WRITE_CSV(1,10,XMAP_DATA,FILEPATH, 2, 3)
C-----------------------------------------------------------------------
      CALL CPU_TIME(FINISH)
      WRITE(*,'(A)') '--------------------------------------'
      WRITE(*,'(2X,A)') 'DMAP COMPLETED!!'
      WRITE(*,'(2X,A)',ADVANCE='NO') 'ELAPSED TIME:'
      WRITE(*,'(2X,F10.4,2X,A)') FINISH-START, 'SECONDS'
      WRITE(*,'(A)') '--------------------------------------'
C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE DMAP
C-----------------------------------------------------------------------
      SUBROUTINE IMAP(PROPS, STATEV, BC, IOPT)
C     # THIS SUBROUTINE GENERATES THE ISO-ERROR MAP
      IMPLICIT REAL*8(A-H, O-Z)
      DIMENSION PROPS(NPROPS), STATEV(NSTATV)
      DIMENSION DFDS0(NTENS), DFDS1(NTENS), SN(NTENS), DUMMY1(NTENS)
      DIMENSION STATEV0(NSTATV), SIG0(NTENS), CEP(NTENS,NTENS),
     1          DSTRAN(NDATA,NTENS), DUMMY12(NTENS)
      DIMENSION XMAP_DATA(50), STAT_VAR(NDIM6), PSIG(2), YSIG(2)
      DIMENSION STATEV1(NDATA,NSTATV), SIG1(NDATA,NTENS)
      DIMENSION ANG(3), STRAN(3,NTENS), SIG2(3,NTENS), STATEV2(3,NTENS)
      DIMENSION DDSTRAN(NTENS), ASOL(NTENS), ESOL(NTENS), ERROR(3)
      DIMENSION ASOL1(NTENS), ASOL2(NTENS), XITER(3)
      REAL*8, DIMENSION(:), ALLOCATABLE :: FACTOR
      CHARACTER :: FILEPATH*128, DEBUGPATH*128
      PARAMETER(PI=DACOS(-1.D0), RADIAN=DATAN(1.D0)/45.D0)
      COMMON /KUMAT/ NPROPS, NSTATV, NTENS, NDATA
      COMMON /KSIZE/ NDIM1, NDIM2, NDIM3, NDIM4, NDIM5, NDIM6, NDIM7
C-----------------------------------------------------------------------
      CALL CPU_TIME(START)
C     [1]   RESULT FILE NAME
      FILEPATH='OUT\IMAP.CSV'
      DEBUGPATH='DEBUG\DEBUG.CSV'
C     [2]   DATA SIZE
      IRANGE=200+1; XINC=10.D0
      ALLOCATE(FACTOR(IRANGE))
      IF(NTENS .EQ. 3) THEN
            NDI=2; NTENS2=NTENS+1
      ELSEIF(NTENS .EQ. 6) THEN
            NDI=3; NTENS2=NTENS
      END IF
C     [3]   INITIALIZATION
      INDX=0; NDIM0=6; XDATA=0.D0
      ERROR=0.D0; STRAN=0.D0; STATEV2=0.D0
C     [4]   READ STATE VARIABLE
      SIG0=0.D0
      CALL READ_STATEV(STATEV, STAT_VAR, SIG0) !STATEV -> STAT_VAR
C-----------------------------------------------------------------------
C     [5]   REFERENCE DEFORMATION MODE
C     [5.1] UNIAXIAL TENSION
      ANG(1)=0.D0
C     [5.2] BALANCED BIAXIAL TENSION
      ANG(2)=45.D0
C     [5.3] PURE SHEAR
      ANG(3)=-45.D0
C-----------------------------------------------------------------------
C     [6]   STRAIN INCREMENT FACTOR
      CALL LINSPACE(0.D0,XINC,IRANGE,FACTOR)
C-----------------------------------------------------------------------
C     [7]   FIND YIELD STRAIN
      DO K=1, 3
C     [7.1] DEGREE -> RADIAN
            ANG1=ANG(K)*RADIAN
C     [7.2] REFERENCE STRESS STATE
            SN=0.D0
            SN(1)= DCOS(ANG1)
            SN(2)= DSIN(ANG1)
C     [7.3] REFERENCE GRADIENT
            DUMMY1=0.D0
            STATEV0=STATEV
            CALL UMAT(PROPS,STATEV0,SN,DUMMY1,CEP)
            DFDS0=STATEV0(NDIM0:NDIM0+NTENS-1)
C     [7.4] STRAIN INCREMENT
            CALL STRAIN_INC(BC,PROPS,SN,DSTRAN,IOPT)
C     [7.5] SIMULATION
            SIG0=0.D0; FLAG=0.D0;
            STATEV0=STATEV; STATEV1=0.D0; SIG1=0.D0;
            DO N=1,NDATA
               CALL UMAT(PROPS,STATEV0,SIG0,DSTRAN(N,:),CEP)
               STRAN(K,:)=STRAN(K,:)+DSTRAN(N,:)
               STATEV1(N,:)=STATEV0
               SIG1(N,:)=SIG0
               IF(STATEV0(5).GT.0.D0) THEN
                  SIG2(K,:)=SIG1(N,:)
                  STATEV2(K,:)=STATEV1(N,:)
                  EXIT
               END IF
            END DO
      END DO
C-----------------------------------------------------------------------
C     [8]   FIND APPROXIMATED AND EXACT SOLUTIONS
      DO I=1, IRANGE
      DO J=1, IRANGE
      DO K=1, 3
C     [8.1] APPROXIMATED SOLUTION
            DUMMY1=0.D0
            DUMMY1(1)=STRAN(K,1)*FACTOR(I)
            DUMMY1(2)=STRAN(K,2)*FACTOR(J)
            SIG0=SIG2(K,:)
            STATEV0=STATEV2(K,:)
            CALL UMAT(PROPS,STATEV0,SIG0,DUMMY1,CEP)
            ASOL=SIG0
C     [8.2] EXACT SOLUTION: SUB-INCREMENTATION
C            INCSUB=200
            TARGET_INC=1.D-1
            FACTOR_MAX=MAX(FACTOR(I),FACTOR(J))
            INCSUB=NINT(FACTOR_MAX/TARGET_INC)
            IF(INCSUB.LE.0) INCSUB=1
            DDSTRAN=0.D0
            SIG0=SIG2(K,:)
            STATEV0=STATEV2(K,:)
            DO L=1, INCSUB
                  DDSTRAN=DUMMY1/FLOAT(INCSUB)
                  CALL UMAT(PROPS,STATEV0,SIG0,DDSTRAN,CEP)
            END DO
            ESOL=SIG0
C     [8.3] ERROR CALCULATION
            CALL DOUBLE_DOT(ESOL-ASOL,ESOL-ASOL,NTENS,XVAL1)
            CALL DOUBLE_DOT(ESOL,ESOL,NTENS,XVAL2)
            ERROR(K)=SQRT(XVAL1)/SQRT(XVAL2)*100.D0
            XITER(K)=STATEV0(5)
      END DO ! K-LOOP
C     [8.4] WRITE DATA
            XMAP_DATA(1)=FACTOR(I)
            XMAP_DATA(2)=FACTOR(J)
            XMAP_DATA(3)=ERROR(1)
            XMAP_DATA(4)=ERROR(2)
            XMAP_DATA(5)=ERROR(3)
            XMAP_DATA(6)=XITER(1)
            XMAP_DATA(7)=XITER(2)
            XMAP_DATA(8)=XITER(3)
            CALL WRITE_CSV(1,8,XMAP_DATA,FILEPATH, 2, 2)
      END DO ! J-LOOP
C     [8.5] VISUALIZE PROGRESS
            PERC=REAL(I,8)/REAL(IRANGE-1,8)
            CALL PROGRESS(PERC,5)
      END DO
C-----------------------------------------------------------------------
C     [9]   CLOSE IMAP DATA
      XMAP_DATA=0.D0
      CALL WRITE_CSV(1,8,XMAP_DATA,FILEPATH, 2, 3)
C-----------------------------------------------------------------------
      CALL CPU_TIME(FINISH)
      WRITE(*,'(A)') '--------------------------------------'
      WRITE(*,'(2X,A)') 'IMAP COMPLETED!!'
      WRITE(*,'(2X,A)',ADVANCE='NO') 'ELAPSED TIME:'
      WRITE(*,'(2X,F10.4,2X,A)') FINISH-START, 'SECONDS'
      WRITE(*,'(A)') '--------------------------------------'
C-----------------------------------------------------------------------
      DEALLOCATE(FACTOR)
      RETURN
      END SUBROUTINE IMAP
C-----------------------------------------------------------------------
      SUBROUTINE PMAP_VAL(DFDS, XVAL)
C     # THIS SUBROUTINE CALCULATES THE SEVERITY OF MATREIAL ANISOTROPY.
      IMPLICIT REAL*8(A-H, O-Z)
      DIMENSION DFDS(NTENS), VAL(6)
      COMMON /KUMAT/ NPROPS, NSTATV, NTENS, NDATA

C     [1] INITIALIZATION
      VAL=0.D0
      IF(NTENS .EQ. 3) THEN
            VAL(1:2)=DFDS(1:2)
            VAL(3)=-(DFDS(1)+DFDS(2))
            VAL(4)=DFDS(3)/DSQRT(2.D0)
      ELSE
            VAL(1:3)=DFDS(1:3)
            VAL(4:6)=DFDS(4:6)/DSQRT(2.D0)
      END IF
C     [2] EVALUATION
      CALL DOUBLE_DOT(VAL,VAL,6,DOUBLE)
      XVAL=DSQRT(DOUBLE)
      IF(ISNAN(XVAL).OR. XVAL.EQ.0.D0) XVAL=-1.D0

      RETURN
      END SUBROUTINE PMAP_VAL
C-----------------------------------------------------------------------
      SUBROUTINE CORRECTION(ANG, DFDS, SN, SIG, DSTRAN)
C     # THIS SUBROUTINE CORRECTS STRESS AND STRAIN DATA.
      IMPLICIT REAL*8(A-H, O-Z)
      DIMENSION DFDS(NTENS), SN(NTENS)
      DIMENSION SIG(NTENS), DSTRAN(NTENS)
      DIMENSION DUMMY01(NTENS), DUMMY02(NTENS)
      DIMENSION DSTRAN0(NTENS), SIG0(NTENS)
      COMMON /KUMAT/ NPROPS, NSTATV, NTENS, NDATA

C     [1] LOCAL COORDINATE
      DUMMY01=DFDS; DUMMY02=SN; DSTRAN0=DSTRAN; SIG0=SIG
      CALL ROTMAT(-ANG,DUMMY01,2.D0)
      CALL ROTMAT(-ANG,DUMMY02,1.D0)
      CALL ROTMAT(-ANG,DSTRAN0,2.D0)
      CALL ROTMAT(-ANG,SIG0,1.D0)
C     [2] FIND REFERENCE STRESS STATE
      VAL=MAXVAL(DABS(DUMMY02))
      DO I=1,NTENS
            IF(DABS(DABS(DUMMY02(I))-VAL).EQ.0.D0) THEN
                  INDX=I
                  EXIT
            END IF
      END DO
      DUMMY01=DUMMY01/DABS(DUMMY01(INDX))
      DUMMY02=DUMMY02/DABS(DUMMY02(INDX))
C     [3] STRAIN INCREMENT CORRECTION
      DO I=1,NTENS
            DSTRAN0(I)=DABS(DSTRAN0(INDX))*DUMMY01(I)
      END DO
      CALL ROTMAT(ANG,DSTRAN0,2.D0)
      DSTRAN=DSTRAN0
C     [4] STRESS CORRECTION
      DO I=1, NTENS
            SIG0(I)=DABS(SIG0(I))*DUMMY02(I)
      END DO
      CALL ROTMAT(ANG,SIG0,1.D0)
      SIG=SIG0

      RETURN
      END SUBROUTINE CORRECTION
C-----------------------------------------------------------------------
      SUBROUTINE INIT_STRESS(BC,HARD_VAR,SIG,STRAN0,EQPLAS,DDSDDE)
C     # THIS SUBROUTINE OBTAINS THE INITIAL STRESS TENSOR
C       RIGHT BEFORE PLASTIC DEFORMATION STARTS
C     # STRAIN INCREMENT IS CALULATED FROM A DEFORMED MATERIAL
      IMPLICIT REAL*8(A-H, O-Z)
      DIMENSION HARD_VAR(NDIM7), SIG(NDIM3), STRAN0(NDIM3),
     1 DDSDDE(NDIM3,NDIM3), DDEDDS(NDIM3,NDIM3)
      DIMENSION EIG_VAL(3), EIG_VEC(3,3), DSTRESS(NDIM3),
     1 DSTRAN(NDIM3), STRAN1(NDIM3)
      DIMENSION DISP(NDIM3)

C      DIMENSION STRESS0(NDIM3),ELAS(NDIM3),PLAS(NDIM3),STRAN(NDIM3)
      COMMON /KUMAT/ NPROPS, NSTATV, NTENS, NDATA
      COMMON /KOPTION/ HARD_PAR, YLD_PAR, FLOW_PAR, SUA_PAR, GRAD_PAR
      COMMON /KSIZE/ NDIM1, NDIM2, NDIM3, NDIM4, NDIM5, NDIM6, NDIM7
      COMMON /KCAMP2/ INDX
      PARAMETER(TOL=1.D-16)
      CALL INVERSE(DDSDDE,NTENS,DDEDDS)
      DSTRAN=MATMUL(DDEDDS,SIG)
C     [0] DETERMINE DISPLACEMENT
      DT=1.D0/NDATA
C      SIGN=1.D0
      INDX3=NDIM1+1
      VAL=SIG(1)*SIG(2)
      IF(VAL.LE.TOL.AND.SIG(INDX3).EQ.0.D0) THEN
          FLAG=1.D0
      ELSEIF(VAL.LE.TOL.AND.SIG(INDX3).NE.0.D0) THEN
          FLAG=2.D0
      ELSE
          FLAG=0.D0
      END IF
      IF(FLAG.EQ.1.D0) THEN ! UNIAXIAL CASE
        DO I=1,NDIM3
          VAL=MAXVAL(DABS(SIG))
          IF(DABS(SIG(I)).EQ.VAL) INDX2=I
        END DO
        SIGN=DSTRAN(INDX2)/DABS(DSTRAN(INDX2))
        IF(INDX2.EQ.INDX.AND.SIGN.EQ.1.D0) BC0=2*BC
        BC1=DLOG(SIGN*BC0+1.D0)
        DISP2=BC1-STRAN0(INDX2)
        DISP2=DEXP(DISP2)-1.D0
        SCALE=DABS(DISP2*DT)/DABS(DSTRAN(INDX2))
        DSTRAN=SCALE*DSTRAN
      ELSEIF(FLAG.EQ.2.D0) THEN ! UNIAXIAL + SHEAR
C     UNIAXIAL
        DO I=1,NDIM1
          VAL=MAXVAL(DABS(SIG(1:NDIM1)))
          IF(DABS(SIG(I)).EQ.VAL) INDX2=I
        END DO
        SIGN=DSTRAN(INDX2)/DABS(DSTRAN(INDX2))
C          IF(INDX2.EQ.INDX.AND.SIGN.EQ.1.D0) BC=2*BC
        BC1=DLOG(SIGN*BC+1.D0)
        DISP2=BC1-STRAN0(INDX2)
        DISP2=DEXP(DISP2)-1.D0
        SCALE=DABS(DISP2*DT)/DABS(DSTRAN(INDX2))
        DSTRAN(1:NDIM1)=SCALE*DSTRAN(1:NDIM1)
C     SHEAR
        INDX2=NDIM1+1
        SIGN=DSTRAN(INDX2)/DABS(DSTRAN(INDX2))
        BC1=DLOG(SIGN*BC+1.D0)
        DISP2=BC1-STRAN0(INDX2)
        DISP2=DEXP(DISP2)-1.D0
        SCALE=DABS(DISP2*DT)/DABS(DSTRAN(INDX2))
        DSTRAN(INDX2)=SCALE*DSTRAN(INDX2)
      ELSE ! MULTIAXIAL CASE
        DO I=1,NDIM3
            IF(DABS(DSTRAN(I)).GE.TOL) THEN
                SIGN=DSTRAN(I)/DABS(DSTRAN(I))
            ELSE
                SIGN=0.D0
            END IF
            BC1=DLOG(SIGN*BC+1.D0)
            DISP(I)=BC1-STRAN0(I)
            DISP(I)=DEXP(DISP(I))-1.D0
            SCALE=DABS(DISP(I)*DT)/DABS(DSTRAN(I))
            IF(ISNAN(SCALE)) SCALE=0.D0
            IF(DISP(I).NE.0.D0) SIGN2=DISP(I)/DABS(DISP(I))
            DSTRAN(I)=SCALE*SIGN2*DABS(DSTRAN(I))
        END DO
      END IF
      SIG_BAR=0.D0
C     [2] FLOW STRESS
      CALL FLOW_STRESS(HARD_VAR, EQPLAS, FLOW_SIG, DHDE)
      DSTRESS=MATMUL(DDSDDE,DSTRAN)
C..................................................................
      DO WHILE(SIG_BAR .LT. FLOW_SIG)
C     [3] UPDATE STRESS
        SIG=SIG+DSTRESS
C     [4] EQUIVALENT STRESS
        CALL EFF_SIG(HARD_VAR, SIG, SIG_BAR)
      END DO
      RETURN
      END SUBROUTINE INIT_STRESS
C-----------------------------------------------------------------------
      SUBROUTINE INIT_STRESS2(BC,HARD_VAR,SIG,STRAN0,EQPLAS,DDSDDE)
C     # THIS SUBROUTINE OBTAINS THE INITIAL STRESS TENSOR
C       RIGHT BEFORE PLASTIC DEFORMATION STARTS
C     # STRAIN INCREMENT IS CALULATED FROM AN UNDEFORMED MATERIAL
      IMPLICIT REAL*8(A-H, O-Z)
      DIMENSION HARD_VAR(NDIM7), SIG(NDIM3), STRAN0(NDIM3),
     1 DDSDDE(NDIM3,NDIM3), DDEDDS(NDIM3,NDIM3)
      DIMENSION EIG_VAL(3), EIG_VEC(3,3), DSTRESS(NDIM3),
     1 DSTRAN(NDIM3), STRAN1(NDIM3)
      DIMENSION DISP(NDIM3)

C      DIMENSION STRESS0(NDIM3),ELAS(NDIM3),PLAS(NDIM3),STRAN(NDIM3)
      COMMON /KUMAT/ NPROPS, NSTATV, NTENS, NDATA
      COMMON /KOPTION/ HARD_PAR, YLD_PAR, FLOW_PAR, SUA_PAR, GRAD_PAR
      COMMON /KSIZE/ NDIM1, NDIM2, NDIM3, NDIM4, NDIM5, NDIM6, NDIM7
      COMMON /KCAMP2/ INDX
      PARAMETER(TOL=1.D-16)
      CALL INVERSE(DDSDDE,NTENS,DDEDDS)
      DSTRAN=MATMUL(DDEDDS,SIG)
C     [0] MAJOR STRESS COMPONENT
      VAL=MAXVAL(DABS(SIG))
      DO I=1,NDIM3
          IF(SIG(I).EQ.VAL) THEN
              INDX2=I
              EXIT
          END IF
      END DO
C     [1] DETERMINE DISPLACEMENT
      DT=1.D0/NDATA
      SIGN=DSTRAN(INDX2)/DABS(DSTRAN(INDX2))
      IF(INDX2.EQ.INDX.AND.SIGN.EQ.1.D0) THEN
          BC0=2*BC
      ELSE
          BC0=BC
      END IF
      BC1=DLOG(SIGN*BC0+1.D0)
      DISP2=BC1-STRAN0(INDX2)
      DISP2=DEXP(DISP2)-1.D0
      SCALE=DABS(DISP2*DT)/DABS(DSTRAN(INDX2))
      DSTRAN=SCALE*DSTRAN
      SIG_BAR=0.D0
C     [2] FLOW STRESS
      CALL FLOW_STRESS(HARD_VAR, EQPLAS, FLOW_SIG, DHDE)
      DSTRESS=MATMUL(DDSDDE,DSTRAN)
C..................................................................
      DO WHILE(SIG_BAR .LT. FLOW_SIG)
C     [3] UPDATE STRESS
        SIG=SIG+DSTRESS
C     [4] EQUIVALENT STRESS
        CALL EFF_SIG(HARD_VAR, SIG, SIG_BAR)
      END DO
      RETURN
      END SUBROUTINE INIT_STRESS2
C-----------------------------------------------------------------------
      SUBROUTINE SHEAR_STRESS(HARD_VAR, STRESS)
      IMPLICIT REAL*8(A-H, O-Z)
      DIMENSION HARD_VAR(NDIM7), STRESS(NDIM3)
      COMMON /KSIZE/ NDIM1, NDIM2, NDIM3, NDIM4, NDIM5, NDIM6, NDIM7

      TAU=STRESS(NDIM1+1)
      BETA1= 0.D0
      BETA2= 2.D0*DTAN(1.D0)
      BETA3= BETA1
      ITER=0
      ITER_MAX= 1000
      DO I=1, ITER_MAX
          STRESS(NDIM1+1)= DTAN(BETA3)
          CALL EFF_SIG(HARD_VAR, STRESS, SIG_BAR)
          STRESS=STRESS/SIG_BAR
          IF(STRESS(NDIM1+1) .GT. TAU) THEN
              BETA2=BETA3
          ELSE
              BETA1=BETA3
          END IF
          BETA3=(BETA1+BETA2)/2.D0
          ITER=ITER+1
          IF(DABS(BETA1-BETA2) .LT. 1.D-10) THEN
              EXIT
          END IF
      END DO

      RETURN
      END SUBROUTINE SHEAR_STRESS
C-----------------------------------------------------------------------
      SUBROUTINE ROTMAT(ANG,TENS1,FACT)
C     # THIS SUBROUTINE ROTATES A TENSOR
C     # FACT=1: STRESS | FACT=2: STRAIN
C     RZ=[COS(Z) -SIN(Z) 0]  |  RY=[ COS(Y) 0 SIN(Y)]
C        [SIN(Z)  COS(Z) 0]  |     [     0  1     0 ]
C        [    0       0  1]  |     [-SIN(Y) 0 COS(Y)]
      IMPLICIT REAL*8(A-H, O-Z)
      DIMENSION TENS1(NTENS)
      DIMENSION R(3,3), RT(3,3), TENS2(3,3)
      COMMON /KUMAT/ NPROPS, NSTATV, NTENS, NDATA
      PARAMETER(RADIAN=DATAN(1.D0)/45.D0)
      TH=ANG*RADIAN
C     [1] ROTATION MATRIX
      R(1,1)= DCOS(TH)
      R(1,2)=-DSIN(TH)
      R(1,3)= 0.D0
      R(2,1)= DSIN(TH)
      R(2,2)= DCOS(TH)
      R(2,3)= 0.D0
      R(3,1)= 0.D0
      R(3,2)= 0.D0
      R(3,3)= 1.D0
      RT=TRANSPOSE(R)
C     [2] TRANSFORM VECTOR INTO TENSOR
      CALL TRANS_VOIGT(TENS1,TENS2,FACT,1)
C     [3] ROTATION THE SECOND-ORDER TENSOR
      TENS2=MATMUL(RT,TENS2)
      TENS2=MATMUL(TENS2,R)
C     [4] TRANSFORM TENSOR INTO VECTOR
      CALL TRANS_VOIGT(TENS1,TENS2,FACT,2)
      RETURN
      END SUBROUTINE ROTMAT
C-----------------------------------------------------------------------
      SUBROUTINE TRANS_VOIGT(TENS1,TENS2,FACT,IOPT)
      IMPLICIT REAL*8(A-H, O-Z)
C     # THIS SUBROUTINE TRANSFORM TENSORS INTO VOIGT NOTATION.
C     S=[S11 S22 S12] OR S=[S11 S22 S33 S12 S13 S23]
C     S=[S11 S22 S12] OR S=[S11 S22 S33 S12 S13 S23]
C     # FACT=1: STRESS TENSOR | FACT=2: STRAIN TENSOR
C     # IOPT=1: VECTOR -> TENSOR | IOPT=2: TENSOR -> VECTOR
      DIMENSION TENS1(NTENS), TENS2(3,3)
      COMMON /KUMAT/ NPROPS, NSTATV, NTENS, NDATA

      IF(IOPT.EQ.1) THEN ! VECTOR -> TENSOR
        TENS2=0.D0
        IF(NTENS.EQ.3) THEN
          TENS2(1,1)=TENS1(1)
          TENS2(2,2)=TENS1(2)
          TENS2(1,2)=TENS1(3)/FACT
          TENS2(2,1)=TENS1(3)/FACT
        ELSE
          TENS2(1,1)=TENS1(1)
          TENS2(2,2)=TENS1(2)
          TENS2(3,3)=TENS1(3)
          TENS2(1,2)=TENS1(4)/FACT
          TENS2(2,1)=TENS1(4)/FACT
          TENS2(1,3)=TENS1(5)/FACT
          TENS2(3,1)=TENS1(5)/FACT
          TENS2(2,3)=TENS1(6)/FACT
          TENS2(3,2)=TENS1(6)/FACT
        END IF
      ELSEIF(IOPT.EQ.2) THEN ! TENSOR -> VECTOR
        TENS1=0.D0
        IF(NTENS.EQ.3) THEN
          TENS1(1)=TENS2(1,1)
          TENS1(2)=TENS2(2,2)
          TENS1(3)=TENS2(1,2)*FACT
        ELSE
          TENS1(1)=TENS2(1,1)
          TENS1(2)=TENS2(2,2)
          TENS1(3)=TENS2(3,3)
          TENS1(4)=TENS2(1,2)*FACT
          TENS1(5)=TENS2(1,3)*FACT
          TENS1(6)=TENS2(2,3)*FACT
        END IF
      END IF

      RETURN
      END SUBROUTINE TRANS_VOIGT
C-----------------------------------------------------------------------
      SUBROUTINE EIG(TENS1, EIG_VAL, EIG_VEC, FACT, IOPT)
C     #   THIS SUBROUTINE CALCULATES EIGEN VALUES AND
C         VECTORS OF THE 2ND-ORDER TENSOR.
C     IOPT=1: NORMAL SPACE -> PRINCIPAL SPACE
C     IOPT=2: PRINCIPAL SPACE -> NORMAL SPACE
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION TENS1(NTENS), TENS2(3,3), EIG_VAL(3), EIG_VEC(3,3)
      DIMENSION R(3,3), RT(3,3)
      COMMON /KUMAT/ NPROPS, NSTATV, NTENS, NDATA

      IF(IOPT.EQ.1) THEN
          CALL TRANS_VOIGT(TENS1,TENS2,FACT,1)
          CALL JACOBI(TENS2, 3, 3, EIG_VAL, EIG_VEC)
C          CALL EIGSRT(EIG_VAL, EIG_VEC, 3, 3)
      ELSEIF(IOPT.EQ.2)THEN
          R=EIG_VEC
          RT=TRANSPOSE(R)
          CALL TRANS_VOIGT(EIG_VAL,TENS2,FACT,1)
          TENS2=MATMUL(R,TENS2)
          TENS2=MATMUL(TENS2,RT)
          CALL TRANS_VOIGT(TENS1,TENS2,FACT,2)
      END IF

      RETURN
      END SUBROUTINE EIG
C-----------------------------------------------------------------------
      SUBROUTINE JACOBI(A,N,NP,D,V)
C     #   THIS SUBROUTINE RETURNS EIGENVALUE AND EIGENVECTOR OF
C         A SYMMTERIC MATRIX.
C     #   THIS SUBROUTINE WAS COPIED FROM THE NUMERICAL RECIPIES.
      INTEGER N,NP,NROT,NMAX
      REAL*8 A(NP,NP),D(NP),V(NP,NP)
      PARAMETER (NMAX=500)
      INTEGER I,IP,IQ,J
      REAL*8 C,G,H,S,SM,T,TAU,THETA,TRESH,B(NMAX),Z(NMAX)
      DO 12 IP=1,N
        DO 11 IQ=1,N
          V(IP,IQ)=0.
11      CONTINUE
        V(IP,IP)=1.
12    CONTINUE
      DO 13 IP=1,N
        B(IP)=A(IP,IP)
        D(IP)=B(IP)
        Z(IP)=0.
13    CONTINUE
      NROT=0
      DO 24 I=1,50
        SM=0.
        DO 15 IP=1,N-1
          DO 14 IQ=IP+1,N
            SM=SM+ABS(A(IP,IQ))
14        CONTINUE
15      CONTINUE
        IF(SM.EQ.0.)RETURN
        IF(I.LT.4)THEN
          TRESH=0.2*SM/N**2
        ELSE
          TRESH=0.
        ENDIF
        DO 22 IP=1,N-1
          DO 21 IQ=IP+1,N
            G=100.*ABS(A(IP,IQ))
            IF((I.GT.4).AND.(ABS(D(IP))+
     * G.EQ.ABS(D(IP))).AND.(ABS(D(IQ))+G.EQ.ABS(D(IQ))))THEN
              A(IP,IQ)=0.
            ELSE IF(ABS(A(IP,IQ)).GT.TRESH)THEN
              H=D(IQ)-D(IP)
              IF(ABS(H)+G.EQ.ABS(H))THEN
                T=A(IP,IQ)/H
              ELSE
                THETA=0.5*H/A(IP,IQ)
                T=1./(ABS(THETA)+SQRT(1.+THETA**2))
                IF(THETA.LT.0.)T=-T
              ENDIF
              C=1./SQRT(1+T**2)
              S=T*C
              TAU=S/(1.+C)
              H=T*A(IP,IQ)
              Z(IP)=Z(IP)-H
              Z(IQ)=Z(IQ)+H
              D(IP)=D(IP)-H
              D(IQ)=D(IQ)+H
              A(IP,IQ)=0.
              DO 16 J=1,IP-1
                G=A(J,IP)
                H=A(J,IQ)
                A(J,IP)=G-S*(H+G*TAU)
                A(J,IQ)=H+S*(G-H*TAU)
16            CONTINUE
              DO 17 J=IP+1,IQ-1
                G=A(IP,J)
                H=A(J,IQ)
                A(IP,J)=G-S*(H+G*TAU)
                A(J,IQ)=H+S*(G-H*TAU)
17            CONTINUE
              DO 18 J=IQ+1,N
                G=A(IP,J)
                H=A(IQ,J)
                A(IP,J)=G-S*(H+G*TAU)
                A(IQ,J)=H+S*(G-H*TAU)
18            CONTINUE
              DO 19 J=1,N
                G=V(J,IP)
                H=V(J,IQ)
                V(J,IP)=G-S*(H+G*TAU)
                V(J,IQ)=H+S*(G-H*TAU)
19            CONTINUE
              NROT=NROT+1
            ENDIF
21        CONTINUE
22      CONTINUE
        DO 23 IP=1,N
          B(IP)=B(IP)+Z(IP)
          D(IP)=B(IP)
          Z(IP)=0.
23      CONTINUE
24    CONTINUE

      RETURN
      END SUBROUTINE JACOBI
C-----------------------------------------------------------------------
      SUBROUTINE EIGSRT(D,V,N,NP)
C     #   THIS SUBROUTINE SORTS THE EIGENVALUES AND VECTORS CALCULATED FROM
C         SUB. JACOBI.
      INTEGER N,NP
      REAL*8 A(NP),D(NP),V(NP,NP)
      INTEGER I,J,K
      REAL*8 P
C      D=DABS(A)
      DO I=1,N-1
        K=I
        P=D(I)
        DO J=I+1,N
          IF(D(J).GE.P)THEN
            K=J
            P=D(J)
          ENDIF
        END DO
        IF(K.NE.I)THEN
          D(K)=D(I)
          D(I)=P
          DO J=1,N
            P=V(J,I)
            V(J,I)=V(J,K)
            V(J,K)=P
          END DO
        ENDIF
      END DO
      RETURN
      END SUBROUTINE EIGSRT
C-----------------------------------------------------------------------
      SUBROUTINE LINSPACE(FROM, TO, N, ARRAY)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION ARRAY(N)

      RANGE = TO - FROM

      IF (N == 0) RETURN

      IF (N == 1) THEN
          ARRAY(1) = FROM
          RETURN
      END IF
      DO I=1, N
          ARRAY(I) = FROM + RANGE*(I-1)/(N-1)
      END DO
      END SUBROUTINE LINSPACE
C-----------------------------------------------------------------------
      SUBROUTINE PROGRESS(PERC, ISTEP)
      IMPLICIT REAL*8 (A-H,O-Z)
C     # THIS SUBROUTINE VISUALIZES THE PERCENTAGE OF PROGRESS.
C     ITESP= 5 / 10
      INTEGER(KIND=4) :: INDX,K
      CHARACTER(LEN=20) :: BAR10=" XXX %  |..........|"
      CHARACTER(LEN=30) :: BAR20=" XXX %  |....................|"

C     [0] AUTOMATIC SET-UP OF DENOMINATOR
      IF(ISTEP.EQ.5 .OR. ISTEP.EQ.10) THEN
            STEP=ISTEP
            ISTEP2=ISTEP
      ELSE
            STEP=10.D0
            ISTEP2=STEP
      END IF
C     [1]
      XVAL=PERC*100.D0
      XMOD=MOD(XVAL,STEP)
      IF(XMOD.NE.0.D0) RETURN
C     [2]
      IF(STEP.EQ.5.D0) THEN
C     CLEAR SCREEN
            CALL SYSTEM('CLS')
C     INITIALIZE INDEX
            INDX=INT(XVAL/STEP)
C     UPDATES THE FRACTION OF CALCULATION DONE
            WRITE(UNIT=BAR20(2:4),FMT="(I3)") ISTEP2*INDX
            DO K=1, INDX
                  BAR20(9+K:9+K)="="
            ENDDO
C     PRINT THE PROGRESS BAR.
            WRITE(*,'(A)') BAR20
      ELSEIF(STEP.EQ.10.D0) THEN
            CALL SYSTEM('CLS')
            INDX=INT(XVAL/STEP)
            WRITE(UNIT=BAR10(2:4),FMT="(I3)") ISTEP2*INDX
            DO K=1, INDX
                  BAR10(9+K:9+K)="="
            ENDDO
            WRITE(*,'(A)') BAR10
      END IF

      RETURN
      END SUBROUTINE PROGRESS
C-----------------------------------------------------------------------
      SUBROUTINE YLD_SURFACE(STAT_VAR)
C     # THIS SUBROUTINE PLOTS YIELD LOCUS (S11, S22) FOR ISOTROPIC HARDENING.
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION STATEV(NSTATV)
      DIMENSION STAT_VAR(NDIM6), SIG(NTENS), PSIG(2)
      DIMENSION YL_SIG(360,2), PI_SIG(360,2)
      COMMON /KUMAT/ NPROPS, NSTATV, NTENS, NDATA
      COMMON /KSIZE/ NDIM1, NDIM2, NDIM3, NDIM4, NDIM5, NDIM6, NDIM7
      CHARACTER :: FILEPATH*128
      PARAMETER(PI=DACOS(-1.D0), RADIAN=DATAN(1.D0)/45.D0)

C     [2]   READ HARDENING STATE VARIABLE
      SIG=0.D0
      CALL READ_STATEV(STATEV, STAT_VAR, SIG) !STATEV -> STAT_VAR
C     [3]   YIELD LOCUS AND PI-PLANE
      DO I=0,360
            ANG=I*RADIAN
            SIG=0.D0
            SIG(1)= DCOS(ANG)
            SIG(2)= DSIN(ANG)
            CALL EFF_SIG(STAT_VAR, SIG, SIG_BAR)
            SIG=SIG/SIG_BAR
            YL_SIG(I,1:2)=SIG(1:2)
            CALL PI_PLANE(SIG,PSIG)
            PI_SIG(I,1:2)=PSIG
      END DO
      FILEPATH='OUT\YLD_LOCUS.CSV'
      CALL WRITE_CSV(360,2,YL_SIG,FILEPATH,1,1)
      FILEPATH='OUT\PI_PLANE.CSV'
      CALL WRITE_CSV(360,2,PI_SIG,FILEPATH,1,1)

      RETURN
      END SUBROUTINE YLD_SURFACE
C-----------------------------------------------------------------------
      SUBROUTINE YLD_SURFACE2(STATEV)
C     # THIS SUBROUTINE PLOTS YIELD LOCUS (S11, S22) FOR ANISOTROPIC HARDENING.
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION STATEV(NSTATV)
      DIMENSION STAT_VAR(NDIM6), SIG(NDIM3),ALPHA(NDIM3),DEV_SIG(NDIM4)
      DIMENSION PSIG(2), YL_SIG(360,4), PI_SIG(360,4)
      COMMON /KUMAT/ NPROPS, NSTATV, NTENS, NDATA
      COMMON /KOPTION/ HARD_PAR, YLD_PAR, FLOW_PAR, SUA_PAR, GRAD_PAR
      COMMON /KSIZE/ NDIM1, NDIM2, NDIM3, NDIM4, NDIM5, NDIM6, NDIM7
      CHARACTER :: FILEPATH*128
      PARAMETER(PI=DACOS(-1.D0), RADIAN=DATAN(1.D0)/45.D0)

C     [2]   READ HARDENING STATE VARIABLE
      SIG=0.D0
      CALL READ_STATEV(STATEV, STAT_VAR, SIG) !STATEV -> STAT_VAR
      IF(HARD_PAR.EQ.1.D0 .OR. HARD_PAR.EQ.2.D0) THEN ! KINEMATIC HARDENING
            ALPHA=STAT_VAR(1:NDIM3)
            CALL EFF_SIG(STAT_VAR, ALPHA, SIG_BAR2)
      ELSE
            ALPHA=0.D0
            SIG_BAR2=1.D0
      END IF
C     [3]   YIELD LOCUS AND PI-PLANE
      DO I=0,360
            ANG=I*RADIAN
            SIG=0.D0
            SIG(1)= DCOS(ANG)
            SIG(2)= DSIN(ANG)
C     [3.1] CHANGED YIELD SURFACE
            CALL EFF_SIG(STAT_VAR, SIG, SIG_BAR)
            YL_SIG(I,1:2)=SIG/SIG_BAR+0.25D0*ALPHA/SIG_BAR2
            CALL PI_PLANE(SIG/SIG_BAR+0.25D0*ALPHA/SIG_BAR2,PSIG)
            PI_SIG(I,1:2)=PSIG
C     [3.2] COMPLETE YIELD SURFACE
            CALL YLD(SIG, DEV_SIG, SIG_BAR, 1)
            YL_SIG(I,3:4)=SIG/SIG_BAR
            CALL PI_PLANE(SIG/SIG_BAR,PSIG)
            PI_SIG(I,3:4)=PSIG
      END DO
      FILEPATH='OUT\YLD_LOCUS.CSV'
      CALL WRITE_CSV(360,4,YL_SIG,FILEPATH,1,1)
      FILEPATH='OUT\PI_PLANE.CSV'
      CALL WRITE_CSV(360,4,PI_SIG,FILEPATH,1,1)

      RETURN
      END SUBROUTINE YLD_SURFACE2
C-----------------------------------------------------------------------
      SUBROUTINE PI_PLANE(STRESS, PI_SIG)
      IMPLICIT REAL*8(A-H, O-Z)
      DIMENSION STRESS(NDIM3), PI_SIG(2), DEV_SIG(NDIM4)
      DIMENSION ROTMAT(2,2)
      PARAMETER(PI=DACOS(-1.D0), RADIAN=DATAN(1.D0)/45.D0)
      COMMON /KSIZE/ NDIM1, NDIM2, NDIM3, NDIM4, NDIM5, NDIM6, NDIM7

C      PI_SIG(1)= 2.D0*STRESS(1)/DSQRT(6.D0)-STRESS(2)/DSQRT(6.D0)
C      PI_SIG(2)= STRESS(2)/DSQRT(2.D0)
      CALL DEVIATORIC(STRESS, DEV_SIG)

      PI_SIG(1)= 2.D0*DEV_SIG(1)/DSQRT(6.D0)-DEV_SIG(2)/DSQRT(6.D0)
     1          -DEV_SIG(3)/DSQRT(6.D0)
      PI_SIG(2)= DEV_SIG(2)/DSQRT(2.D0)-DEV_SIG(3)/DSQRT(2.D0)

C
      RETURN
      END SUBROUTINE PI_PLANE
C-----------------------------------------------------------------------
      SUBROUTINE WRITE_CSV(NSIZE1,NSIZE2,XDATA,FILEPATH, IOPT1, IOPT2)
C     # THIS SUBROUTINE WRITE CODE RESULTS IN CSV FORMAT.
C
C     IOPT1=0: FREE FORMAT
C     IOPT1=1: NORMAL FORMAT     : 0.0001
C     IOPT1=2: ENGINEERING FORMAT: 1.0000E-5
C
C     IOPT2=1: CLOSE FILE AFTER WRITING CSV
C     IOPT2=2: KEEP FILE OPEN AFTER WRITING CSV
C     IOPT2=3: CLOSE FILE IMMEDIATELY
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION XDATA(NSIZE1,NSIZE2)
      CHARACTER :: FILEPATH*128

101   FORMAT(1X,F8.4,',')
102   FORMAT(1X,F8.4/)
103   FORMAT(1X,F8.4)
C
201   FORMAT(1X,ES12.4,',')
202   FORMAT(1X,ES12.4/)
203   FORMAT(1X,ES12.4)

C     [1]   OPEN CSV FILE
      OPEN(999, FILE=TRIM(FILEPATH), STATUS='UNKNOWN')
      IF(IOPT2.EQ.3) THEN
            CLOSE(999)
            RETURN
      END IF
C-----------------------------------------------------------------------
C     [2]   WRITE CSV FILE
      IF(IOPT1.EQ.1) THEN
      IF(NSIZE1.GT.1) THEN
            DO I=1,NSIZE1
            DO J=1,NSIZE2
                  IF(J.LT.NSIZE2) THEN
                        WRITE(999, 101, ADVANCE='NO') XDATA(I,J)
                  ELSEIF(J.EQ.NSIZE2) THEN
                        WRITE(999, 102, ADVANCE='NO') XDATA(I,J)
                  END IF
            END DO
            END DO
      ELSEIF(NSIZE1.EQ.1) THEN
            DO I=1,NSIZE1
            DO J=1,NSIZE2
                  IF(J.LT.NSIZE2) THEN
                        WRITE(999, 101, ADVANCE='NO') XDATA(I,J)
                  ELSEIF(J.EQ.NSIZE2) THEN
                        WRITE(999, 103, ADVANCE='NO') XDATA(I,J)
                  END IF
            END DO
            END DO
      END IF
C-----------------------------------------------------------------------
      ELSEIF(IOPT1.EQ.2) THEN
      IF(NSIZE1.GT.1) THEN
            DO I=1,NSIZE1
            DO J=1,NSIZE2
                  IF(J.LT.NSIZE2) THEN
                        WRITE(999, 201, ADVANCE='NO') XDATA(I,J)
                  ELSEIF(J.EQ.NSIZE2) THEN
                        WRITE(999, 202, ADVANCE='NO') XDATA(I,J)
                  END IF
            END DO
            END DO
      ELSEIF(NSIZE1.EQ.1) THEN
            DO I=1,NSIZE1
            DO J=1,NSIZE2
                  IF(J.LT.NSIZE2) THEN
                        WRITE(999, 201, ADVANCE='NO') XDATA(I,J)
                  ELSEIF(J.EQ.NSIZE2) THEN
                        WRITE(999, 203, ADVANCE='NO') XDATA(I,J)
                  END IF
            END DO
            END DO
      END IF
      END IF
C-----------------------------------------------------------------------
C     [3]   CLOSE CSV FILE
      IF(IOPT2.EQ.1) CLOSE(999)

      RETURN
      END SUBROUTINE WRITE_CSV