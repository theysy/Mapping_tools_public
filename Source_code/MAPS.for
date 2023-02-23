      INCLUDE 'LIBRARY\MML_U2SA.FOR'
      INCLUDE 'LIBRARY\LIBRARY.FOR'
C-----------------------------------------------------------------------
      IMPLICIT REAL*8(A-H, O-Z)
      REAL*8, DIMENSION(:), ALLOCATABLE :: PROPS, STATEV, SN, DUMMY1
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: DUMMY2
      DIMENSION SR(6)
      CHARACTER :: FILEPATH*128, FILEPROPS*128, DEBUGPATH*128
C
      COMMON /KUMAT/ NPROPS, NSTATV, NTENS, NDATA
      COMMON /KOPTION/ HARD_PAR, YLD_PAR, FLOW_PAR, SUA_PAR, GRAD_PAR
      COMMON /KSIZE/ NDIM1, NDIM2, NDIM3, NDIM4, NDIM5, NDIM6, NDIM7

C-----------------------------------------------------------------------
      XMAP_PAR=3.D0 ! 1: PRECISION MAP | 2: CONVERGENCE MAP | 3: DATA MAP
      FILEPROPS='PROPS_AA2090_YLD2K.csv'
C-----------------------------------------------------------------------
C     [1]   SIMULATION PARAMETERS
      BC=0.1D0 !BOUNDARY CONDITION
      DT=1.D-2 !TIME INCREMENT
      NDATA=CEILING(1.D0/DT)+1
C-----------------------------------------------------------------------
C     [2]   REFERENCE STRESS STATE
C     SR=[S11 S22 S33 S12 S13 S23]
      SR=0.D0
      SR(1)=1.D0
      ANG=0.D0
C-----------------------------------------------------------------------
C     [3]   READ USER MATERIAL PROPERTIES
      FILEPATH='UMAT_PROPS\'
      OPEN(1, FILE=TRIM(FILEPATH)//TRIM(FILEPROPS), STATUS='UNKNOWN')
C-----------------------------------------------------------------------
C     [4]   ALLOCATE PRIMARY ARRAYS
      I=0
      IOS=0
      DO WHILE(IOS .NE. -1)
            READ(1, *, IOSTAT=IOS)
            IF(IOS .EQ. -1) THEN
                  NPROPS=I
                  EXIT
            ELSE
                  I=I+1
            END IF
      END DO
      REWIND(1)
      ALLOCATE(PROPS(NPROPS))
      DO I=1, NPROPS
          READ(1, *) PROPS(I)
      END DO
      CLOSE(1)
C
      NSTATV=70
      ALLOCATE(STATEV(NSTATV))
C
      IF(PROPS(2) .NE. 4.D0) THEN
          NTENS=3
      ELSE
          NTENS=6
      END IF
      ALLOCATE(SN(NTENS), DUMMY1(NTENS), DUMMY2(NTENS,NTENS))
C
      SN=SR(1:NTENS)
C
      DUMMY1=0.D0; STATEV=0.D0
      CALL UMAT(PROPS, STATEV, SN, DUMMY1, DUMMY2)
C-----------------------------------------------------------------------
C     [5]   PRE-STRAIN SIMULATION
C      CALL PRE_STRAIN(ANG,BC,DT,SN,PROPS,STATEV)
C-----------------------------------------------------------------------
C     [6]   WRITE YIELD LOCUS AND PI-PLANE
      CALL YLD_SURFACE(STATEV)
C-----------------------------------------------------------------------
C     [7]   MAPPING
C     (1: PRECISION MAP | 2: CONVERGENCE MAP (STRAIN)
      IF(XMAP_PAR .EQ. 1.D0) THEN
            CALL PMAP(PROPS,STATEV,BC,2)
      ELSEIF(XMAP_PAR .EQ. 2.D0) THEN
            CALL CMAP(PROPS,STATEV,BC,2)
      ELSEIF(XMAP_PAR .EQ. 3.D0) THEN
            CALL DMAP(PROPS,STATEV,BC,2)
      END IF
C
C-----------------------------------------------------------------------
      DEALLOCATE(PROPS,STATEV,SN,DUMMY1,DUMMY2)
C-----------------------------------------------------------------------
      END
C-----------------------------------------------------------------------