      SUBROUTINE IC0 (NROW,A,IA,JA,L,IL,JL,JT,JI,DR,NZLMAX,
     $                INFO)
C
      IMPLICIT REAL*8(A-H,O-Z)

      INTEGER*4 INFO, JA(*), NROW, NZLMAX, IA(NROW+1), 
     #          IL(NROW+1)
      INTEGER JL(NZLMAX),JI(*)
      LOGICAL JT(NROW)
      REAL*8 A(*), DR(NROW), L(NZLMAX)
C
C     Miscellaneous parameters.
C
      REAL*8 ZONE, ZZERO
      PARAMETER (ZONE = 1.0D0, ZZERO = 0.0D0)
      REAL*8 DZERO
      PARAMETER (DZERO = 0.0D0)
C
C     Local variables.
C
      INTEGER I, J, K, KK
      REAL*8 RSUM, ZSUM
C
C ---
C     Computation of the IC0 starts here.
C     -----------------------------------
C     Initialize the counters and the data.
C
      INFO  = 0
      IL(1) = NROW+1
      DO 20 J = 1, NROW
         DR(J) = ZZERO
         JT(J) = .FALSE.
 20   CONTINUE
C
C     Iterate over all rows.  All further references to A actually apply
C     to the matrix as it is modified by the elimination,  with possible
C     fill-in, and not to the original matrix.
C
      DO 90 I = 1, NROW
C
C     Zero out DR.
C
         DO 30 J = 1, NROW
            IF (JT(J)) THEN
               DR(J) = ZZERO
               JT(J) = .FALSE.
            END IF
 30      CONTINUE
C
C     Spread the Ith row of A into DR.
C
         ITEST = 0
         DO 40 K = IA(I), IA(I+1)-1
            J     = JA(K)
            IF(I.GT.J) THEN
                  DR(J) = A(K)
                  JT(J) = .TRUE.
            ENDIF
            IF(I.EQ.J) THEN
               AA=A(K)
               ITEST = 1
            ENDIF
 40      CONTINUE
         IF(Itest.eq.0) write(*,*)'Ca foire!'

C
C     Compute the full row I of L.
C     Iterate over the columns of row I, up to the diagonal.
C
         RSUM = ZZERO
         DO 60 J = 1, I-1
            ZSUM = ZZERO
            DO 50 KK = IL(J), IL(J+1)-1
               K = JL(KK)
               IF (JT(K)) ZSUM = ZSUM + L(KK) * DR(K)
 50         CONTINUE
            ZSUM = ( DR(J) - ZSUM ) * L(J)
            IF (DABS(ZSUM).EQ.DZERO) GO TO 60
            DR(J) = ZSUM
            JT(J) = .TRUE.
            RSUM  = RSUM + ZSUM * ZSUM
 60      CONTINUE
         IF (AA.GT.RSUM) THEN
            L(I) = DSQRT( AA - RSUM )
         ELSE
            L(I)=1.0D0
            WRITE(*,*)'L -> 1',AA,RSUM,I
         ENDIF
C
C     Elimination of the Ith row of A is complete.
C
         K=0
         DO 70 J=1,I-1
            IF(JT(J)) THEN
               K=K+1
               JI(K)=J
            ENDIF
 70      CONTINUE

         KK = IL(I)
         IF (KK+K-1.GT.NZLMAX) INFO = 1
         IF (INFO.EQ.0) THEN
            DO 80 JJ = 1,K
               J      = JI(JJ)
               if(jt(j))then
                  JL(KK) = J
                  L(KK)  = DR(J)
                  KK     = KK + 1
               endif
 80         CONTINUE
         END IF
C
         IL(I+1) = KK
         L(I)  = ZONE / L(I)
C
 90   CONTINUE
c        OPEN(UNIT=1,FILE='llt.m',STATUS='UNKNOWN')
c        Do i=1,NROW
c          write(1,*)'L(',i,',',i,')=',1.0D0/l(i),';'
c          DO j=IL(i),IL(i+1)-1
c            write(1,*)'L(',i,',',jl(j),')=',l(j),';'
c          ENDDO
c        ENDDO
c        CLOSE(1)
C
C ------------------------------------
C
      WRITE (6,'(A15,I10)') ' Elements in L: ', IL(NROW+1)-1
C
 120  RETURN
C
      END
