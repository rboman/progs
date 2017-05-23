      SUBROUTINE CHKCONT(X,C,d)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(6),C(6,15),d(15)

      DO i=1,15
         SUM=0.0D0
         DO j=1,6
            SUM=SUM+C(j,i)*X(j)
         ENDDO
         IF(SUM-d(i).GT.(1.0D-3*DABS(SUM))) THEN
C             WRITE(*,*)DABS(SUM-d(i)),(1.0D-3*DABS(SUM))
             WRITE(*,100)i,SUM,d(i)
         ENDIF
      ENDDO
      
      RETURN
 100  FORMAT(' Contrainte #',I2,' violee !',2D15.4)
      END