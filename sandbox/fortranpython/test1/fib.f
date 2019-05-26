C https://docs.scipy.org/doc/numpy/f2py/getting-started.html
C Fixed format
C  => sera interfacé "fib" même si "FIB"
C
C FILE: FIB1.F
      SUBROUTINE FIB(A, N)
C
C     CALCULATE FIRST N FIBONACCI NUMBERS
C
      INTEGER N
      REAL*8 A(N)
      DO I=1,N
         IF (I.EQ.1) THEN
             A(I) = 0.0D0
         ELSEIF (I.EQ.2) THEN
             A(I) = 1.0D0
         ELSE 
             A(I) = A(I-1) + A(I-2)
         ENDIF
      ENDDO
      END
C END FILE FIB1.F


      SUBROUTINE FIB2(A, N)
C
C     CALCULATE FIRST N FIBONACCI NUMBERS
C
          INTEGER N
          REAL*8 A(N)
Cf2py intent(in) n
Cf2py intent(out) a
Cf2py depend(n) a
          DO I=1,N
             IF (I.EQ.1) THEN
                 A(I) = 0.0D0
             ELSEIF (I.EQ.2) THEN
                 A(I) = 1.0D0
             ELSE 
                 A(I) = A(I-1) + A(I-2)
             ENDIF
          ENDDO
          END
