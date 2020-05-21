C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C       CHKDUAL : Teste si le Hessien est diagonal
C
C dernière modification : 09.02.97
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Ouput : IERR=1 si Hessien non diag.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE CHKDUAL(N,A,IERR)
C
      INTEGER N,IERR,i,j,k
      REAL*8 A(*)
C
      IERR=0
      k=0
      DO i=1,N
         DO j=1,i
            k=k+1
            IF((A(k).NE.0).AND.(i.NE.j)) IERR=1
         ENDDO
      ENDDO
C
      RETURN
      END