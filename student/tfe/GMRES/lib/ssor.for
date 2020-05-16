C++++++++++++++++++++++++++++++++++++++++++++++++++++
C                 SSOR preconditioner
C
C Rem : A ne peut pas avoir d'elem. diag. nul !
C       Diverge si cond(A) >>
C       Sinon, donne de bons résultats
C       Colonnes triées sinon crash !
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++

C----------------------------------------------------
C    Creation du preconditionneur -> ALU
C----------------------------------------------------

      SUBROUTINE SSOR(N,A,IA,JA,ALU,JLU,JU,omega,ierr)
C
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER IA(*), JA(*), JLU(*), JU(*)
      REAL*8 A(*), ALU(*), omega
C
      ierr=0
      kk=N+1      
      DO i=1,N
         JLU(i)=kk+1
         DO k=IA(i), IA(i+1)-1
            kk=kk+1
            IF(JA(k).LT.i) THEN
               ALU(kk)=A(k)/ALU(JA(k))*omega
            ELSE IF (JA(k).EQ.i) THEN
               IF(A(k).EQ.0.0D0) THEN
                  IERR=1
                  GOTO 999
               ENDIF
               ALU(i)=A(k)
               JU(i)=kk+1
            ELSE IF (JA(k).GT.i) THEN
               ALU(kk)=A(k)*omega
            ENDIF
            JLU(kk)=JA(k)
         ENDDO
      ENDDO
      DO i=1,N
         ALU(i)=1.0D0/ALU(i)
      ENDDO

 999  RETURN
      END
