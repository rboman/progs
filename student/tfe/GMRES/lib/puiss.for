C+++++++++++++++++++++++++++++++++++++++++++++++++++++
C Extraction de la plus grande vp
C+++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C ierr=1 si pas de convergence apres itmax ités
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE PUISS(N,A,IA,JA,ALU,JLU,JU,Z,Y,xlmax,
     #                 itmax,xprec,ierr,iflag)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      DIMENSION A(*),IA(*),JA(*),Z(N),Y(N),
     #          ALU(*),JLU(*),JU(*)
C
      ERR  = 1.0D0
      ITE  = 0
      IERR = 0
      write(*,*)xprec,iflag
      read(*,*)
C
C     Vecteur initial
C     ---------------
      DO i=1,N
         z(i)=1.0D0
      ENDDO
C
C     Boucle principale      
C     -----------------
 10   IF(ERR.LT.xprec) GOTO 99
C
         sum=0.0D0
         DO i=1,N
            sum=sum+z(i)*z(i)
         ENDDO
         sum=DSQRT(sum)
         DO i=1,N
            y(i)=z(i)/sum
         ENDDO
C
         SOLD=S
         CALL SMMV(N, A, IA, JA, Y, Z)
         if(iflag.ne.0) 
     #      CALL LUSOL(N, z, z, ALU, JLU, JU)
         S = ProdScal(N, Y, Z)
         WRITE(*,*)'ite :',ITE,'lmax =',S,ERR
         ERR=DABS(S-SOLD)
         ITE=ITE+1
         IF(ITE.GT.ITMAX) THEN
            IERR=1
            GOTO 99
         ENDIF
      GOTO 10
C
 99   xlmax=S
C
      RETURN
      END