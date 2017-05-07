      SUBROUTINE CONDI(H,j,m,co,si,conda)

      implicit real*8(a-h,o-z)

      dimension co(m),si(m),H(m+1,m)

      do ii=1,1000

C     *** RQ ***

      do i=1,j-1
         t1=h(i,i)
         t2=h(i,i+1)
         t4=h(i+1,i+1)
         c=co(i)
         s=si(i)
         h(i+1,i)  =  s*t4
         h(i,i)    =  c*t1+s*t2
         h(i,i+1)  = -s*t1+c*t2
         h(i+1,i+1)=  c*t4
      enddo

C     *** condA ***

      alam1=1000.0
      alam2=0.0
      do i=1,j
C         Write(*,*)H(i,i)
         if(dabs(H(i,i)).LT.alam1) alam1=dabs(H(i,i))
         if(dabs(H(i,i)).GT.alam2) alam2=dabs(H(i,i))
      enddo
      condA=alam2/alam1
      write(*,*)'Nbre de cond de A=',condA

C     *** QR ***

      do jj=1,j-1 
  
         DO i=1, jj-1
            temp   =  H(i,jj)*co(i) + H(i+1,jj)*si(i)
            H(i+1,jj) = -H(i,jj)*si(i) + H(i+1,jj)*co(i)
            H(i,jj)   =  temp
         ENDDO

         temp=DSQRT(H(jj,jj)*H(jj,jj)+H(jj+1,jj)*H(jj+1,jj))
         si(jj)=H(jj+1,jj)/temp
         co(jj)=H(jj,jj)/temp

         i=jj
         temp        =  H(i,jj)*co(i) + H(i+1,jj)*si(i)
         H(i+1,jj)   = -H(i,jj)*si(i) + H(i+1,jj)*co(i)
         H(i,jj)     =  temp            

      enddo



      enddo

      RETURN
      END