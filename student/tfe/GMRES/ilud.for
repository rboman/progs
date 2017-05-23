c-----------------------------------------------------------------------
      subroutine ilud(n,a,ja,ia,alph,tol,alu,jlu,ju,iwk,w,jw,ierr)
c-----------------------------------------------------------------------
      implicit none 
      integer n,ja(*),ia(n+1),jlu(*),ju(n),jw(2*n),iwk,ierr      
      real*8 a(*),alu(*),w(2*n),tol, alph 
      
c----------------------------------------------------------------------*
c                     *** ILUD preconditioner ***                      *
c    incomplete LU factorization with standard droppoing strategy      *
c----------------------------------------------------------------------*
c Author: Yousef Saad * Aug. 1995 --                                   * 
c----------------------------------------------------------------------*
c This routine computes the ILU factorization with standard threshold  *
c dropping: at i-th step of elimination, an element a(i,j) in row i is *
c dropped  if it satisfies the criterion:                              *
c                                                                      *
c  abs(a(i,j)) < tol * [average magnitude of elements in row i of A]   *
c                                                                      *
c There is no control on memory size required for the factors as is    *
c done in ILUT. This routines computes also various diagonal compensa- * 
c tion ILU's such MILU. These are defined through the parameter alph   *
c----------------------------------------------------------------------* 
c on entry:
c========== 
c n       = integer. The row dimension of the matrix A. The matrix 
c
c a,ja,ia = matrix stored in Compressed Sparse Row format              
c
c alph    = diagonal compensation parameter -- the term: 
c
c           alph*(sum of all dropped out elements in a given row) 
c
c           is added to the diagonal element of U of the factorization 
c           Thus: alph = 0 ---> ~ ILU with threshold,
c                 alph = 1 ---> ~ MILU with threshold. 
c 
c tol     = Threshold parameter for dropping small terms in the
c           factorization. During the elimination, a term a(i,j) is 
c           dropped whenever abs(a(i,j)) .lt. tol * [weighted norm of
c           row i]. Here weighted norm = 1-norm / number of nnz 
c           elements in the row. 
c  
c iwk     = The length of arrays alu and jlu -- this routine will stop
c           if storage for the factors L and U is not sufficient 
c
c On return:
c=========== 
c
c alu,jlu = matrix stored in Modified Sparse Row (MSR) format containing
c           the L and U factors together. The diagonal (stored in
c           alu(1:n) ) is inverted. Each i-th row of the alu,jlu matrix
c           contains the i-th row of L (excluding the diagonal entry=1)
c           followed by the i-th row of U.
c
c ju      = integer array of length n containing the pointers to
c           the beginning of each row of U in the matrix alu,jlu.
c
c ierr    = integer. Error message with the following meaning.
c           ierr  = 0    --> successful return.
c           ierr .gt. 0  --> zero pivot encountered at step number ierr.
c           ierr  = -1   --> Error. input matrix may be wrong.
c                            (The elimination process has generated a
c                            row in L or U whose length is .gt.  n.)
c           ierr  = -2   --> Insufficient storage for the LU factors --
c                            arrays alu/ jalu are  overflowed. 
c           ierr  = -3   --> Zero row encountered.
c
c Work Arrays:
c=============
c jw      = integer work array of length 2*n.
c w       = real work array of length n 
c  
c----------------------------------------------------------------------
c
c w, ju (1:n) store the working array [1:ii-1 = L-part, ii:n = u] 
c jw(n+1:2n)  stores the nonzero indicator. 
c 
c Notes:
c ------
c All diagonal elements of the input matrix must be  nonzero.
c
c----------------------------------------------------------------------- 
c     locals
      integer ju0,k,j1,j2,j,ii,i,lenl,lenu,jj,jrow,jpos,len 
      real*8 tnorm, t, abs, s, fact, dropsum  
c-----------------------------------------------------------------------
c     initialize ju0 (points to next element to be added to alu,jlu)
c     and pointer array.
c-----------------------------------------------------------------------
      ju0 = n+2
      jlu(1) = ju0
c
c     initialize nonzero indicator array. 
c
      do 1 j=1,n
         jw(n+j)  = 0
 1    continue
c-----------------------------------------------------------------------
c     beginning of main loop.
c-----------------------------------------------------------------------
      do 500 ii = 1, n
         j1 = ia(ii)
         j2 = ia(ii+1) - 1
         dropsum = 0.0d0 
         tnorm = 0.0d0
         do 501 k=j1,j2
            tnorm = tnorm + abs(a(k)) 
 501     continue
         if (tnorm .eq. 0.0) goto 997
         tnorm = tnorm / real(j2-j1+1) 
c     
c     unpack L-part and U-part of row of A in arrays w 
c     
         lenu = 1
         lenl = 0
         jw(ii) = ii
         w(ii) = 0.0
         jw(n+ii) = ii
c
         do 170  j = j1, j2
            k = ja(j)
            t = a(j)
            if (k .lt. ii) then
               lenl = lenl+1
               jw(lenl) = k
               w(lenl) = t
               jw(n+k) = lenl
            else if (k .eq. ii) then
               w(ii) = t
            else
               lenu = lenu+1
               jpos = ii+lenu-1 
               jw(jpos) = k
               w(jpos) = t
               jw(n+k) = jpos
            endif
 170     continue
         jj = 0
         len = 0 
c     
c     eliminate previous rows
c     
 150     jj = jj+1
         if (jj .gt. lenl) goto 160
c-----------------------------------------------------------------------
c     in order to do the elimination in the correct order we must select
c     the smallest column index among jw(k), k=jj+1, ..., lenl.
c-----------------------------------------------------------------------
         jrow = jw(jj)
         k = jj
c     
c     determine smallest column index
c     
         do 151 j=jj+1,lenl
            if (jw(j) .lt. jrow) then
               jrow = jw(j)
               k = j
            endif
 151     continue
c
         if (k .ne. jj) then
c     exchange in jw
            j = jw(jj)
            jw(jj) = jw(k)
            jw(k) = j
c     exchange in jr
            jw(n+jrow) = jj
            jw(n+j) = k
c     exchange in w
            s = w(jj)
            w(jj) = w(k)
            w(k) = s
         endif
c
c     zero out element in row by setting resetting jw(n+jrow) to zero.
c     
         jw(n+jrow) = 0
c
c     drop term if small
c     
c         if (abs(w(jj)) .le. tol*tnorm) then
c            dropsum = dropsum + w(jj) 
c            goto 150
c         endif
c     
c     get the multiplier for row to be eliminated (jrow).
c     
         fact = w(jj)*alu(jrow)
c
c     drop term if small
c     
         if (abs(fact) .le. tol) then
            dropsum = dropsum + w(jj) 
            goto 150
         endif
c     
c     combine current row and row jrow
c
         do 203 k = ju(jrow), jlu(jrow+1)-1
            s = fact*alu(k)
            j = jlu(k)
            jpos = jw(n+j)
            if (j .ge. ii) then
c     
c     dealing with upper part.
c     
               if (jpos .eq. 0) then
c
c     this is a fill-in element
c     
                  lenu = lenu+1
                  if (lenu .gt. n) goto 995
                  i = ii+lenu-1
                  jw(i) = j
                  jw(n+j) = i
                  w(i) = - s
               else
c
c     this is not a fill-in element 
c
                  w(jpos) = w(jpos) - s
               endif
            else
c     
c     dealing with lower part.
c     
               if (jpos .eq. 0) then
c
c     this is a fill-in element
c
                  lenl = lenl+1
                  if (lenl .gt. n) goto 995
                  jw(lenl) = j
                  jw(n+j) = lenl
                  w(lenl) = - s
               else
c
c     this is not a fill-in element 
c
                  w(jpos) = w(jpos) - s
               endif
            endif
 203     continue
         len = len+1 
         w(len) = fact
         jw(len)  = jrow
         goto 150
 160     continue
c     
c     reset double-pointer to zero (For U-part only)
c     
         do 308 k=1, lenu
            jw(n+jw(ii+k-1)) = 0
 308     continue
c
c     update l-matrix
c
         do 204 k=1, len
            if (ju0 .gt. iwk) goto 996
            alu(ju0) =  w(k) 
            jlu(ju0) =  jw(k)
            ju0 = ju0+1
 204     continue
c     
c     save pointer to beginning of row ii of U
c     
         ju(ii) = ju0
c
c     go through elements in U-part of w to determine elements to keep
c
         len = 0
         do 205 k=1, lenu-1
c            if (abs(w(ii+k)) .gt. tnorm*tol) then 
            if (abs(w(ii+k)) .gt. abs(w(ii))*tol) then 
               len = len+1
               w(ii+len) = w(ii+k) 
               jw(ii+len) = jw(ii+k)
            else
               dropsum = dropsum + w(ii+k) 
            endif
205      continue
c
c     now update u-matrix
c
         if (ju0 + len-1 .gt. iwk) goto 996
         do 302 k=ii+1,ii+len
            jlu(ju0) = jw(k)
            alu(ju0) = w(k)
            ju0 = ju0+1
 302     continue
c
c     define diagonal element 
c 
         w(ii) = w(ii) + alph*dropsum 
c
c     store inverse of diagonal element of u
c              
         if (w(ii) .eq. 0.0) w(ii) = (0.0001 + tol)*tnorm
c     
         alu(ii) = 1.0d0/ w(ii) 
c     
c     update pointer to beginning of next row of U.
c     
         jlu(ii+1) = ju0
c-----------------------------------------------------------------------
c     end main loop
c-----------------------------------------------------------------------
 500  continue
      ierr = 0
      return
c
c     incomprehensible error. Matrix must be wrong.
c     
 995  ierr = -1
      return
c     
c     insufficient storage in alu/ jlu arrays for  L / U factors 
c     
 996  ierr = -2
      return
c     
c     zero row encountered
c     
 997  ierr = -3 
      return
c----------------end-of-ilud  ------------------------------------------
c-----------------------------------------------------------------------
      end
