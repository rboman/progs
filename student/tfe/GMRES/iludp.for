c----------------------------------------------------------------------
      subroutine iludp(n,a,ja,ia,alph,droptol,permtol,mbloc,alu,
     *     jlu,ju,iwk,w,jw,iperm,ierr)
c-----------------------------------------------------------------------
      implicit none
      integer n,ja(*),ia(n+1),mbloc,jlu(*),ju(n),jw(2*n),iwk,
     *     iperm(2*n),ierr
      real*8 a(*), alu(*), w(2*n), alph, droptol, permtol 
c----------------------------------------------------------------------*
c                     *** ILUDP preconditioner ***                     *
c    incomplete LU factorization with standard droppoing strategy      *
c    and column pivoting                                               * 
c----------------------------------------------------------------------*
c author Yousef Saad -- Aug 1995.                                      *
c----------------------------------------------------------------------*
c on entry:
c==========
c n       = integer. The dimension of the matrix A.
c
c a,ja,ia = matrix stored in Compressed Sparse Row format.
c           ON RETURN THE COLUMNS OF A ARE PERMUTED.
c
c alph    = diagonal compensation parameter -- the term: 
c
c           alph*(sum of all dropped out elements in a given row) 
c
c           is added to the diagonal element of U of the factorization 
c           Thus: alph = 0 ---> ~ ILU with threshold,
c                 alph = 1 ---> ~ MILU with threshold. 
c 
c droptol = tolerance used for dropping elements in L and U.
c           elements are dropped if they are .lt. norm(row) x droptol
c           row = row being eliminated
c
c permtol = tolerance ratio used for determning whether to permute
c           two columns.  Two columns are permuted only when 
c           abs(a(i,j))*permtol .gt. abs(a(i,i))
c           [0 --> never permute; good values 0.1 to 0.01]
c
c mbloc   = if desired, permuting can be done only within the diagonal
c           blocks of size mbloc. Useful for PDE problems with several
c           degrees of freedom.. If feature not wanted take mbloc=n.
c
c iwk     = integer. The declared lengths of arrays alu and jlu
c           if iwk is not large enough the code will stop prematurely
c           with ierr = -2 or ierr = -3 (see below).
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
c iperm   = contains the permutation arrays ..
c           iperm(1:n) = old numbers of unknowns
c           iperm(n+1:2*n) = reverse permutation = new unknowns.
c
c ierr    = integer. Error message with the following meaning.
c           ierr  = 0    --> successful return.
c           ierr .gt. 0  --> zero pivot encountered at step number ierr.
c           ierr  = -1   --> Error. input matrix may be wrong.
c                            (The elimination process has generated a
c                            row in L or U whose length is .gt.  n.)
c           ierr  = -2   --> The L/U matrix overflows the arrays alu,jlu
c           ierr  = -3   --> zero row encountered.
c
c work arrays:
c=============
c jw      = integer work array of length 2*n.
c w       = real work array of length 2*n 
c
c Notes:
c ------
c IMPORTANT: TO AVOID PERMUTING THE SOLUTION VECTORS ARRAYS FOR EACH 
c LU-SOLVE, THE MATRIX A IS PERMUTED ON RETURN. [all column indices are
c changed]. SIMILARLY FOR THE U MATRIX. 
c To permute the matrix back to its original state use the loop:
c
c      do k=ia(1), ia(n+1)-1
c         ja(k) = perm(ja(k)) 
c      enddo
c 
c-----------------------------------------------------------------------
c     local variables
c
      integer k,i,j,jrow,ju0,ii,j1,j2,jpos,len,imax,lenu,lenl,jj,icut
      real*8 s,tmp,tnorm,xmax,xmax0,fact,abs,t,dropsum 
c----------------------------------------------------------------------- 
c     initialize ju0 (points to next element to be added to alu,jlu)
c     and pointer array.
c-----------------------------------------------------------------------
      ju0 = n+2
      jlu(1) = ju0
c
c  integer double pointer array.
c
      do 1 j=1,n
         jw(n+j)  = 0
         iperm(j) = j
         iperm(n+j) = j
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
            tnorm = tnorm+abs(a(k))
 501     continue
         if (tnorm .eq. 0.0) goto 997
         tnorm = tnorm/(j2-j1+1)
c
c     unpack L-part and U-part of row of A in arrays  w  --
c
         lenu = 1
         lenl = 0
         jw(ii) = ii
         w(ii) = 0.0
         jw(n+ii) = ii
c
         do 170  j = j1, j2
            k = iperm(n+ja(j))
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
c     zero out element in row by resetting jw(n+jrow) to zero.
c     
         jw(n+jrow) = 0
c
c     drop term if small
c     
         if (abs(w(jj)) .le. droptol*tnorm) then
            dropsum = dropsum + w(jj) 
            goto 150
         endif      
c
c     get the multiplier for row to be eliminated: jrow
c
         fact = w(jj)*alu(jrow)
c
c     combine current row and row jrow
c
         do 203 k = ju(jrow), jlu(jrow+1)-1
            s = fact*alu(k)
c     new column number
            j = iperm(n+jlu(k))
            jpos = jw(n+j)
c
c     if fill-in element is small then disregard:
c     
            if (j .ge. ii) then
c
c     dealing with upper part.
c
               if (jpos .eq. 0) then
c     this is a fill-in element
                  lenu = lenu+1
                  i = ii+lenu-1 
                  if (lenu .gt. n) goto 995
                  jw(i) = j
                  jw(n+j) = i 
                  w(i) = - s
               else
c     no fill-in element --
                  w(jpos) = w(jpos) - s
               endif
            else
c
c     dealing with lower part.
c
               if (jpos .eq. 0) then
c     this is a fill-in element
                 lenl = lenl+1
                 if (lenl .gt. n) goto 995
                 jw(lenl) = j
                 jw(n+j) = lenl
                 w(lenl) = - s
              else
c     no fill-in element --
                 w(jpos) = w(jpos) - s
              endif
           endif
 203    continue
        len = len+1 
        w(len) = fact
        jw(len)  = jrow
        goto 150
 160    continue
c
c     reset double-pointer to zero (U-part)
c     
        do 308 k=1, lenu
           jw(n+jw(ii+k-1)) = 0
 308    continue
c
c     update L-matrix
c
        do 204 k=1, len
           if (ju0 .gt. iwk) goto 996
           alu(ju0) =  w(k)
           jlu(ju0) = iperm(jw(k))
           ju0 = ju0+1
 204    continue
c
c     save pointer to beginning of row ii of U
c
        ju(ii) = ju0
c
c     update u-matrix -- first apply dropping strategy 
c
         len = 0
         do 205 k=1, lenu-1
            if (abs(w(ii+k)) .gt. tnorm*droptol) then 
               len = len+1
               w(ii+len) = w(ii+k) 
               jw(ii+len) = jw(ii+k) 
            else
               dropsum = dropsum + w(ii+k) 
            endif
205      continue
c
        imax = ii
        xmax = abs(w(imax))
        xmax0 = xmax
        icut = ii - 1 + mbloc - mod(ii-1,mbloc)
c
c     determine next pivot -- 
c 
        do 208 k=ii+1,ii+len 
           t = abs(w(k))
           if (t .gt. xmax .and. t*permtol .gt. xmax0 .and.
     *          jw(k) .le. icut) then
              imax = k
              xmax = t
           endif
208     continue
c
c     exchange w's
c
        tmp = w(ii)
        w(ii) = w(imax)
        w(imax) = tmp
c
c     update iperm and reverse iperm
c
        j = jw(imax)
        i = iperm(ii)
        iperm(ii) = iperm(j)
        iperm(j) = i
c     reverse iperm
        iperm(n+iperm(ii)) = ii
        iperm(n+iperm(j)) = j
c----------------------------------------------------------------------- 
        if (len + ju0-1 .gt. iwk) goto 996
c
c     copy U-part in original coordinates
c     
        do 302 k=ii+1,ii+len
           jlu(ju0) = iperm(jw(k))
           alu(ju0) = w(k)
           ju0 = ju0+1
 302    continue
c
c     define diagonal element 
c 
         w(ii) = w(ii) + alph*dropsum 
c
c     store inverse of diagonal element of u
c
        if (w(ii) .eq. 0.0) w(ii) = (1.0D-4 + droptol)*tnorm
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
c
c     permute all column indices of LU ...
c
      do 503 k = jlu(1),jlu(n+1)-1
         jlu(k) = iperm(n+jlu(k))
503   continue
c
c     ...and of A
c
      do 504 k=ia(1), ia(n+1)-1
         ja(k) = iperm(n+ja(k))
504   continue
c
      ierr = 0
      return
c
c     incomprehensible error. Matrix must be wrong.
c
 995  ierr = -1
      return
c
c     insufficient storage in arrays alu, jlu to store factors
c
 996  ierr = -2
      return
c
c     zero row encountered
c
 997  ierr = -3 
      return
c----------------end-of-iludp-------------------------------------------
c-----------------------------------------------------------------------
      end
