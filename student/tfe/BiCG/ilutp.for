c----------------------------------------------------------------------
      subroutine ilutp(n,a,ja,ia,lfil,droptol,permtol,mbloc,alu,
     *     jlu,ju,iwk,w,jw,iperm,ierr)
c-----------------------------------------------------------------------
c     implicit none
      integer n,ja(*),ia(n+1),lfil,jlu(*),ju(n),jw(2*n),iwk,
     *     iperm(2*n),ierr
      real*8 a(*), alu(*), w(n), droptol
c----------------------------------------------------------------------*
c       *** ILUTP preconditioner -- ILUT with pivoting  ***            *
c      incomplete LU factorization with dual truncation mechanism      *
c----------------------------------------------------------------------*
c author Yousef Saad *Sep 8, 1993 -- Latest revision, August 1996.     *
c----------------------------------------------------------------------*
c on entry:
c==========
c n       = integer. The dimension of the matrix A.
c
c a,ja,ia = matrix stored in Compressed Sparse Row format.
c           ON RETURN THE COLUMNS OF A ARE PERMUTED. SEE BELOW FOR 
c           DETAILS. 
c
c lfil    = integer. The fill-in parameter. Each row of L and each row
c           of U will have a maximum of lfil elements (excluding the 
c           diagonal element). lfil must be .ge. 0.
c           ** WARNING: THE MEANING OF LFIL HAS CHANGED WITH RESPECT TO
c           EARLIER VERSIONS. 
c
c droptol = real*8. Sets the threshold for dropping small terms in the
c           factorization. See below for details on dropping strategy.
c
c lfil    = integer. The fill-in parameter. Each row of L and
c           each row of U will have a maximum of lfil elements.
c           WARNING: THE MEANING OF LFIL HAS CHANGED WITH RESPECT TO
c           EARLIER VERSIONS. 
c           lfil must be .ge. 0.
c
c permtol = tolerance ratio used to  determne whether or not to permute
c           two columns.  At step i columns i and j are permuted when 
c
c                     abs(a(i,j))*permtol .gt. abs(a(i,i))
c
c           [0 --> never permute; good values 0.1 to 0.01]
c
c mbloc   = if desired, permuting can be done only within the diagonal
c           blocks of size mbloc. Useful for PDE problems with several
c           degrees of freedom.. If feature not wanted take mbloc=n.
c
c  
c iwk     = integer. The lengths of arrays alu and jlu. If the arrays
c           are not big enough to store the ILU factorizations, ilut
c           will stop with an error message. 
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
c iperm   = contains the permutation arrays. 
c           iperm(1:n) = old numbers of unknowns
c           iperm(n+1:2*n) = reverse permutation = new unknowns.
c
c ierr    = integer. Error message with the following meaning.
c           ierr  = 0    --> successful return.
c           ierr .gt. 0  --> zero pivot encountered at step number ierr.
c           ierr  = -1   --> Error. input matrix may be wrong.
c                            (The elimination process has generated a
c                            row in L or U whose length is .gt.  n.)
c           ierr  = -2   --> The matrix L overflows the array al.
c           ierr  = -3   --> The matrix U overflows the array alu.
c           ierr  = -4   --> Illegal value for lfil.
c           ierr  = -5   --> zero row encountered.
c
c work arrays:
c=============
c jw      = integer work array of length 2*n.
c w       = real work array of length n 
c
c IMPORTANR NOTE:
c --------------
c TO AVOID PERMUTING THE SOLUTION VECTORS ARRAYS FOR EACH LU-SOLVE, 
C THE MATRIX A IS PERMUTED ON RETURN. [all column indices are
c changed]. SIMILARLY FOR THE U MATRIX. 
c To permute the matrix back to its original state use the loop:
c
c      do k=ia(1), ia(n+1)-1
c         ja(k) = iperm(ja(k)) 
c      enddo
c 
c-----------------------------------------------------------------------
c     local variables
c
      integer k,i,j,jrow,ju0,ii,j1,j2,jpos,len,imax,lenu,lenl,jj,mbloc,
     *     icut
      real*8 s, tmp, tnorm,xmax,xmax0, fact, abs, t, permtol
c     
      if (lfil .lt. 0) goto 998
c----------------------------------------------------------------------- 
c     initialize ju0 (points to next element to be added to alu,jlu)
c     and pointer array.
c-----------------------------------------------------------------------
      ju0 = n+2
      jlu(1) = ju0
c
c  integer double pointer array.
c
      do 1 j=1, n
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
         tnorm = 0.0d0
         do 501 k=j1,j2
            tnorm = tnorm+abs(a(k))
 501     continue
         if (tnorm .eq. 0.0) goto 999
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
c     get the multiplier for row to be eliminated: jrow
c
         fact = w(jj)*alu(jrow)
c
c     drop term if small
c     
         if (abs(fact) .le. droptol) goto 150
c
c     combine current row and row jrow
c
         do 203 k = ju(jrow), jlu(jrow+1)-1
            s = fact*alu(k)
c     new column number
            j = iperm(n+jlu(k))
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
 203	continue
c     
c     store this pivot element -- (from left to right -- no danger of
c     overlap with the working elements in L (pivots). 
c     
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
 308	continue
c
c     update L-matrix
c
        lenl = len 
        len = min0(lenl,lfil)
c     
c     sort by quick-split
c
        if(lenl.GT.0) then
           call qsplit (w,jw,lenl,len)
        endif
c
c     store L-part -- in original coordinates ..
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
c     update U-matrix -- first apply dropping strategy 
c
         len = 0
         do 205 k=1, lenu-1
            if (abs(w(ii+k)) .gt. droptol*tnorm) then 
               len = len+1
               w(ii+len) = w(ii+k) 
               jw(ii+len) = jw(ii+k) 
            endif
205      continue
         lenu = len+1
         len = min0(lenu,lfil)

         if((lenu-1).GT.0) then
            call qsplit (w(ii+1), jw(ii+1), lenu-1,len)
         endif
c
c     determine next pivot -- 
c
        imax = ii
        xmax = abs(w(imax))
        xmax0 = xmax
        icut = ii - 1 + mbloc - mod(ii-1,mbloc)
        do 206 k=ii+1,ii+len-1
           t = abs(w(k))
           if (t .gt. xmax .and. t*permtol .gt. xmax0 .and.
     *          jw(k) .le. icut) then
              imax = k
              xmax = t
           endif
206     continue
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
c
c     reverse iperm
c
        iperm(n+iperm(ii)) = ii
        iperm(n+iperm(j)) = j
c----------------------------------------------------------------------- 
c
        if (len + ju0 .gt. iwk) goto 997
c
c     copy U-part in original coordinates
c     
        do 302 k=ii+1,ii+len-1 
           jlu(ju0) = iperm(jw(k))
           alu(ju0) = w(k)
           ju0 = ju0+1
 302	continue
c
c     store inverse of diagonal element of u
c
        if (w(ii) .eq. 0.0) w(ii) = (1.0D-4 + droptol)*tnorm
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
      do 502 k=ia(1), ia(n+1)-1
         ja(k) = iperm(n+ja(k))
502   continue
c
      ierr = 0
      return
c
c     incomprehensible error. Matrix must be wrong.
c
 995  ierr = -1
      return
c
c     insufficient storage in L.
c
 996  ierr = -2
      return
c
c     insufficient storage in U.
c
 997  ierr = -3
      return
c
c     illegal lfil entered.
c
 998  ierr = -4
      return
c
c     zero row encountered
c
 999  ierr = -5
      return
c----------------end-of-ilutp-------------------------------------------
c-----------------------------------------------------------------------
      end
