c-----------------------------------------------------------------------
        subroutine lusol(n, y, x, alu, jlu, ju)
        integer n, jlu(*), ju(*)
        real*8 x(n), y(n), alu(*)
        
c-----------------------------------------------------------------------
c
c This routine solves the system (LU) x = y, 
c given an LU decomposition of a matrix stored in (alu, jlu, ju) 
c modified sparse row format 
c
c-----------------------------------------------------------------------
c on entry:
c n   = dimension of system 
c y   = the right-hand-side vector
c alu, jlu, ju 
c     = the LU matrix as provided from the ILU routines. 
c
c on return
c x   = solution of LU x = y.     
c-----------------------------------------------------------------------
c 
c Note: routine is in place: call lusol (n, x, x, alu, jlu, ju) 
c       will solve the system with rhs x and overwrite the result on x . 
c
c-----------------------------------------------------------------------
c local variables
c
        integer i,k
c
c forward solve
c
        do 40 i = 1, n
           x(i) = y(i)
           do 41 k=jlu(i),ju(i)-1
              x(i) = x(i) - alu(k)* x(jlu(k))
 41        continue
 40     continue
c
c     backward solve.
c
        do 90 i = n, 1, -1
           do 91 k=ju(i),jlu(i+1)-1
              x(i) = x(i) - alu(k)*x(jlu(k))
 91        continue
           x(i) = alu(i)*x(i)
 90     continue
c
        return
c----------------end of lusol ------------------------------------------
c-----------------------------------------------------------------------
        end

c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

c-----------------------------------------------------------------------
        subroutine lutsol(n, y, x, alu, jlu, ju) 
        integer n, jlu(*), ju(*)
        real*8 x(n), y(n), alu(*)
        
c-----------------------------------------------------------------------
c
c This routine solves the system  Transp(LU) x = y,
c given an LU decomposition of a matrix stored in (alu, jlu, ju) 
c modified sparse row format. Transp(M) is the transpose of M. 
c----------------------------------------------------------------------- 
c on entry:
c n   = dimension of system 
c y   = the right-hand-side vector
c alu, jlu, ju 
c     = the LU matrix as provided from the ILU routines. 
c
c on return
c x   = solution of transp(LU) x = y.   
c-----------------------------------------------------------------------
c
c Note: routine is in place: call lutsol (n, x, x, alu, jlu, ju) 
c       will solve the system with rhs x and overwrite the result on x . 
c 
c-----------------------------------------------------------------------
c local variables
c
        integer i,k
c
        do 10 i = 1, n
           x(i) = y(i)
 10     continue
c
c forward solve (with U^T)
c
        do 20 i = 1, n
           x(i) = x(i) * alu(i)
           do 30 k=ju(i),jlu(i+1)-1
              x(jlu(k)) = x(jlu(k)) - alu(k)* x(i)
 30        continue
 20     continue
c     
c     backward solve (with L^T)
c     
        do 40 i = n, 1, -1 
           do 50 k=jlu(i),ju(i)-1
              x(jlu(k)) = x(jlu(k)) - alu(k)*x(i)
 50        continue
 40     continue
c
        return
c----------------end of lutsol -----------------------------------------
c-----------------------------------------------------------------------
        end

c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

c----------------------------------------------------------------------- 
        subroutine qsplit(a,ind,n,ncut)
        integer n,ind(n), ncut
        real*8 a(n)
c-----------------------------------------------------------------------
c     does a quick-sort split of a real array.
c     on input a(1:n). is a real array
c     on output a(1:n) is permuted such that its elements satisfy:
c
c     abs(a(i)) .ge. abs(a(ncut)) for i .lt. ncut and
c     abs(a(i)) .le. abs(a(ncut)) for i .gt. ncut
c
c     ind(1:n) is an integer array which permuted in the same way as a(*).
c-----------------------------------------------------------------------
        real*8 tmp, abskey
        integer itmp, first, last
c-----
        first = 1
        last = n
        if (ncut .lt. first .or. ncut .gt. last) return
c
c     outer loop -- while mid .ne. ncut do
c
 1      mid = first
        abskey = abs(a(mid))
        do 2 j=first+1, last
           if (abs(a(j)) .gt. abskey) then
              mid = mid+1
c     interchange
              tmp = a(mid)
              itmp = ind(mid)
              a(mid) = a(j)
              ind(mid) = ind(j)
              a(j)  = tmp
              ind(j) = itmp
           endif
 2      continue
c
c     interchange
c
        tmp = a(mid)
        a(mid) = a(first)
        a(first)  = tmp
c
        itmp = ind(mid)
        ind(mid) = ind(first)
        ind(first) = itmp
c
c     test for while loop
c
        if (mid .eq. ncut) return
        if (mid .gt. ncut) then
           last = mid-1
        else
           first = mid+1
        endif
        goto 1
c----------------end-of-qsplit------------------------------------------
c-----------------------------------------------------------------------
        end
