
c
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	subroutine lltsol(n, y, x, alu, jlu, ju)
c
        implicit real*8(a-h,o-z)
c
        integer n, jlu(*), ju(*)
        real*8 x(n), y(n), alu(*)
	
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c local variables
c
        integer i,k
c
c forward solve (with L)
c
        do 40 i = 1, n
           sum = 0.0D0
           do 41 k=ju(i),ju(i+1)-1
              sum = sum + alu(k) * x(jlu(k))
 41        continue
           x(i) = (y(i) - sum) * alu(i)
 40     continue
c     
c backward solve (with L^T)
c     
	do 50 i = n,1,-1
	   do 51 k=ju(i),ju(i+1)-1
              x(jlu(k)) = x(jlu(k)) - alu(k) * x(i) *alu(i)
 51        continue
           x(i) = x(i) * alu(i)
 50     continue
c
c
 100 	return
c----------------end of lltsol ------------------------------------------
c-----------------------------------------------------------------------
	end

c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++