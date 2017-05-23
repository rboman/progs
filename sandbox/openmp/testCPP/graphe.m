clear all; close all

load OMPTester.txt; A=OMPTester;

m=max(A(:,1));
n=max(A(:,2));
siz(m,n)=0;
nbt(m,n)=0;
mem(m,n)=0;
cpu(m,n)=0;
flops(m,n)=0;

for i=1:size(A,1)
    idx1=A(i,1);
    idx2=A(i,2);
    siz(idx1,idx2)=A(i,3);
    nbt(idx1,idx2)=A(i,4);
    mem(idx1,idx2)=A(i,5);
    cpu(idx1,idx2)=A(i,6);
    flops(idx1,idx2)=A(i,7);
end

speedup(m,n)=0;
pareff(m,n)=0;

for i=1:m
    for j=1:n
        speedup(i,j) = cpu(i,1)/cpu(i,j);
        pareff(i,j) = speedup(i,j)/nbt(i,j);
    end
end


figure(1)
hold on
legtxt=[];
for i=1:m
    h = plot(mem(:,i),flops(:,i)./cpu(:,i)/1024/1024,'o-');
    leg1 = [num2str(nbt(1,i)) ' thread'];    
    if(i~=1)
        leg1 = [leg1 's'];
    end
    set(h,'DisplayName',leg1);
end
set(gca,'XScale','log')
xlabel('Problem size [Mb]')
ylabel('Mflops/s')
legend(gca,'show');
grid
hold off

figure(2)
plot(nbt',speedup')
xlabel('Nb of threads')
ylabel('Speedup')
grid

figure(3)
semilogx(mem,pareff)
xlabel('Problem size')
ylabel('Parallel efficiency')
grid
