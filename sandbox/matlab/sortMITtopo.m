%
% tri d'une topo MIT 2D dans le desordre en fct d'un contour
%

wire = [10,12,5,8,15,22,14,7,3,1,9];

topo ={ 
 [22,15,8],
 [5,12],
 [1,10,9],
 [7,14,3] };

nc = size(wire,2);
grps=zeros(1,nc);
for i=1:nc,
    c = wire(i);
    for k=1:size(topo,1)
        topok = topo{k};
        for l=1:size(topok,2)
            if topok(l) == c
                grps(i) = k;
                break
            end
        end
        if grps(i)~=0
            break
        end
    end
end
grps;

tmod={[],[],[],[],[]};

nng=1;
rev=0;
oldng=grps(1);
for i=1:nc
   ng = grps(i);
   if ng==0
       disp('error')
   end
   if ng ~= oldng
       oldng = ng;
       nng=nng+1;
   end
   tmod{nng} = [ tmod{nng} wire(i) ];
end
tmod{1} = [ tmod{5} tmod{1}];

wire
disp(tmod{1})
disp(tmod{2})
disp(tmod{3})
disp(tmod{4})

   


