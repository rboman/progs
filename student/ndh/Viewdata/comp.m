Tm=[]; Tm1=[];

c_10x10;
Tm=[Tm Tmin];
c_10x20;
Tm=[Tm Tmin];
c_10x30;
Tm=[Tm Tmin];
c_10x50;
Tm=[Tm Tmin];
Tm1=Tm;

Tm=[];
c_20x10;
Tm=[Tm Tmin];
c_20x20;
Tm=[Tm Tmin];
c_20x30;
Tm=[Tm Tmin];
c_20x50;
Tm=[Tm Tmin];
Tm1=[Tm1; Tm];

Tm=[];
c_40x10;
Tm=[Tm Tmin];
c_40x20;
Tm=[Tm Tmin];
c_40x30;
Tm=[Tm Tmin];
c_40x50;
Tm=[Tm Tmin];
Tm1=[Tm1; Tm];

Tm=[];
c_70x10;
Tm=[Tm Tmin];
c_70x20;
Tm=[Tm Tmin];
c_70x30;
Tm=[Tm Tmin];
c_70x50;
Tm=[Tm Tmin];
Tm1=[Tm1; Tm];

c_exact;
Tm1=(-Tm1+Tmin)/Tmin;
plot([10 20 30 50],Tm1','k',[10 20 30 50],Tm1','ko')
xlabel('Nombre de pas d''intégration (istep)')
title('Erreur relative de la solution au centre de la plaque')
