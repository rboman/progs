Tm=[]; cp=[];
cn_5;
Tm=[Tm Tmin];cp=[cp cpu];
cn_10;
Tm=[Tm Tmin];cp=[cp cpu];
cn_15;
Tm=[Tm Tmin];cp=[cp cpu];
cn_20;
Tm=[Tm Tmin];cp=[cp cpu];
cn_25;
Tm=[Tm Tmin]; cp=[cp cpu];
cn_30;
Tm=[Tm Tmin];cp=[cp cpu];
cn_35;
Tm=[Tm Tmin];cp=[cp cpu];
cn_40;
Tm=[Tm Tmin];cp=[cp cpu];
cn_45;
Tm=[Tm Tmin];cp=[cp cpu];
cn_50;
Tm=[Tm Tmin];cp=[cp cpu];
cn_55;
Tm=[Tm Tmin];cp=[cp cpu];
cn_60;
Tm=[Tm Tmin];cp=[cp cpu];
cn_65;
Tm=[Tm Tmin];cp=[cp cpu];
c_exact;
Tm=(-Tm+Tmin)/Tmin;
plot(10:5:65,Tm(2:13),'k',10:5:65,Tm(2:13),'ko')
xlabel('Nombre d''éléments (N)')
title('Erreur relative de la solution au centre de la plaque')

%figure, plot(5:5:65, cp,'k')