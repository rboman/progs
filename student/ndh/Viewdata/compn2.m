Tm=[]; cp=[];
can_12;
Tm=[Tm Tmin];cp=[cp cpu];
can_20;
Tm=[Tm Tmin];cp=[cp cpu];
can_28;
Tm=[Tm Tmin];cp=[cp cpu];
can_36;
Tm=[Tm Tmin];cp=[cp cpu];
can_44;
Tm=[Tm Tmin]; cp=[cp cpu];
can_52;
Tm=[Tm Tmin];cp=[cp cpu];
can_60;
Tm=[Tm Tmin];cp=[cp cpu];
ca_exact;
Tm=(Tm-Tmin)/Tmin;
plot(12:8:60,Tm,'k',12:8:60,Tm,'ko')
xlabel('Nombre d''éléments (N)')
title('Erreur relative de la solution au centre de la plaque')

%figure, plot(12:8:60, cp)