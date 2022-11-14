# F1 Kandidatuppsats
Samling av dokument för kandidatuppsats
Här har vi samling av de dokument vi behöver för kandidatuppsats.

Samling av modellen när de blivit simulerade kommer ligga i mappen 'fit'.

R filer: \
01: Data cleaning / data inhämtning / skriver rds fil för kval \
\
02: Data cleaning / data inhämtning / skriver rds fil för kval \
\
03: Bearbetning av data, beräkning av position_prop, grid_prop för kval och race, skriver sedan rds med bearbetade resultatlistor \
\
04: Modellerna i BRMS, här modellerna körs samt skriver RDS med givna simuleringar. Både kval och race. \
\
05: Jämför modellerna och ser viklen som är bäst. Här skrivs RDS filer med LOO resultat vilka kommer användas senare. Även skapandet av Bayes_grid plottar av bästa modellen/modellerna. \
\
06: Baserat på modellen vilken kommer användas (Som har bäst LOO i föregående fil (05_jamfor) ) skrivs RDS-fil av summering av simuleringarna och även alla kedjornas simuleringar för kvalmodellen. \
\
07: Baserat på modellen vilken kommer användas (Som har bäst LOO i föregående fil (05_jamfor) ) skrivs RDS-fil av summering av simuleringarna och även alla kedjornas simuleringar för racemodellen. \
\
08: Kontroll av prediktiv förmåga av modell genom visualisering, Kval. \
\
09: Kontroll av prediktiv förmåga av modell genom visualisering, Race. \
\
10: Inferens på modellen för kval. \
\
11: Inferens på modellen för race. \
\
12: Sortering av data samt plottning Kval (!kolla denna) \
\
13: Sortering av data samt plottning Race (!Kolla denna) \
\
14: Marginal effect av startposition på slutposition, beräkning (Dubbelkolla denna då använt metod vilken man gör på logistisk regression, är det samma som beta-regression?) \
\
15: Beräkning av sammansatt ranking samt plottning av detta, förare. \
\
16: Plottning av estimat över tid (Kanske om orkar justera för detta då vi kan se att det är många på tidiga årtal som får bra ranking). \
\
17: Deskriptiv statistik över datamaterialet. Mycket plottar. \
\
18: Justering för team-koefficienter, tidsserie av estimaten. (? Skall ha med och kanske justera för detta ? )
