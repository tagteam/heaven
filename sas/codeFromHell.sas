data pop;
 input pnr recno inddto uddto pattype;
cards;
1	1	10	25	0
1       29      12      14      0 /*Indskudt indlæggelse*/
1       30      25      26      0 /* Fortsat indlæggelse*/
1	2	30	420	2
1	4	1045	1051	0
2	3	100	110	0
2       31      105     105     0 /*Indskudt indlæggelse*/
3	5	214	222	0
4	6	190	195	0
4       32      195     197     0 /*Fortsat indlæggelse*/
4	7	75	85	0
5	8	261	270	0
6	9	33	40	0
6	10	314	321	0
7	11	61	69	0
8	12	89	97	0
8	13	54	65	2
9	14	102	110	0
10	15	200	204	0
10	16	221	222	0
11	17	121	145	0
12	18	190	194	0
13	19	21	27	0
13	20	31	34	0
14	21	166	171	0
15	22	121	154	0
15	23	199	208	0
16	24	157	165	0
17	25	122	127	0
18	26	223	230	0
19	27	304	324	0
20	28	267	272	0
;
%Macro codehell3(inddata,uddata);
 /******************************************************
  Inddata er data fra lpr med diagnoser og inddto/uddto
  Alle indlæggelser fra et forløb - overlappende intervaller
  samles til en indlæggelse igen defineret ved inddto/uddto
  Der tages ikke højde for afdelinger og specialer der er fjernet.
  Hvis de inkluderes bliver det systematisk fra sidste indlæggelse
  diagnosen fra hver forløb inkluderes som variabel

  Uddata er det komprimerede datasæt - kan være identisk med inddata  
*******************************************************/
 proc sort data=&inddata; by pnr inddto; run;  data &uddata; set &inddata;
  by pnr;
  retain ind2 ud2 pttype;
  drop inddto uddto afd sgh spec recnum;
  rename ind2=inddto ud2=uddto;  
   *Er vi på den første record af en patient.  Der skal IKKE outputtes, blot sættes gemmeværdier af datoer;
   if first.pnr then
    do;
     ind2=inddto; ud2=uddto; 
	 pttype=10;
     end;
	if pattype=0 then pttype=0;
   *Specialekoder;
  *Er vi startet på et nyt forløb og skal outputte det forrige;
   if inddto>ud2 and ud2 ne . then 
    do;
     output; 
     ind2=inddto; ud2=uddto;
    end;
  *Er der overlap mellem record defineret ved inddto/uddto og ind2/ud2 - så justeres datoer;
   if inddto<=ud2 and inddto ne . then ud2=max(ud2,uddto);
   if last.pnr then output;
  format inddto uddto ind2 ud2 date8.;
 run;
%mend;

%codehell3(pop,pop2);



