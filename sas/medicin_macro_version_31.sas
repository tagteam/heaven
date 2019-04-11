
/**********************************************************
Macro til udregning af tabel over medicinering gennem en interesseperiode - version 31 - 29. oktober 2011

CAVE: Denne makro må ikke anvendes i blinde!!!!! Makroen er helt afhængig af forudsætninger som puttes i den. Variationer valg af typisk dosis samt mindste
 og maksimale dosis. Det er derfor imperativt at man manuelt sammenligner recepter og behandlingsperioder for en række cases for at sikre at makroen fungerer 
 hensigtsmæssigt. Herunder er det vigtigt at huske at "typisk dosis" også er obligatorisk startdosis når man vælger "left only". I dette tilfælde kan det
 være passende at vælge en mindre "typisk dosis".

Komprimeret output i sidste version

Grundprincippet er at lægge et antal recepter fra før og efter hver recept på række
og benytte den samlede information til at beregne dosis for "interesserecepten" i midten
Hvor mange recepter der benyttes i beregningen fremgår af "antal_recepter" der angives som
variabel til makroen.

Der kan aktuelt være op til 4 forskellige pillestørrelser til en behandling (dos1-4) og for
hver af disse skal der angives hvilken daglige minimumsdosis (min_dos1-4) og maximumdosis (max_dox1-4)
som dette svarer til. Endeligt angives en typisk dosis (typ_dos) som er default.

Beregningen gennemføres for et af brugen valgt datointerval mellem firstdato og lastdato.

Variablen trace angiver som der skal skrives trackinginformation i loggen og gemmes temporære datasæt. Denne
facilitet er til fejlretning.

I kontinuert behandling antages det at en ændring i tilsyneladende dosis på en faktor 2 repræsenterer et reelt spring i
dosis


Beregningsregler:
1. Hvis en recept er alene forbruges "typisk" dosis til der ikke er flere piller.
2. Hvis en recept er del af en sekvens beregnes en middelværdi der så vidt muligt sikrer kontinuerlig behandling ved
   valg af doser mellem min og max
3. Hvis en pille er den sidste i en kontinuerlig sekvens af samme dosis fortsættes med middeldosis (den mulige dosis som er tættest på denne)
   til der ikke er flere piller
4. Hvis der er dosisskift beregnes middelværdi blandt sammenhængende recepter og der vælges en dosis som er typisk dosis
   hvis denne passer med pillerne, max/min hvis den beregnede middelværdi i perioden over under disse og middeldosis hvis det passer
   nogenlunde med udleveringen.
5. Hvis der er dosisskift og udleveringen er den sidste i en sekvens der hænger sammen vælges en dosis der er så tæt som muligt på middeldosis.

Programmets generelle opbygning:

Først genereres et datasæt med relevante recepter og relevante variable. Samtlige indlæggelser påhæftes.
Svarende til antallet af recepter som ønskes indgået i beregningerne generes et sæt ens datasæt som samles forskudt
 således at hver record har det ønskede antal recepter ved siden af hinanden.  Den midterste recept er interesse-
 recepten som der beregnes dosering for
Herefter beregnes et antal hjælpevariable og helt central gennemføres en beregning af hvorvidt recepterne ved siden
 af hinanden har noget med hinanden at gøre. Yderligere beregnes antallet af INDLÆGGELSES data MELLEM to recepter som bruges
 til at antage gratis medicin under indlæggelse. Der udføres IKKE en sådan justering EFTER sidste recept.
Herefter beregnes dosering i den midterste receptperiode udefra 5 situationer
Endeligt genereres EN record pr. patient med alle BEHANDLINGSPERIODER i interesseintervallet. Hele intervallet er dækket
 og i behandlingsfri perioder er dosen 0.


Om at vurdere resultatet:

1. Med plot genereres en graf over middeldosis gennem perioden samt en graf over procent i behandling
2. Datasættet &output_data._alt - altså navnet på output-data efterfulgt af "_alt" laver en tabel med en record pr. behandlingsperiode og
   mange records pr. patient. Den er noget lettere at læse en &output_data
3. De sidste variable "ude til højre" i datasættet temp indeholder oplysninger om hver recept som er læselige

4. I LANG TID FREMOVER ER DET HELT KRITISK AT MAN GENNEMGÅR ET PÆNT ANTAL PATIENTER OG SIKRER SIG AT DER ER BEREGNET KORREKT FOR DISSE.
   DETTE SIKRER MULIGVIS IKKE MOD ALLE FEJL, MEN DOG MOD DE GROVE

Seneste justeringer:
Lille fejl rettet i situation 5
Tilføjelse af beregninger af datoer/doser som KUN kigger bagud
21.12.08 - Minder om at doser skal være i rækkefølge!
21.1.8 - Tilretter beregning af "left_only" så fejl noteret december undgås
8.10.11 - Ved left-only startedes ved mindste dosis - dette rettet
26.10.11 - Ved left-only beregnes linkdos_low altid med mindste dosis for at undgå ureglementerede huller
29.10.11 - kun udregning i perioder med ægte recepter - uden betydning.
  Tilføjelse af variabel max_depot


********************************************************************/



%macro x_recepter(recept_data, /* forventes at indeholde variable - skulle gerne passe med DST-standarder:
                                  pnr - cpr/patientidentifikation
                                  atc - ATC kode
                                  eksd - udleveringsdato som sas-dato
                                  strnum - numerisk styrke
                                  apk - antal udleverede pakker
                                  packsize - antal piller i hver pakke*/
 
                  datoer, /* Et produkt af medicin-hjælpe-macro eller andet program som ordner ALLE indlæggelser pr PNR på
				          EEN record med fortløbende inddto uddto */


                  output_data, /* tabel over behandlingsperioder - navn på SAS datasæt valgt af brugeren*/
                  behandling, /* atc kode - den behandling som der skal beregnes på*/
                  antal_recepter, /* antal recepter der indgår i beregninger - testet med 5, altså op til 2 før og 2 efter interesserecept */
                  dos1, dos2, dos3, dos4, /* Doser svarende til de følgende variable - det er pillestørrelser
                         - her og de følgende variable skal ALLE have en værdi. Hvis der findes færre skal der blot gentages*/
                  min_dos1, min_dos2, min_dos3, min_dos4,    /* Mindst accepterede dosis af lægemidler på hver pillestyrke*/
                  max_dos1, max_dos2, max_dos3, max_dos4,    /* Max accepterede dosis*/
                  typ_dos1, typ_dos2, typ_dos3, typ_dos4,    /* Typiske doser - en slags "default" dosis - og startdosis altid ved left_only */
				  max_depot, /* Maximum sktørrelse af "restdosis" som kan overføres til følgende receptperioder. Denne giver mulighed for
				   at forhindre excessiv ophobning hvis små antagelser om maxdosis medfører til tiltagende stort depot 
                   Max_depot er piller*styrke - Hvis der højst må gemmes 100 piller a 10 mg, så er max_depot 1000
				   */
                  firstdato, /* første og sidste dato som har interesse kan angives som en "SAS-dato" eller med konventionen
                     'ddmmyy'd     */
                  lastdato,
                  trace, /* Hvis værdien er 1 så kommer der tracking udskrift i loggen - hvis nul, så ikke. Tilsvarende slettes en række
                    temporære datasæt hvis værdien er 0 */
                  plot, /* Hvis værdien er 1 så kommer der grafer */
                  praefix, /* præfix på generede variable som kan benyttes til at skelne fra lignende variable genereret i andre trin*/
				  left_only /* danner tabeller "l_" hvor doser og sluttider KUN regnes bagud*/
                  );
 /*******************************************************************************************
  Datasættet temp isolerer de recepter som har den atc-kode der har interesse
  Der checkes for nogle åbenlyse fejl, og for åbenlyst dårligt input til makroen
 *******************************************************************************************/
                  options mprint;
 %let slemme_data='0'; /* forklaring - se første datastep*/

    data temp; set &recept_data;
    
/* if atc="&behandling" and eksd>=&firstdato and eksd<=&lastdato; /*kun den rette ATC kode og datointerval indgår */
 if atc="&behandling" and eksd>=12784 and eksd<=20089; /*kun den rette ATC kode og datointerval indgår */
  if eksd<0 or apk<0.0001 or strnum<0.000001 or packsize<0.5 then
   do;
    PUT "ADVARSEL - rådden recept - " pnr= eksd= apk= strnum= packsize=;
    call symput('slemme_data',trim(left(put(1,4.))));
   end;

   /* Det følgende checker om alle doser i databasen faktisk er angivet til makroen - makroen kan
      kun finde ud af det hvis alle doser er defineret med dosx min/max/typ */
   /* if strnum notin (&dos1, &dos2, &dos3, &dos4) then */
       if strnum not in(&dos1, &dos2, &dos3, &dos4) then
   do;
    call symput('slemme_data',trim(left(put(1,4.))));
    PUT "ADVARSEL - ikke alle doser defineret - se tabel i output";
   end;
/* if &antal_recepter notin (3,5,7,9,11) then */
    if &antal_recepter not in(3,5,7,9,11) then
   do;
    call symput('slemme_data',trim(left(put(1,4.))));
    put "Ulovligt antal recepter: &antal_recepter";
   end;
 run;
 data _null_;
  if not(&typ_dos1<= &typ_dos2<= &typ_dos3<= &typ_dos4) then
   do;
    put "Advarsel - typiske doser ikke ascenderende";
   end;
 run;

 %if &slemme_data=1 %then
 %do;
  title1 'Slemme data - se tabeller - kig også i loggen';
  proc freq data=temp;
   tables strnum eksd apk strnum;
  run;
  title1;
 %end;
 %else
 %do; /* denne blok når enden af makroen*/

 /********************************************************************************************
  Ny i version 25 - recepter udleveret samme dag komprimeres til EN recept
 ********************************************************************************************/
  proc sort data=temp; by pnr eksd; run;
  data temp; set temp;
   by pnr eksd;
   retain antal_x sum_dosis sum_total;
   if first.eksd and not last.eksd then
     do;
      antal_x=0; sum_dosis=0; sum_total=0;
     end;
   if first.eksd=0 or last.eksd=0 then 
     do;
	  sum_total=sum_total+apk*packsize*strnum;
	  sum_dosis=sum_dosis+strnum;
	  antal_x=antal_x+1;
	  delete_maerket=1;
	 end;
   if last.eksd and not first.eksd then
     do;
	  apk=1;
	  strnum=sum_dosis/antal_x;
	  if &dos1<=strnum<&dos2 then strnum=&dos1;
	  if &dos2<=strnum<&dos3 then strnum=&dos2;
	  if &dos3<=strnum<&dos4 then strnum=&dos3;
	  if &dos4<=strnum then strnum=&dos4;
      packsize=round(sum_total/strnum);
	  delete_maerket=0;
	 end;
   run;
   data temp; set temp;
    drop sum_total sum_dosis antal_x delete_maerket;
	if delete_maerket=1 then delete;
   run;
	  




 /*******************************************************************************************
 I det følgende trin hæftes et "løbenummer" - loebenr - på hver recept. Dette gøres af hensyn
  til det følgende skridt. Løbenummer starter forfra ved 1 ved hver ny patient.
 *******************************************************************************************/
 proc sort data=temp; by pnr eksd; run;
 data temp; set temp;
  by pnr;
  retain loebenr;
  length loebenr 3;
  if first.pnr then loebenr=0;
  loebenr=loebenr+1; /* Giver et loebenr til recepterne på samme patient*/
  keep pnr atc eksd strnum apk packsize loebenr; /* Kun nødvendige data medtages*/
  format eksd date9.;
 run;


 /*************************************************************************************
 Der genereres datasæt svarende til antal_recepter.
 Konstruktionen med %let-kommandoen der definerer en macrovairbel x og dens anvendelse y skyldes:
 - Makroer evalueret ved kompilering, ikke når datasteppet kører, derfor er resultatet af "X" netop den følgende tekstkonstruktion
   som kun køres en gang og ikke resultatet af beregningen.
 - y genereres i hver loop og vil således generere et ny tal hver gang.

 eksd - ekspeditionsdato
 apk - antal pakker med piller
 strnum - pillestyrke numerisk
 packsize - pakkestørrelse

 Hver af de antal_recepter datasæt får altså nye variable svarende til ovennævnte receptvariable
 samt et nyt loebenr som vil variere fra 1-"midterreceptnummer" til antal_recepter-"midterreceptnummer", altså en
 symmetrisk række omkring midten

 **********************************************************************************/

  %do i=1 %to &antal_recepter;
   data temp_&i; set temp;
    %let x=&i-round(&antal_recepter/2,1);
    y=&x;
    rename eksd=eksd_&i
          apk=apk_&i
          strnum=strnum_&i
          packsize=packsize_&i;
    loebenr=loebenr-y;
   run;
  %end;

 /*********************************************************************
 Her samles de udvalgte recepter således at der ligger det valgte antal
 recepter før og efter interesserecepten på samme record

 Konstruktionen medfører at der også genereres records hvor interesserecepten
 i midten ikke eksisterer. Disse records fjernes i samme trin.

 *********************************************************************/
 data temp; merge %do i=1 %to &antal_recepter; temp_&i  %end; ; by pnr loebenr;
  array dataarray[&antal_recepter] eksd_1-eksd_&antal_recepter;
  if dataarray[round(&antal_recepter/2,1)]=. then delete; /* kun interesse hvis midterrecepten er relevant*/
  drop y loebenr;
 run;

 /*************************************************************************
  Hvis "trace" er slået til gemmes alle disse temporære datasæt, hvis ikke
  bliver de slettet for at spare plads for det tilfælde at makroen skal køre
  for rigtigt mange stoffer
 ***************************************************************************/

 %if not &trace  %then
 %do;
 proc datasets library=work;
  delete %do i=1 %to &antal_recepter; temp_&i  %end; ;
 run;
 %end;

 
 /*********************************************
   Indlæggelsesdatoer hæftes på recepter
 ********************************************/
 /* Find først hvor mange indlæggelser der KAN være */
 proc univariate data=&datoer noprint;;
  output out=max_indl max=max_indl;
  var max_indl;
 run;
 /* Værdien af max_indl skal bruges som macrovariabel og som det fremgår er det lidt
  nørdet at lægge den beregnede værdi af max i en macrovariabel
  Fidusen er at macro-step gennemføres når sas-programmet kompileres, ikke når det regner på
  data. Hvis man i stedet havde skrevet %let max_indl=max_indl - så ville makrovariabeln
  &max_indl indeholde værdien "max_indl" altså et stykke tekst. Konstruktionen nedenfor sørger
  for at værdien af makrovariablen bliver det relevante tal
 */
 Data max_indl; set max_indl;
  if max_indl=. or max_indl<1 then max_indl=1;
  call symput('max_indl',trim(left(put(max_indl,4.)))); ;
 run;

 proc sort data=temp; by pnr; run;
 data temp; merge temp (in=data) &datoer; by pnr; 
  if data;
 run;

 /****************************************************************************
  ****************************************************************************
  Herefter benyttes data med et antal recepter til at beregne doser i perioder

  Inputdata temp_m1 indeholder altså interesserecepten, et antal recepter før og efter
  samt alle indlæggelses og udksrivelsesdatoer for patienten

  center - midterreceptnummeret - interesserecepten
  eksd_ar[] array af udleveringsdatoer for recepterne
  apk_ar [] antal pakker der er udleveret
  packsize_ar[] antal piller i pakken
  strnum_ar[] styrken af pillerne
  inddto_ar[] indlæggelsesdatoer
  uddto_ar[] udskrivelsesdatoer
  dos_ar[] doser på piller i recepterne - strnum for de aktuelle recepter
  mindos_ar[] mindste accepterede dosis for recepterne
  maxdos_ar[] maximalt accepterede dosis for recepterne
  typdos_ar[] default doser for recepterne
  indltid_ar[] tid indlagt i hver receptperiode

  primdos_ar[] primitiv dosisudregning i hver receptperiode baseret på dosis og tid udskrevet - mellemregning
  receptdosis_ar[] - dosis udleveret fra en recept totalt
  tidl_dos_ar - valgte doser fra tidligere recepter
  dage_ud_sygehus_ar - dage udenfor sygehus i hver periode
  restdosis - til overs fra tidligere recepter
  ***************************************************************************/

  data temp; set temp; /* Senere kan m1 evt. overskrives i stedet for */
   length ar center 3;
   length startdag slutdag reglsekv_high reglsekv_low 4;

   ar=1*&antal_recepter; /* Antal recepter som tal */
   center=ar/2+0.5;      /* Nummeret på den recept vi interesserer os for */

   array eksd_ar[&antal_recepter]  eksd_1-eksd_&antal_recepter; /*Ekspeditionsdato*/
   array apk_ar[&antal_recepter] apk_1-apk_&antal_recepter; /*Antal pakker */
   array packsize_ar[&antal_recepter] packsize_1-packsize_&antal_recepter;/*Pakkestørrelse*/
   array strnum_ar[&antal_recepter] strnum_1-strnum_&antal_recepter; /*styrker*/
   array inddto_ar[&max_indl] inddto1-inddto&max_indl; /* Indlæggelsesdatoer*/
   array uddto_ar[&max_indl]uddto1-uddto&max_indl; /* Udskrivelsesdatoer*/

   array dos_ar[&antal_recepter] dosr1-dosr&antal_recepter; /*De anvendte doser ved de udvalgte recepter*/
   array mindos_ar[&antal_recepter] mindos1-mindos&antal_recepter;
   array maxdos_ar[&antal_recepter] maxdos1-maxdos&antal_recepter;
   array typdos_ar[&antal_recepter] typdos1-typdos&antal_recepter;
   array indltid_ar[&antal_recepter] indl_tid1-indl_tid&antal_recepter; /* Indlæggelses tid i hver receptperiode*/

   array primdos_ar[&antal_recepter]; /* Udregnet primitiv dosis af ET interval og piller */
   array receptdosis_ar[&antal_recepter]; /*dosis udleveret fra en recept totalt*/
   array dage_ud_sygehus_ar[&antal_recepter]; /* dage udenfor sygehus i hver periode*/
   length dage_ud_sygehus_ar1-dage_ud_sygehus_ar&antal_recepter 4;
   array tidl_dos_ar[&antal_recepter]; /* endelige doser (finaldosis) for forrige recepter - bemærk: retaines */
   retain tidl_dos_ar1-tidl_dos_ar&antal_recepter
          restdosis; /* til overs fra tidligere recepter*/

   format startdag slutdag eksd_1-eksd_&antal_recepter  date9.;
   /***************************************************************************************
    Hvis trace er 1, så skrives der flittigt i loggen med mellemregninger. Hver sted skrves et
    post-nummer som fortæller hvor i programmet det blev udløst og forskellige mellemregninger
   ********************************************************************************************/
   if &trace then
    do;
	    put " ";
		put " ";
        put "************ post-1  Nu går vi i gang med en ny recept som center **********  " pnr=;
		put "  Tabel over recepterne  ";
		do i=1 to &antal_recepter;
		 put i= eksd_ar[i]= apk_ar[i]= packsize_ar[i]= strnum_ar[i]=;
		end;
	end;
	
   /***************************************************
    Før de egentlige beregninger skal der laves nogle mellemregninger.  Den første af disse er at
    finde den samlede indlæggelsestid i HVER receptperiode og lægge resultatet i
     indltid_ar[]
    Denne beregning laves på alle recepter undtagen den sidste, hvor slutdato er ukendt

    ar er antallet af recepter = mackrovariablen antal_recepter
    Ydre do-loop gennemløber recepterne (1 to ar-1)
    Ved start sættes indlæggelsestiden til 0
    Indre do-loop gennemløber så alle indlæggelser for patienten og genererer

	Rettelse 29.10.11 - kun udregning i perioder med ægte recepter - uden betydning.
   ********************************************/
   do i=1 to ar-1;
    indltid_ar[i]=0;
    do ii=1 to &max_indl;
     if inddto_ar[ii] ne . and inddto_ar[ii]>=eksd_ar[i] and eksd_ar[i] ne . and inddto_ar[ii]<=eksd_ar[i+1] and eksd_ar[i+1] ne . then
         /* ovenfor: hvis indlæggelsesdatoen ligger i aktuelle receptperiode*/
         indltid_ar[i]=indltid_ar[i]+ min(uddto_ar[ii],eksd_ar[i+1])-inddto_ar[ii];
         /* ovenfor: Indlæggelsestiden summeres. Hver indlæggelse går fra indlæggelsestidspunktet og til udskrivelses
          tidspunktet eller til næste udlevering - og højst til næste udlevering - lidt sært med recepter der udleveres
          i løbet af en indlæggelse, men mon ikke det findes*/
    end;
   end;
   if &trace then
    do;
     put "post 2 - indlæggelsestider i recept-perioder ";
      do i=1 to &antal_recepter;
	   put i= indltid_ar[i]=;
	  end;
	end;

     /*****************************************************************
     Find min/max/typ-dosis for aktuelle styrker
     I modsætning til de min/max/typ som angives for makroen som helhed
     er dette min/max/typ for de aktuelle styker i rækken af recepter

     For hver recept gennemløbes de 4 "lovlige" doser og recepterne tildeles værdier af min/max/typ

     Bemærk konstruktionen "&&dos&ii". Der spørges om strnum_ar[] har værdien &dos1,2,3,4 svarende til
     de variable der blev angivet til makroen foroven. Men her "udregnes" værdien ved hjælp af makrovariablen
     ii der gennemløberne værdierne 1 til 4. For at værdien af "&dos1-4" udregnes korrekt skal makrocompileren løbe
     hen over konstruktionen 2 gange hvilket sikred med "&&".
   *******************************************************************/

   do i=1 to &antal_recepter; /* bemærk at "r" betyder aktuelle recepter og uden "r" de 4 lovlige doser*/
    %do ii=1 %to 4;
     if strnum_ar[i]=&&dos&ii then
      do;
       /* den rigtige dosis er fundet og nu sættes alle værdier. Bemærk en metoden sikrer at der gerne må
        være gentagelser i min/max/typ - bare de er ens*/
       dos_ar[i]=&&dos&ii; mindos_ar[i]=&&min_dos&ii; maxdos_ar[i]=&&max_dos&ii; typdos_ar[i]=&&typ_dos&ii;
      end;
    %end;
   end;
   if &trace then
    do;
     put "post 3 - Max-min doser for aktuelle recepter ";
	 do i=1 to &antal_recepter;
	  put i= mindos_ar[i]= maxdos_ar[i]= typdos_ar[i]=;
	 end;
	end;
	 
       /* fejlsøgning*/
    if mindos_ar[center]=. or maxdos_ar[center]=. or typdos_ar[center]=. then
     put "*************************************** Alvorlig fejl - alle doser ikke defineret ********";

  /****************************************************************************************
   Beregning af følgende hjælpevariable
     primdos_ar[] primitiv dosisudregning i hver receptperiode baseret på dosis og tid udskrevet - mellemregning
     receptdosis_ar[] - dosis udleveret fra en recept totalt
     dage_ud_sygehus_ar;
  *****************************************************************************************/

    do i=1 to &antal_recepter-1;
     receptdosis_ar[i]=packsize_ar[i]*apk_ar[i]*strnum_ar[i];
     if eksd_ar[i] ne . and eksd_ar[i+1] ne . then
      do;
       dage_ud_sygehus_ar[i]=(eksd_ar[i+1]-eksd_ar[i])-indltid_ar[i];
	   if dage_ud_sygehus_ar[i]<1 then dage_ud_sygehus_ar[i]=1;
       primdos_ar[i]=receptdosis_ar[i] / dage_ud_sygehus_ar[i];
      end;
    end;
    if &trace then
	do;
     put "post 4 - Hjælpevariable ";
	 do i=1 to &antal_recepter;
	  put i= primdos_ar[i]= receptdosis_ar[i]= dage_ud_sygehus_ar[i]= tidl_dos_ar[i]= ;
	 end;
	 put " Den enlige " restdosis=;
	end;
    
   /****************************************************************************************************
    Det følgende stykke program er helt centralt. Her undersøges i hvilket omfang recepterne før og efter
     interesserecepten har "relation" til interesserecepten - om det er samme dosis og om der kan være tale
     om en løbende behandling. Det sidste er netop situationen hvor flere udleveringer kan benyttes til at udregne
     den korrekte dosis.

    regelsekv_low - er den laveste recept i rækken som kan udtrykke en kontinuert behandling som rækker ind over
     interesserecepten
    regelsekv_high er den højeste recept i rækken som kan være en kontinuert behandling
    linkdos_low er laveste i rækken som udtrykker en kontinuert række recepter med samme tabletstyrke
    linkdos_high er den højeste i rækken som udtrykke en kontinuert række recepter med samme tabletstyrke
   ******************************************************************************************************/
   reglsekv_low=.; reglsekv_high=.; /* numre på de i sekvensen som representerer samme dosis som center */
   linkdos_low=.; linkdos_high=.; /* numre på de i sekvensen som uafhængigt af dosis kan have relation til center evt. via næste recept - altså ikke nødvendigvis en pause */



   /*******************************************************************************************************
    Den ydre do-loop løber fra 1 til sidste før interesserecepten
    Hvis dosis er den samme som interesserecepten sættes regelsekv_low til denne. Hvis en af de næste
    pludseligt er en anden dose sættes den tilbage til blank ".".  regelsekv_low bliver således det laveste
    nummer hvor der er en kontinuert række af recepter med samme pillestyrke op til interesserecepten
    center - interesserecepten - den midterste.

    Derefter kommer en kompliceret beregning af om de tidligere recepter evt. kan forklare et kontinuert forløb
    op til centerrecepten.

    Kravet er at en sekvens af - faktiske doser for tidligere medicin samt minimumsdoser for fremtidig medicin giver
    et kontinuert forløb op til center - OG at der ikke løbes tør for tabletter undervejs.
    Altså - en stor udlevering kan kompensere for efterfølgende små, men ikke omvendt.

    indtid - antal indlæggelsesdage fra starten af aktuelle recept til starten af interesserecepten
    dage - dage udenfor sygehus mellem starten af aktuelle recept og starten af interesserecepten
    temp_dage - dage udenfor sygehus mellem starten af aktuelle recept og starten af den næste i rækken op
     mod center som gennemløbes
    sumdag - dage som kan forklares ved faktiske eller minimale forbrug.
   Nøglesammenligningen til at etablere "link" er at sammenligne "dage" med "sumdag"
   ***************************************************************************************************/

   do i=1 to center-1;
      if strnum_ar[i]=strnum_ar[center] and reglsekv_low=. then reglsekv_low=i;
      /* ovenfor: Hvis styrken er den samme som interesserecepten sættes regelsekv_low til at være dette nummer*/
      if strnum_ar[i] ne strnum_ar[center] then reglsekv_low=.; /* fjerner "øer" med anden dosis */
      sumdag=0; indtid=0;
      if &trace then put "post s.1 - linkdoshigh søgning fra neden" i= center=  ;
      do ii=i to center-1;
       if tidl_dos_ar[ii] >0 and &left_only=0 then sumdag=sumdag+receptdosis_ar[ii] /tidl_dos_ar[ii];
       else
	     if mindos_ar[ii] >0 then
         sumdag=sumdag+receptdosis_ar[ii] /mindos_ar[ii];
        if &trace then put "post s.2 - linkdoshigh søgning fra neden" i= ii= sumdag=  ;
        /* ovenfor - summering af pakkestørrelse*antal pakker*styrke delt med minimumsdosis -
           altså antallet af dage som kan forklares af ALLE recepter fra i til og med recepten
           før center - med minimumsdosis for hver recept.

          Bemærk nævneren - hvis det er en tidligere dosis som er defineret, så vælge denne - ellers mindos
		 Rettelse 26.10.11: Tilføjelse af left_only til logisk betingelse vedrøende valg af linkdos_low
         */
       indtid=indtid+indltid_ar[ii]; /* sum af indlæggelsestid*/
       temp_dage=(eksd_ar[ii+1]-eksd_ar[i])-indtid;
       /*ovenfor: antallet af dage fra den recept vi er i gang med og til næste recept som evt. skulle kunne
        forklares af udleveringer indtil nu. Man skal hele tiden kunne "række" til næste recept*/
       if sumdag ne 0 and sumdag ne . and temp_dage ne . and sumdag<temp_dage then sumdag=.;
       /* ovenfor: Hvis antallet af behandlingsdage som kan forklares af minimumsdosis/faktisk dosis ikke når frem til
        næste recept, så skal link_dos_low ikke præges af disse og sumdag sættes til "." således at
        summering standser - men beregningen gentages for den næste recept som så får chancen til at
        blive link_dos_low*/
      end;
      dage=(eksd_ar[center]-eksd_ar[i])-indtid;
      /* ovenfor - antallet af dage fra starten af aktuelle recept indtil midterrecepten fratrukket indlæggelsestiden*/
      if sumdag ne 0 and sumdag ne . and sumdag>=dage and dage ne . and linkdos_low = . then
         linkdos_low=i;
      /* ovenfor - Hvis antallet af behandlingsdage rækker helt til center så bliver link_dos_low dette receptnummer -
       også hvis de følgende recepter er små*/
   end;

   /*********************************************************************
    Nu gentages øvelsen fra oven ned mod center
   ***********************************************************************/

   do i=&antal_recepter to center by -1; /*fra øverste ned mod center*/
      if strnum_ar[i]=strnum_ar[center] and reglsekv_high=. then reglsekv_high=i;
      if strnum_ar[i] ne strnum_ar[center] then reglsekv_high=.; /* fjerner "øer" med anden dosis */
      sumdag=0; indtid=0;

      do ii=center to i-1;
	   if mindos_ar[ii] >0 then
       sumdag=sumdag+receptdosis_ar[ii]/mindos_ar[ii];
       indtid=indtid+indltid_ar[ii];
	   
      end;
      dage=(eksd_ar[i]-eksd_ar[center])-indtid;
	  if restdosis ne . then
	   sumdag=sumdag+restdosis/mindos_ar[center]; /* Tillæg af den restdosis der er fra recepter før center*/
      if sumdag ne 0 and sumdag ne . and sumdag>dage and dage ne . and linkdos_high=. then linkdos_high=i;
	  if sumdag<dage or sumdag=. or dage=. then linkdos_high=.;
	  if &trace then put "post 4.1 - linkdoshigh søgning fra oven" i= sumdag= dage= indtid= ;
   end;

   if reglsekv_low=. then reglsekv_low=center;
   if reglsekv_high=. then reglsekv_high=center;
   if linkdos_low=. then linkdos_low=center;
   if linkdos_high=. then linkdos_high=center;
   if &trace then
    do;
     put "post 5 - reglsekv og linkdos ";
	 put reglsekv_low= reglsekv_high= linkdos_low= linkdos_high= ;
	end;

   /************************************
    Situation 1 - All alone - brug af typisk dosis til der ikke er flere piller.
    Indlæggelser i forløbet ignoreres

     receptdosis_ar[] - dosis udleveret fra en recept totalt
	restdosis er totaldosis som overføres til næste recept

    *************************************/
   if linkdos_low=center and linkdos_high=center then
    do;
      finaldosis=typdos_ar[center];
      startdag=eksd_ar[center]; /* Bemærk at det er den altid!!*/
      slutdag=startdag-1+round(receptdosis_ar[center]/typdos_ar[center]);
      slutdag=max(slutdag,startdag+1);
	  restdosis=0; 
	
      /*ovenfor - der tillægges antal dage som kan forklares af typisk dosis uden hensyn til indlæggelser*/

     if &trace then
	  put "post 6 - Situation 1 - Ensom recept "
        finaldosis=  startdag=   slutdag= restdosis=;
	   
    end;
   /*******************************************************************************
    Situation 2 - kronisk beh som fortsætter både før og efter center. I dette tilfælde
    gættes der på at middelværdien over hele sekvensen representerer den "korrekte" dosis.
    Hvis den maximalt mulige dosis - baseret på rest-medicin fra tidligere recepter sammen med
    aktuelle recept er mindre end middeldosis, da vælges denne.

    Hvis der sker dosisændringer på en faktor 2 antages de at være relle og der midles ikke - med
    mindre intervallet kun kan køre når dosis regnes med.

    Følgende sikre at man kun går ned i dosis hvis der er behov for det.

    Endeligt genereres en pause hvis dosis tvinges ned ned og siden op.  Dette er eneste brud
    på denne algoritme i relation til at bevare sammenhængen mellem recepter.

    sum - samlet dosis - sum_efter/foer dosis efter interesserecept
    dage - behandlingsdage (udenfor sygehus) - dage_efter/foer  efter interesserecept

    dosis_til_raadighed er resterende dosis sendt videre til aktuelle recept

    primdos_ar[] primitiv dosisudregning i hver receptperiode baseret på dosis og tid udskrevet - mellemregning
    receptdosis_ar[] - dosis udleveret fra en recept totalt
    
    tidl_dos_ar - valgte doser fra tidligere recepter
    dage_ud_sygehus_ar - dage udenfor sygehus
   *******************************************************************************/
   
    sum=0; dage=0; dosis_til_raadighed=0; dage_efter=0;  sum_efter=0; dage_foer=0; sum_foer=0;
    if reglsekv_low ne . and reglsekv_high ne . and linkdos_low ne . and linkdos_high ne . and
     reglsekv_low ne center and reglsekv_high ne center and linkdos_low ne center and linkdos_high ne center then
     /*ovenfor - der er samme dosis både før og efter og doseringerne kan nå center*/
    do;
	if not &left_only then
     do; 
      do i=max(reglsekv_low,linkdos_low) to min(reglsekv_high,linkdos_high)-1;
       /* ovenfor: beregningen løber fra laveste recept med samme dosis som kan nå center til
        højeste recept med samme dosis med MULIG forbindelse til center*/
       if receptdosis_ar[i] ne . then
        do;
          sum=sum+receptdosis_ar[i];
          if i>center then sum_efter=sum_efter+receptdosis_ar[i];
          if i<center then sum_foer=sum_foer+receptdosis_ar[i];
        end;
        /*ovenfor: summering af udleverede doser*/
       if dage_ud_sygehus_ar[i] ne . then
        do;
         dage=dage+dage_ud_sygehus_ar[i];
         if i>center then dage_efter=dage_efter+dage_ud_sygehus_ar[i];
         if i<center then dage_foer=dage_foer+dage_ud_sygehus_ar[i];
        end;
       /*ovenfor: summering af dage udenfor sygehus*/
      end;
      dosis_til_raadighed=receptdosis_ar[center];
	  if restdosis ne . then
	  dosis_til_raadighed=dosis_til_raadighed+restdosis;
	  /* ovenfor: den dosis som kan anvendes er receptdosis+evt. restdosis fra tidl recepter*/
      if dage=0 then dage=1; /* flere recepter samme dag!*/;
      meandos=sum/dage; /* middeldosis i HELE intervallet*/
      if dage_efter >=1 then meandos_efter=sum_efter/dage_efter;
      if dage_foer >=1 then meandos_foer=sum_foer/dage_foer;
      Max_mulig_dosis=dosis_til_raadighed / dage_ud_sygehus_ar[center];
      /*ovenfor - Den største MULIGE dosis*/
      if &trace then
	    do;
          put "post 7 -  "   meandos= meandos_foer= meandos_efter= max_mulig_dosis= dosis_til_raadighed= primdos_ar[center]= ;
          put dage_foer= sum_foer= dage_efter= sum_efter=;
	    end;

      finaldosis=min(primdos_ar[center],max_mulig_dosis); /* første gæt */
      if &trace then
          put "post 8 - første gæt "   finaldosis= pnr=;
 
      if meandos<max_mulig_dosis and mindos_ar[center]<meandos<maxdos_ar[center]then
       do;
        finaldosis=meandos; /* andet gæt er middeldosis */
        if &trace then
          put "post 9 - andet gæt middeldosis "   finaldosis= pnr=;
       end;

      if primdos_ar[center]>meandos_efter and primdos_ar[center]>meandos_foer
        then
         do;
          finaldosis=mean(meandos_efter,meandos_foer);  /* der er udleveret mange piller bare EN gang */
          if &trace then
          put "post 10 - mange piller en gang "   finaldosis= pnr=;
         end; 

       if meandos_foer>max_mulig_dosis and meandos_efter<=max_mulig_dosis and meandos_efter ne . and meandos_foer ne . and primdos_ar[center]<=max_mulig_dosis
       then
        do;
         finaldosis=mean(meandos_efter,primdos_ar[center]); /* skæve værdier før - kig kun efter*/
         if &trace then
          put "post 11 - skæv før,kig efter  "  finaldosis= pnr=;
        end;

       if meandos_efter ne . and meandos_foer ne . and meandos_efter>max_mulig_dosis and meandos_foer<=max_mulig_dosis and primdos_ar[center]<=max_mulig_dosis
       then
        do;
          finaldosis=mean(meandos_efter,primdos_ar[center]); /* skæve værdier efter - kig kun før*/
          if &trace then
           put "post 12 - skæv efter,kig før  "  finaldosis= pnr=;
        end;

       if finaldosis<meandos_foer and meandos_foer<max_mulig_dosis and finaldosis<meandos_efter then
        do;
         finaldosis=meandos_foer;
          /* ovenfor - der er lavet dosisnedgang selv om der er piller til at
          opretholde dosis - kan opstå hvis en tidligere stor recept kompenserer
          for at aktuelle er lille - dosis opretholdes i dette tilfælde*/
         if &trace then
          put "post 13 - forkert dosisnedgang  "  finaldosis= pnr=;
        end;

       finaldosis=round(min(finaldosis,max_mulig_dosis),mindos_ar[center]);
         if finaldosis>max_mulig_dosis then finaldosis=finaldosis-mindos_ar[center];
	   if &trace then
	    put " Post 13.1 Finaldosis efter afrunding  " finaldosis=;

       if finaldosis<mindos_ar[center] then
         do;
          finaldosis=mindos_ar[center];
          if &trace then
		   put "post13.2 - kom under min - min-dos valgt " finaldosis=;
	     end;
       if finaldosis>maxdos_ar[center] then 
         do;
          finaldosis=maxdos_ar[center];
          if &trace then
		   put "post13.3 - kom over max - max-dos valgt " finaldosis=;
	     end;

       startdag=eksd_ar[center];
        if eksd_ar[center-1] ne . then
        startdag=max(startdag,eksd_ar[center-1]+1); /* når recepter kommer i klump rettes de ind */
       slutdag=max(eksd_ar[center+1]-1,startdag+1); /* behandling slutter ALTID dagen før næste recept og tidligst
        dagen efter startdag*/
    
         /*** Beregning af NÆSTE restdosis ***/
	    restdosis=dosis_til_raadighed-finaldosis*((slutdag-startdag)-indltid_ar[center]); 
		/* 29.10.11 - max restdosis*/
		restdosis=min(&max_depot,restdosis);

       if max_mulig_dosis<tidl_dos_ar[center-1] and max_mulig_dosis<meandos_efter
        then /* pludseligt fald i dosis - det er en pause - bemærk at denne kommer sidst
               af hensyn til dag-beregningen */
        do;
         finaldosis=max(mean(tidl_dos_ar[center-1],meandos_efter),mindos_ar[center]);
	     finaldosis=min(finaldosis,maxdos_ar[center]);
	      finaldosis=round(finaldosis,mindos_ar[center]);
         slutdag=startdag-1+round(dosis_til_raadighed/finaldosis);
         if &trace then
          put "post 14 - kort pause  "  finaldosis= dosis_til_raadighed=;
	     restdosis=0;
        end;

	
	  /*Fejlsøgning*/
	   if restdosis<0 or restdosis=. then 
        put "Fejl - ulovlig restdosis i situation 2 " pnr= restdosis=;

     if &trace then
      put "post 15 - Situation 2 - midt i konstant forløb med samme tabletstyrke før og efter "
       finaldosis=  startdag=   slutdag= restdosis=;
	 if finaldosis>max_mulig_dosis and restdosis>0 /*ignorerer tvungen kort pause*/ then
	  put "Situation 2 - der er valgt en dosis som er større en max-mulig-dosis" pnr= finaldosis= max_mulig_dosis=;
    end;
     /**********************************************************************************
     Situation 2 - left
     Der gennemføres en beregning identisk med situation 3 - altså beregning af
     dosis og slutdato uden kendskab til fremtidige recepter
 	*********************************************************************************/
	if &left_only then
	 do;
      sum=0; dage=0; dosis_tiraadighed=0;
      do i=linkdos_low to center-1;
       sum=sum+receptdosis_ar[i];
        /*ovenfor: summering af udleverede doser*/
       dage=dage+dage_ud_sygehus_ar[i];
       /*ovenfor: summering af dage udenfor sygehus*/
      end;
	  dosis_tiraadighed=receptdosis_ar[center];
	  if restdosis ne . then
	  dosis_tiraadighed=dosis_tiraadighed+restdosis;
	  /* til rådighed er aktuelle receptdosis+evt. restdosis fra tidl recepter*/
      if dage=0 then dage=1; /* obs flere recepter samme dag*/
      meandos=sum/dage; /* middeldosis i HELE intervallet*/
      if &trace then
       put "post 15A - situation2 left  " meandos= max_mulig_dosis= dosis_tiraadighed=;
      finaldosis=round(meandos,mindos_ar[center]);
      if finaldosis<mindos_ar[center] then finaldosis=mindos_ar[center];
      if finaldosis>maxdos_ar[center] then finaldosis=maxdos_ar[center];
      /*ovenfor - den mindste af (meandos, maxdos for center) afrundes i enheder af mindos */
      startdag=max(eksd_ar[center],eksd_ar[center-1]+1);
      slutdag=eksd_ar[center]-1 + round(dosis_tiraadighed/finaldosis);
         /* ovenfor - antal dage er gammel restmedicin + udleveret divideret med dosis*/
      slutdag=max(slutdag,startdag+1);
	  /* version 27 korrektion af slutdag og restdosis*/
	  slutdag=min(slutdag,eksd_ar[center+1]-1);
	    /*** Beregning af NÆSTE restdosis ***/
	  restdosis=dosis_tiraadighed-finaldosis*((slutdag-startdag)-indltid_ar[center]);
      /* 29.10.11 - max restdosis*/
		restdosis=min(&max_depot,restdosis); 
      if &trace then
       put "post 15B - Situation 2 kun kigge bagud "
        finaldosis=  startdag=   slutdag= restdosis= ; 

	    /* version 27 korrektion af slutdag og restdosis nappet for fejlbehæftet version 26*/
	   slutdag=min(slutdag,eksd_ar[center+1]-1);
	    /*** Beregning af NÆSTE restdosis ***/
	   restdosis=dosis_tiraadighed-finaldosis*((slutdag-startdag)-indltid_ar[center]); 
        /* 29.10.11 - max restdosis*/
		restdosis=min(&max_depot,restdosis); 
     end;
	end;
   /**********************************************************************************
   Situation 3 - Kronisk forløb som SLUTTER med center
   Der beregnes en middeldosis som forsætter fra starten af det kroniske forløb
   (inden for de valgte recepter) til der ikke er flere piller tilbage.
   Indlæggelser i forløbet sparer på medicin - indlæggelser efter den sidste recept ignoreres.

    primdos_ar[] primitiv dosisudregning i hver receptperiode baseret på dosis og tid udskrevet - mellemregning
    receptdosis_ar[] - dosis udleveret fra en recept totalt
    tidl_dos_ar - valgte doser fra tidligere recepter
    dage_ud_sygehus_ar;

    *********************************************************************************/
   sum=0; dage=0; dosis_til_raadighed=0;
   if reglsekv_low ne center and linkdos_low ne center and linkdos_high = center then
   do;
    if &trace then
     put "post 16 - Situation 3 starter "
      finaldosis=  startdag=   slutdag= ;
    do i=linkdos_low to center-1;
     sum=sum+receptdosis_ar[i];
      /*ovenfor: summering af udleverede doser*/
     dage=dage+dage_ud_sygehus_ar[i];
     /*ovenfor: summering af dage udenfor sygehus*/
    end;
	dosis_til_raadighed=receptdosis_ar[center];
	if restdosis ne . then
	dosis_til_raadighed=dosis_til_raadighed+restdosis;
	/* til rådighed er aktuelle receptdosis+evt. restdosis fra tidl recepter*/
    if dage=0 then dage=1; /* obs flere recepter samme dag*/
    meandos=sum/dage; /* middeldosis i HELE intervallet*/
    if &trace then
     put "post 17  " meandos= max_mulig_dosis= dosis_til_raadighed=;
    finaldosis=round(meandos,mindos_ar[center]);
    if finaldosis<mindos_ar[center] then finaldosis=mindos_ar[center];
    if finaldosis>maxdos_ar[center] then finaldosis=maxdos_ar[center];
    /*ovenfor - den mindste af (meandos, maxdos for center) afrundes i enheder af mindos */
    startdag=max(eksd_ar[center],eksd_ar[center-1]+1);
    slutdag=eksd_ar[center]-1 + round(dosis_til_raadighed/finaldosis);
       /* ovenfor - antal dage er gammel restmedicin + udleveret divideret med dosis*/
    slutdag=max(slutdag,startdag+1);
	/* ny restdosis */ restdosis=0;
    if &trace then
     put "post 18 - Situation 3 - afslutning af forløb med konstant styrke "
      finaldosis=  startdag=   slutdag= ;
	/* beregninger som kun kigger bagud*/
	
   end;

   /******************************************************************************
      Situation 4 - dosisskift -start. Der beregnes en gennemsnitsdosis for HELE
      perioden således at aktuelle receptperiode bevares. Denne lidt kryptiske form for også på en nogenlunde
      ordentlig måde at håndtere situationer hvor flere styrker udleveres samtidigt eller næsten samtidigt med
      henblik på optrapning eller nedtrapning. Der midles i nogen grad i disse situationer.

      sum er udleveret medicin i linkperioden
      dage er dage i link-perioden
      sum_brugt er totaldosis brugt FØR center
      sum_tidl er udleveret FØR center

      sum - samlet dosis
      dage - behandlingsdage (udenfor sygehus)

      dosis_til_raadighed er resterende dosis sendt videre til aktuelle recept
      dage_efter - dage efter interesserecept
      sum_efter - dosis efter interesserecept
      dage_foer - dage foer interesserecept

      primdos_ar[] primitiv dosisudregning i hver receptperiode baseret på dosis og tid udskrevet - mellemregning
      receptdosis_ar[] - dosis udleveret fra en recept totalt
      tidl_dos_ar - valgte doser fra tidligere recepter
      dage_ud_sygehus_ar - dage udenfor sygehus
   ************************************************************************************************/
   
    sum=0; dage=0; dosis_til_raadighed=0; dage_efter=0;  sum_efter=0; dage_foer=0; sum_foer=0;
    if (reglsekv_high=center and linkdos_high ne center) or
      /* link opad uden samme dosis*/
      (linkdos_high ne center and linkdos_low ne center and reglsekv_low=center) or
       /* link nedad og opad - med ny dosis nedad*/
       (linkdos_low=center and linkdos_high ne center) then
       /* første i en serie*/
    do;
	 if not &left_only then
     do;
        if &trace then
          put "post 19 - Start situation 4 "
          finaldosis=  startdag=   slutdag= ;
      do i=linkdos_low to linkdos_high-1;
       if receptdosis_ar[i] ne . then
       do;
          sum=sum+receptdosis_ar[i];
          if i>center then sum_efter=sum_efter+receptdosis_ar[i];
          if i<center then sum_foer=sum_foer+receptdosis_ar[i];
        end;
         /*ovenfor: summering af udleverede doser*/
       if dage_ud_sygehus_ar[i] ne . then
       do;
         dage=dage+dage_ud_sygehus_ar[i];
         if i>center then dage_efter=dage_efter+dage_ud_sygehus_ar[i];
         if i<center then dage_foer=dage_foer+dage_ud_sygehus_ar[i];
        end;
        /*ovenfor: summering af dage udenfor sygehus*/
      end;
 	   dosis_til_raadighed=receptdosis_ar[center];
	   if restdosis ne . then dosis_til_raadighed=dosis_til_raadighed+restdosis;
	   /* ovenfor - dosis til rådighed er receptdosis og evt. restdosis fra tidl recepter */
      if &trace then
         put "post 20 - " center= dage_ud_sygehus_ar[center]= dage= dage_efter= dage_foer= dosis_til_raadighed=; 

      if dage=0 then dage=1; /* flere recepter samme dag!*/;
      if dage_ud_sygehus_ar[center]=0 then dage_ud_sygehus_ar[center]=1;
      meandos=sum/dage; /* middeldosis i HELE intervallet*/
      if dage_efter>=1 then meandos_efter=sum_efter/dage_efter;
      if dage_foer>=1 then meandos_foer=sum_foer/dage_foer;
      Max_mulig_dosis=dosis_til_raadighed / dage_ud_sygehus_ar[center];
      /*ovenfor - Den største MULIGE dosis*/
      if &trace then
          put "post 21 -  " meandos_foer= meandos_efter=  meandos= max_mulig_dosis= primdos_ar[center]=;


      if mindos_ar[center]<=primdos_ar[center]<=maxdos_ar[center] then
	   do;
        finaldosis=primdos_ar[center];
	    if &trace then
          put "post 21 - Situation 4 første gæt middel i interval"
          finaldosis=  startdag=   pnr= ;
	   end;
       /* Ovenfor - 1. prioritet er beregning over EN recept */
      if finaldosis=. and
       mindos_ar[center]<=min(meandos,max_mulig_dosis)<=maxdos_ar[center]  then
	    do;
         finaldosis=min(meandos,max_mulig_dosis);
	     finaldosis=max(finaldosis,mindos_ar[center]);
	     finaldosis=min(finaldosis,maxdos_ar[center]);
	     if &trace then
          put "post 22 - Situation 4 - andet gæt "
          finaldosis=  pnr= ;
        /* Ovenfor - Hvis perioden ellers alene kan forklare en middeldosis/max-mulig, så vælges denne */
        end;
      if finaldosis=. and primdos_ar[center]<=mindos_ar[center] then
       do;
        finaldosis=mindos_ar[center];
	    if &trace then
         put "post 23 - Situation 4 - lille dosis "
         finaldosis=  pnr= ;
	   end;
       /* ovenfor - hvis både primdos og meandos og max-mulig medfører urimelige værdier så vælges max henholdsvis min for dosis
       afhængig af primdos*/

      if meandos_foer<meandos_efter and meandos_foer ne . and meandos_foer<primdos_ar[center]<=meandos_efter
       and primdos_ar[center]<=max_mulig_dosis
       then 
	    do;
         finaldosis=primdos_ar[center]; /* en optrapning */
	     if &trace then
          put "post 24 - Situation 4 - optrapning "
          finaldosis=  pnr= ;
        end;
      if meandos_efter<meandos_foer and meandos_efter ne . and meandos_efter<primdos_ar[center]<=meandos_foer
       and primdos_ar[center]<=max_mulig_dosis
       then 
        do;
         finaldosis=primdos_ar[center]; /* en nedtrapning */
	     if &trace then
          put "post 25 - Situation 4 nedtrapning "
          finaldosis=  pnr=;
	    end; 

      if finaldosis>maxdos_ar[center] then finaldosis=maxdos_ar[center];
       finaldosis=round(finaldosis,mindos_ar[center]);
         if finaldosis>max_mulig_dosis then finaldosis=finaldosis-mindos_ar[center];
	  if finaldosis<mindos_ar[center]and finaldosis ne . then finaldosis=mindos_ar[center];
      /* Og hvis der STADIG ikke er fundet en dosis! */
      if finaldosis=. and max_mulig_dosis>maxdos_ar[center]and meandos_foer=. and meandos_efter=. then
	   finaldosis=typdos_ar[center]; /* en afart af ensom dosis - f.eks. to recepter samme dag og ikke andet*/
	  if finaldosis=. and max_mulig_dosis>maxdos_ar[center] then finaldosis=maxdos_ar[center]; /* endnu mere sært*/
	  if finaldosis=. and max_mulig_dosis<typdos_ar[center] then finaldosis=typdos_ar[center];
	  if finaldosis=. then finaldosis=mindos_ar[center]; /* ultimative default*/


      /* Tabletstørrelsen vælges ALTID fra aktuelle recept - der er ikke håndtering af multiple tabletter*/

      startdag=max(eksd_ar[center],eksd_ar[center-1]+1);
      slutdag=max(eksd_ar[center+1]-1,startdag+1);
	  /* ny restdosis*/ restdosis=dosis_til_raadighed-finaldosis*((slutdag-startdag)-indltid_ar[center]);
      /* 29.10.11 - max restdosis*/
		restdosis=min(&max_depot,restdosis); 
	  if restdosis<0 then
	   put "fejl - negativ restdosis " pnr= finaldosis= restdosis= dosis_til_raadighed= slutdag= startdag=;

       /* Der resterer nu det potentielle problem at aktuelle dosis ikke tillader at man når frem til næste recept selv om
       der er anvendt min-dos*/
      if finaldosis>max_mulig_dosis then
       do;
        slutdag=eksd_ar[center]-1+round(dosis_til_raadighed/finaldosis);
	    restdosis=0;
	     if &trace then
          put "post 26 - Situation 4 - tvungen pause "
          finaldosis= slutdag= ;
        if slutdag>eksd_ar[center+1] and eksd_ar[center+1]-startdag>2 /*flere recepter samme dag skubber dagene*/ then
         put "************* HELT FORKERT PAUSEBEREGNING I SIT 4 ** " eksd_ar[center+1]= slutdag= pnr=;
       end;

      slutdag=min(slutdag,eksd_ar[center+1]);
      slutdag=max(slutdag,startdag+1);
	
    	/* fejlsøgning*/
	  if restdosis<0 or restdosis=. then
	   put " fejl - Ulovlig restdosis i situation 4 " pnr= restdosis=;
      if &trace then
       put "post 27 - Situation 4 - start på forløb eller dosisskift i kronisk forløb - men aldrig sidste recept i sekvens"
        finaldosis=  startdag=   slutdag= restdosis=;
    end;
    /******************************************************************************
      Situation 4 - left
     Der gennemføres en beregning identisk med situation 5 - altså beregning af
     dosis og slutdato uden kendskab til fremtidige recepter
     ************************************************************************************************/
	if &left_only then
	do;
	  sum=0; dage=0; dosis_tiraadighed=0;
      do i=linkdos_low to center-1;
       sum=sum+receptdosis_ar[i];
        /*ovenfor: summering af udleverede doser*/
       dage=dage+dage_ud_sygehus_ar[i];
       /*ovenfor: summering af dage udenfor sygehus*/
      end;
	  dosis_tiraadighed=receptdosis_ar[center];
      if restdosis ne . then
	  dosis_tiraadighed=dosis_tiraadighed+restdosis;
	  /* dosis til rådighed er receptdosis + evt. rest fra tidl recepter*/
      if dage=0 then dage=1; /* obs flere recepter samme dag */
      meandos=sum/dage; /* middeldosis i HELE intervallet*/
	  if &trace then put "post 27A " sum= dage= meandos=;
      /*subscenarie 1-2 - meandos er meget stor/lille i forhold til sidste maxdos */
      if meandos>=maxdos_ar[center] then
       finaldosis=maxdos_ar[center];
      if meandos<mindos_ar[center] then
       finaldosis=mindos_ar[center]; 
	  /* rettelse 8.10.11:*/
	  if meandos=0 then finaldosis=typdos_ar[center];

      /*subscenarie 3 - meandos er imellem max og min */
      if mindos_ar[center]<meandos<maxdos_ar[center]  then
       finaldosis=typdos_ar[center]; 
 
    	 startdag=max(eksd_ar[center],eksd_ar[center-1]+1);
      slutdag=eksd_ar[center]-1+ round(dosis_tiraadighed/finaldosis);
      slutdag=max(slutdag,startdag+1);
	   /* version 27 korrektion af slutdag og restdosis*/
	  slutdag=min(slutdag,eksd_ar[center+1]-1);
	    /*** Beregning af NÆSTE restdosis ***/
	  restdosis=dosis_tiraadighed-finaldosis*((slutdag-startdag)-indltid_ar[center]); 
	  /* 29.10.11 - max restdosis*/
		restdosis=min(&max_depot,restdosis);

      /* version 27 korrektion af slutdag og restdosis passe på på på*/
	  slutdag=min(slutdag,eksd_ar[center+1]-1);
	   /*** Beregning af NÆSTE restdosis ***/
	  restdosis=dosis_tiraadighed-finaldosis*((slutdag-startdag)-indltid_ar[center]);    
      /* 29.10.11 - max restdosis*/
		restdosis=min(&max_depot,restdosis); 

      if &trace then
       put "post 27B - Situation 4-left -  "
        finaldosis=  startdag=   slutdag= ;
    end;
   end;

   /******************************************************************************
      Situation 5 - Anden dosis før - dette er sidste dosis

      sum - samlet dosis
      dage - behandlingsdage (udenfor sygehus)

      dosis_til_raadighed er resterende dosis sendt videre til aktuelle recept

      primdos_ar[] primitiv dosisudregning i hver receptperiode baseret på dosis og tid udskrevet - mellemregning
      receptdosis_ar[] - dosis udleveret fra en recept totalt
      tidl_dos_ar - valgte doser fra tidligere recepter
      dage_ud_sygehus_ar - dage udenfor sygehus
   ************************************************************************************************/
   sum=0; dage=0; dosis_til_raadighed=0;
   if reglsekv_low=center and linkdos_high = center and linkdos_low ne center then
   /*ovenfor: forbindelse nedad med anden dosis - men ikke opad */
   do;
    do i=linkdos_low to center-1;
     sum=sum+receptdosis_ar[i];
      /*ovenfor: summering af udleverede doser*/
     dage=dage+dage_ud_sygehus_ar[i];
     /*ovenfor: summering af dage udenfor sygehus*/
    end;
	dosis_til_raadighed=receptdosis_ar[center];
	if restdosis ne . then
	dosis_til_raadighed=dosis_til_raadighed+restdosis;
	/* dosis til rådighed er receptdosis + evt. rest fra tidl recepter*/
    if dage=0 then dage=1; /* obs flere recepter samme dag */
    meandos=sum/dage; /* middeldosis i HELE intervallet*/
    /*subscenarie 1-2 - meandos er meget stor/lille i forhold til sidste maxdos */
    if meandos>=maxdos_ar[center] then
     finaldosis=maxdos_ar[center];
    if meandos<mindos_ar[center] then
     finaldosis=mindos_ar[center];
	 /* rettelse 8.10.11:*/
	 if meandos=0 then finaldosis=typdos_ar[center];

    /*subscenarie 3 - meandos er imellem max og min */
    if mindos_ar[center]<meandos<maxdos_ar[center]  then
     finaldosis=typdos_ar[center];

	startdag=max(eksd_ar[center],eksd_ar[center-1]+1);
    slutdag=eksd_ar[center]-1+ round(dosis_til_raadighed/finaldosis);
    slutdag=max(slutdag,startdag+1);
	
	/* ny restdosis*/ restdosis=0;
    if &trace then
     put "post 28 - Situation 5 - Afslutning af forløb med ny dosis "
      finaldosis=  startdag=   slutdag= ;
   end;

   /**********************************************************************
	Sluttabel over datoer - og check på datoer
   **************************************************************************/

	if &trace then
	 do;
	  put "  ";
	  put "**   Tabel over datoer og styrker ";
	  do i=1 to &antal_recepter;
	   put i= eksd_ar[i]= tidl_dos_ar[i]=;
	  end;
	  put "Recept slut: " finaldosis= startdag= slutdag= restdosis=;
	 end;

   /***************************************
	 Fejlsøgning
   *****************************************/

   if startdag>slutdag then
    put "  Fejl - Startdag>slutdag   " pnr= startdag= slutdag=;
   if slutdag>eksd_ar[center+1] and eksd_ar[center+1] ne . and eksd_ar[center+1]-startdag>2 /*flere recepter samme dag skubber dagene*/ then
    put " Fejl - Slutdag > næste receptdato  " pnr= startdag= slutdag= eksd_ar[center+1]= ;


   /***************************************************************************************'
   Til slut gemmes finaldoser til beregning af næste recepter
   *************************************************************************************/
   do i=1 to center-2;
    tidl_dos_ar[i]=tidl_dos_ar[i+1];
   end;
    tidl_dos_ar[center-1]=finaldosis;

   run;

   /*******************************************************************************
   Nu benyttes tabellen over start og stop til at generere EN record pr. pnr med
   tabel over behandlingsperioder
   **************************************************************************/

   /* Dette trin beregner det maximale antal BEHANDLINGSPERIODER for hver patient
   Variablen max stiger kun når der er huller i behandlingen eller ændringer
   af dosis*/
   data temp_m3; set temp;
    by pnr;
    retain max l_max;
     if first.pnr then do; max=1; l_max=1; end;
     else
	  do;
       if lag(slutdag)<startdag-1 or lag(finaldosis) ne finaldosis then max=max+1;
	   l_max=l_max+1;
	  end;
   run;
   /* Her beregnes den maximale værdi af max som lægges i datasættet max*/
   proc univariate data=temp_m3 noprint;
    output out=max max=max l_max;
    var max l_max;
   run;
   /* Værdien af max skal bruges som macrovariabel og som det fremgår er det lidt
    nørdet at lægge den beregnede værdi af max i en macrovariabel - men det lykkedes*/
   Data max; set max;
    max=max+1; l_max=l_max+1;
    call symput('max',trim(left(put(max,4.)))); ;
	call symput('l_max',trim(left(put(l_max,4.)))); ;
   run;
   %if not &trace %then
    %do;
    proc datasets library=work;
     delete temp_m3 max ;
    run;
   %end;
   /***************************************************************************************
    Her udregnes det endelige datasæt med en tabel over perioder.
    Tabellen dækker HELE interesseintervallet i tid således at perioder UDEN behandling også
    tabelleres men med dosis=0. En periode slutter ALTID dagen inden næste periode, så perioder
    af en dags varighed har samme start og slut

   antalperioder - en tæller som går op gennem behandlingsperioderne - starter med 1 for hver pt.

   dosis_ar - sekvensen af doser
   startdag_ar - sekvens af startdage
   slutdag_ar - sekvens af slutdage

   
   ****************************************************************************************/
   data &output_data ; set temp;
    by pnr;
     retain dosis1-dosis&max startdag1-startdag&max slutdag1-slutdag&max antalperioder;
	 length dosis1-dosis&max startdag1-startdag&max slutdag1-slutdag&max antalperioder 4;
     array dosis_ar[&max] dosis1-dosis&max;
     array startdag_ar[&max] startdag1-startdag&max;
     array slutdag_ar[&max] slutdag1-slutdag&max;
	 format startdag1-startdag&max slutdag1-slutdag&max  date9.;
     format startdag slutdag date9.; /* de indlæste værdier */ 

     if &trace then
	  put "recept  " finaldosis= startdag= slutdag= ;

     /* Når der startes på en ny patient nulstilles alle retainvariabel og antalperioder
      sættes til en */
     if first.pnr then
      do;
       antalperioder=1;
       do i=1 to &max;
        dosis_ar[i]=.; startdag_ar[i]=.; slutdag_ar[i]=.;
       end;
	    if &trace  then
          put "***post 29 - Første recept for ny patient  " pnr=;
     end;
    
     /********************************************************************************
     Ved første recept genereres EN periode
      *************************************************************************/
    if first.pnr then
    do;
        dosis_ar[antalperioder]=finaldosis;
        startdag_ar[antalperioder]=startdag;
        slutdag_ar[antalperioder]=slutdag;
        if &trace  then
          put "post 30 - Første periode startede ved begyndelsen " 
           dosis_ar[antalperioder]= startdag_ar[antalperioder]=   slutdag_ar[antalperioder]=;
    end;
    /* else - altså ikke første periode for en patient.
    princippet er det samme.
    1. Hvis næste behandlingsperiode blot er en fortsættelse så rettes slutdatoen bare
       svarende til næste recept.
    2. Hvis næste behandlingsperiode starter når den forrige slutter så er der ny indgang
       i tabellen med ny dosis.
    3. Hvis der er en behandlingsfri periode inden næste recept, så genereres først den behandling-
       fri periode siden næste behandlingsperiode
    */
    else
    do;
    /*1*/if finaldosis=dosis_ar[antalperioder] and slutdag_ar[antalperioder]>=startdag-1 then
       /*ovenfor - dosis er uændret - slutdagen for forrige perioder rækker indover denne*/
        do;
         slutdag_ar[antalperioder]=max(slutdag,slutdag_ar[antalperioder]); /* næste dosis er den samme, ingen pause, altså ændres kun slutdato */
         if &trace  then
         put "post 33 - Næste periode samme dosis - ændrer kun slutdato " slutdag_ar[antalperioder]=;
        end;
    /*2*/if slutdag_ar[antalperioder]>=startdag-1 and finaldosis ne dosis_ar[antalperioder]  then
         /* Næste behandlingsperiode - ny dosis - kommer umiddelbar efter */
       do;
        antalperioder=antalperioder+1; /*næste behandlingsperiode */
        dosis_ar[antalperioder]=finaldosis;
        startdag_ar[antalperioder]=max(startdag,slutdag_ar[antalperioder-1]+1);
        slutdag_ar[antalperioder]=max(slutdag,startdag+1);
        if &trace  then
         put "post 34 - Næste periode umiddelbart efter med ny dosering " 
              dosis_ar[antalperioder]= startdag_ar[antalperioder]=   slutdag_ar[antalperioder]=;
       end;

    /*3*/if slutdag_ar[antalperioder]<startdag-1 then
       do;
        antalperioder=antalperioder+1;
        dosis_ar[antalperioder]=finaldosis; /* anden periode bliver behandlingsperiode*/
        startdag_ar[antalperioder]=max(startdag,slutdag_ar[antalperioder-1]+1);
        slutdag_ar[antalperioder]=max(slutdag,startdag_ar[antalperioder]+1);
        if &trace  then
         put "post 36 - Og så behandling efter behandlingsfri periode" 
          dosis_ar[antalperioder]= startdag_ar[antalperioder]=   slutdag_ar[antalperioder]=;
       end;
    end;
    /* Når man står på sidste recept for en patient:
     1. Hvis behandlingen er ophørt, så genereres en behandlingsfri periode fra ophør
        til slut på interesseperioden.
     2. Der laves output til tablellen
    */
    if last.pnr /*sidste recept på dette pnr */ then
     do;
      if &trace  then
         put "***post 37 - Sidste recept " ;
      keep  dosis1-dosis&max  startdag1-startdag&max slutdag1-slutdag&max
            pnr antalperioder;
      output;
     end;
 run;
 /*
 %if not &trace %then
    %do;
    proc datasets library=work;
     delete  temp_m2;
    run;
   %end;
 */

 

  /*********** Sætter praefix på variable *************/

  data &output_data; set &output_data;
   rename antalperioder=&praefix.antalperioder;
   %do i=1 %to &max;
    rename dosis&i=&praefix.dosis&i
           startdag&i=&praefix.startdag&i
           slutdag&i=&praefix.slutdag&i;
   %end;;
  run;

  /******** Final tabel på "den anden led" ***/

  data &output_data._alt; set &output_data;
    array dos_ar[&max] &praefix.dosis1-&praefix.dosis&max; /*medicindosis i hver periode*/
    array startdag_ar[&max] &praefix.startdag1-&praefix.startdag&max; /*første dag i hver periode*/
    array slutdag_ar[&max] &praefix.slutdag1-&praefix.slutdag&max; /*sidste dag i hver periode*/
	if &trace  then
         put "post 39 - Datasæt på den anden led";
     do ii=1 to &praefix.antalperioder;
       dosis=dos_ar[ii];
       startdag=startdag_ar[ii];
       slutdag=slutdag_ar[ii];
       keep pnr dosis startdag slutdag;
       output;
     end;
	 format startdag slutdag date7.;
  run;


 /***************************************************************************************************
   plot output - og final tabel med en record pr. behandlingsperiode og mange records pr. patient
 *****************************************************************************************************/
 %if &plot=1 %then
 %do;
  data plot; set &output_data;
    array dos_ar[&max] &praefix.dosis1-&praefix.dosis&max; /*medicindosis i hver periode*/
    array startdag_ar[&max] &praefix.startdag1-&praefix.startdag&max; /*første dag i hver periode*/
    array slutdag_ar[&max] &praefix.slutdag1-&praefix.slutdag&max; /*sidste dag i hver periode*/
    do dag=&firstdato to &lastdato;
      do ii=1 to &max;
       if dag ge startdag_ar[ii] and dag le slutdag_ar[ii] then
        do;
          dosis=dos_ar[ii];
          if dosis>0 then behandling=1; else behandling=0;  /*behandling i perioden - ja nej */
        end;
      end;
      keep dag dosis behandling;
      output;
    end;
  run;
  proc sort data=plot; by dag; run;
  proc univariate data=plot noprint;
   output out=plotfil mean=dosis behandling std=dosis_std behandling_std;
   var dosis behandling;
   by dag;
  run;
  data plotfil; set plotfil;
    high=dosis_std+dosis_std;
    low=dosis_std-dosis_std;
  run;
  goptions reset=all;
   symbol1 i=j l=1;
   symbol2 i=j l=2;
   symbol3 i=j l=2;
  title1 'Population mean dose and standard deviation';
  proc gplot data=plotfil;
   plot (dosis high low)*dag/overlay;
  run;
  title1 'Population fraction in treatment';
  proc gplot data=plotfil;
   plot behandling*dag;
  run;
  quit;
  title1;
  run;
%end;

%end; /* lukker næsten fra starten*/
%mend;

