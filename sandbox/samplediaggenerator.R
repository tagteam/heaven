#pnr is character
#diag is character

#inddto is Date min=1941 max=2016 
#uddto is Date min=1977 max=2016
#difference between uddto and inddto is min=0;1st.Qu.=0;Median=4;mean=77,3rd Qu. 25; max=19.730 

#diagtype is character +,A,B,C,G,H
#pattype is character 0,1,2,3
#recnum is numeric

sample.diag<-read.table(text="
recnum diag pnr inddto uddto pattype diagtype
1 I21.9 PNR1 10 25 0 A
1 E10.1 PNR1 10 25 0 A
2 410.02 PNR1 30 420 2 B
3 I21.4 PNR2 100 110 0 B
3 I21.2 PNR2 100 110 0 A
3 I63.4 PNR2 100 110 0 H
3 I21.9 PNR2 100 110 0 A
3 I21.6 PNR2 100 110 0 A
4 I63.9 PNR1 1045 1051 0 A
5 I21.2 PNR3 214 222 0 A
5 I63.4 PNR3 214 222 0 H
6 I21.9 PNR4 190 195 0 A
7 I21.6 PNR4 75 85 0 A
7 I64.1 PNR4 75 85 0 +
8 410 PNR5 261 270 0 A
9 I21.1 PNR6 33 40 0 A
10 I63.9 PNR6 314 321 0 A
12 41001 PNR8 89 97 0 B
13 E11.4 PNR8 54 65 2 A
14 I21.4 PNR9 102 110 0 A
15 I21.9 PNR10 200 204 0 A
16 I21.9 PNR10 221 222 0 A
17 E11.9 PNR11 121 145 0 B
18 I21.9 PNR12 190 194 0 A
19 E11.9 PNR13 21 27 0 A
20 I21.9 PNR13 31 34 0 B
21 I21.9 PNR14 166 171 0 A
22 I21.9 PNR15 121 154 0 A
23 250 PNR15 199 208 0 A
24 I21.9 PNR16 157 165 0 H
25 I21.9 PNR17 122 127 0 +
26 25009 PNR18 223 230 0 A
27 I21.9 PNR19 304 324 0 A
28 I21.9 PNR20 267 272 0 A
29 I21.9 PNR1 12 14 0 B
30 I21.9 PNR1 25 26 0 A
31 I21.9 PNR2 105 105 0 A
32 I21.9 PNR4 195 197 0 B
33 000 PNR21 2000 2000 3 B
34 I21.1B PNR22 2010 2011 0 A 
34 I00 PNR22 2011 2011 0 A
34 I00 PNR22 2011 2011 0 A
34 I00 PNR22 2011 2011 0 A
35 I21.1B PNR23 1 20001 0 A
36 I21.1B PNR24 2000 2000 0 A
37 I21.1B PNR25 1000 980 0 A
60 I21.3 PNR1 11 15 0 A 
50 I21.3 PNR1 13 13 0 A 
65 I21.3 PNR1 10 23 0 A 
102 I21.3 PNR1 15 20 0 A
103 25009 PNR26 20000 20010 0 A
",header=T)


sample.diag$pnr<-as.character(sample.diag$pnr)
sample.diag$diag<-as.character(sample.diag$diag)
sample.diag$diagtype<-as.character(sample.diag$diagtype)
sample.diag$pattype<-as.character(sample.diag$pattype)

sample.diag$recnum<-as.numeric(sample.diag$recnum)

sample.diag$inddto<-as.Date(sample.diag$inddto,origin="1970-01-01")
sample.diag$uddto<-as.Date(sample.diag$uddto,origin="1970-01-01")


str(sample.diag)

sample.diag <- rbind(sample.diag, sample.diag, sample.diag)

sapply(sample.diag,class)

 