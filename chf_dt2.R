library(data.table)

pop <- read.table(text=
"pid recno start end pattype
1 1 10 25 0
1 29 12 14 0 #Indskudt indlæggelse
1 30 25 26 0 # Fortsat indlæggelse
1 2 30 420 2
1 4 1045 1051 0
2 3 100 110 0
2 31 105 105 0 #Indskudt indlæggelse
3 5 214 222 0
4 6 190 195 0
4 32 195 197 0 #Fortsat indlæggelse
4 7 75 85 0
5 8 261 270 0
6 9 33 40 0
6 10 314 321 0
7 11 61 69 0
8 12 89 97 0
8 13 54 65 2
9 14 102 110 0
10 15 200 204 0
10 16 221 222 0
11 17 121 145 0
12 18 190 194 0
13 19 21 27 0
13 20 31 34 0
14 21 166 171 0
15 22 121 154 0
15 23 199 208 0
16 24 157 165 0
17 25 122 127 0
18 26 223 230 0
19 27 304 324 0
20 28 267 272 0", header=T)
pop$pid<-as.character(pop$pid)
pop<-data.table(pop)
sapply(pop,class)
pop$pid[pop$pid==1]<-"A0302"

#code from hell----
cfh<-function(diag){
  diag$end<-as.numeric(diag$end)
  diag$start<-as.numeric(diag$start)
  diag<-diag[order(diag$pid,diag$start),]
  diag$prev.end <- with(diag, ave(end, pid, FUN=function(pid) head(c(0,pid),-1)))
  diag$prev.start <- with(diag, ave(start, pid, FUN=function(pid) head(c(0,pid),-1)))
  diag$first.start<-ifelse(diag$prev.end>=diag$start,diag$prev.start,diag$start)
  diag$last.end<-pmax(diag$end,diag$prev.end)
  diag$out<-as.character(paste(diag$pid, diag$first.start, sep=""))
  diag<-diag[order(diag$pid,-diag$last.end),]
  diag<-subset(diag,!duplicated(out))
  diag<-diag[order(diag$pid,diag$first.start),]
  diag$start<-diag$first.start
  diag$end<-diag$last.end
  diag[,c('prev.end','prev.start','first.start','last.end','out')]<-list(NULL,NULL,NULL,NULL,NULL)
  diag<-data.table(diag)
}

setkey(pop)

a<-Sys.time()
for(i in 1:100000000){
  old<-nrow(pop)
  pop<-cfh(pop)
  neww<-nrow(pop)
  if (old==neww){
    break
  }
}
b<-Sys.time()
round(b-a,1)
