## ------------------------------------------------------------------------
 # The following is a minute version of DREAM
 library(data.table)
 library(heaven)
 microDREAM <-data.table(PNR=c(1,2),branche_2008_01=c("","3"),
   branche_2008_02=c("1","3"), branche_2008_03=c("1",""),
   y_9201=c("","1"),y_9202=c("","2"),y_9203=c("","2"))
 microDREAM[]
 # Explanation of codes for "branche" 
 branche <- data.table(branche=c("1","2","3"),
             text=c("school","gardening","suage"))   
 support <- data.table(support=c("1","2"),text=c("education","sick")) 
 temp <- importDREAM(microDREAM,branche,type="branche",pnr="PNR")   
 temp[]
 temp2 <- importDREAM(microDREAM,support,type="support",pnr="PNR")
 temp2[]    

## ------------------------------------------------------------------------
## Example
 library(data.table)
 library(heaven)
 case <- c(rep(0,40),rep(1,15)) 
 ptid <- paste0("P",1:55)
 sex <- c(rep("fem",20),rep("mal",20),rep("fem",8),rep("mal",7))
 byear <- c(rep(c(2020,2030),20),rep(2020,7),rep(2030,8))
 case.Index <- c(seq(1,40,1),seq(5,47,3))
 control.Index <- case.Index
 diabetes <- seq(2,110,2)
 heartdis <- seq(110,2,-2)
 diabetes <- c(rep(1,55))
 heartdis <- c(rep(100,55))
 dat <- data.table(case,ptid,sex,byear,diabetes,heartdis,case.Index,control.Index)
 # Very simple match without reuse - no dates to control for
 out <- riskSetMatch("ptid","case",c("byear","sex"),dat,2,NoIndex=TRUE)
 head(out,10)
 # Risk set matching without reusing cases/controls - 
 # Some cases have no controls
 out2 <- riskSetMatch("ptid","case",c("byear","sex"),dat,2,caseIndex="case.Index",
   controlIndex="control.Index")
 head(out2,10) 
 # Risk set matching with reuse of cases (control prior to case) and reuse of 
 # controls - more cases get controls
 out3 <- riskSetMatch("ptid","case",c("byear","sex"),dat,2,caseIndex=
   "case.Index",controlIndex="control.Index"
   ,reuseCases=TRUE,reuseControls=TRUE)
 head(out3,10)  
 # Same with 2 cores
 out4 <- riskSetMatch("ptid","case",c("byear","sex"),dat,2,caseIndex=
   "case.Index",controlIndex="control.Index"
   ,reuseCases=TRUE,reuseControls=TRUE,cores=2)  
 head(out4,10)   
 #Time dependent matching. In addtion to fixed matching parameters there are
 #two other sets of dates where it is required that if a case has that condi-
 #tion prior to index, then controls also need to have the condition prior to
 #the case index to be eligible - and if the control does not have the condi-
 #tion prior to index then the same is required for the control.
 out5 <- riskSetMatch("ptid","case",c("byear","sex"),dat,2,caseIndex=
   "case.Index",controlIndex="control.Index"
   ,reuseCases=TRUE,reuseControls=TRUE,cores=1,
   dateterms=c("diabetes","heartdis"))  
 head(out5,10)
 #POSTPROCESSING
 #It may be convinient to add the number of controls found to each case in or-
 #der to remove cases without controls or where very few controls have been
 #found.  This is easily obtained using data.table - with the example above:
 #out5[,numControls:=.N,by=caseid] # adds a column with the number of controls
                                  # for each case-ID   


## ------------------------------------------------------------------------
 require(data.table)
 case <- c(rep(0,40),rep(1,15)) 
 ptid <- paste0("P",1:55)
 sex <- c(rep("fem",20),rep("mal",20),rep("fem",8),rep("mal",7))
 byear <- c(rep(c(2020,2030),20),rep(2020,7),rep(2030,8))
 caseIndex <- c(seq(1,40,1),seq(5,47,3))
 controlIndex <- caseIndex
 library(data.table)
 dat <- data.table(ptid,case,sex,byear,caseIndex,controlIndex)
 # Very simple match without reuse - no dates to control for
 dataout <- riskSetMatch("ptid","case",c("byear","sex"),dat,2,caseIndex="caseIndex",
   controlIndex="controlIndex",reuseCases=TRUE,reuseControls=TRUE)
 matchReport(dataout,"ptid","case","caseid")   

## ---- out.width = "400px",echo=FALSE-------------------------------------
knitr::include_graphics("./images/lexisfunctions.png")

## ------------------------------------------------------------------------
 library(data.table)
 
 dat <- data.table(pnr=c("123456","123456","234567","234567","345678","345678"
 ,"456789","456789"),
                 start=as.integer(c(0,100,0,100,0,100,0,100)),
                 end=as.integer(c(100,200,100,200,100,200,100,200)),
                 event=as.integer(c(0,1,0,0,0,1,0,1)))
 split <- data.table (pnr=c("123456","234567","345678","456789"),
 como1.onset=as.integer(c(0,NA,49,50)), como2.onset=as.integer(c(25,75,49,49)),
 como3.onset=as.integer(c(30,NA,49,48)), como4.onset=as.integer(c(50,49,49,47))) 
 #Show the datasets:
 dat[]
 split[]
 lexisTwo(dat # inddato with id/in/out/event
    ,split # Data with id and dates
    ,c("pnr","start","end","event") #names of id/in/out/event - in that order
    ,c("como1.onset","como2.onset","como3.onset","como4.onset")) 
    #Names of date-vars to split by

## ------------------------------------------------------------------------
library(data.table)
dat <- data.table(id=c("A","A","B","B","C","D"),
                 start=as.Date(c(0,100,0,100,200,400),origin='1970-01-01'),
                 end=as.Date(c(100,200,100,200,300,500),origin='1970-01-01'),
                 event=c(0,1,0,0,1,1))
split <- data.table (id=c("A","A","A","A","B","B","B","D","D"),
                    start=as.Date(c(0,50,25,150,110,150,400,300,500),origin='1970-01-01'),
                    end= as.Date(c(25,75,150,151,120,250,500,400,500),origin='1970-01-01'),
                    value=c(1,2,3,4,1,2,3,6,7),
                    name=c("d1","d1","d2","d2","d1","d1","d2","d3","d4"))
#Show the dataset:
dat[]
split[]                   
temp <- lexisFromTo(dat # inddato with id/in/out/event
                   ,split # Data with id and dates
                   ,c("id","start","end","event") #names of id/in/out/event - in that order
                  ,c("id","start","end","value","name")) #Nmes var date-vars to split by
temp[]

## ------------------------------------------------------------------------
library(data.table)
dat <- data.table(ptid=c("A","A","B","B","C","C","D","D"),
                start=c(0,100,0,100,0,100,0,100),
                end=c(100,200,100,200,100,200,100,200),
                dead=c(0,1,0,0,0,1,0,1),
                Bdate=c(-5000,-5000,-2000,-2000,0,0,100,100))
#Example 1 - Splitting on a vector with 3 values to be added to "Bdate"                 
out <- lexisSeq(indat=dat,invars=c("ptid","start","end","dead"),
               varname="Bdate",c(0,150,5000),format="vector")
out[]
#Example 2 - splitting on a from-to-by vector with no adding (calender time?)
out2 <- lexisSeq(indat=dat,invars=c("ptid","start","end","dead"),
                 varname=NULL,c(0,200,50),format="seq",value="myvalue")
out2[]

