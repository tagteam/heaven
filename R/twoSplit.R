## library(data.table)
## dat <- data.table(id=c("A","A","B","B","C","C","D","D"),
                 ## start=c(0,100,0,100,0,100,0,100),
                 ## end=c(100,200,100,200,100,200,100,200),
                 ## event=c(0,1,0,0,0,1,0,1))

## split <- data.table (id=c("A","B","C","D"),date1=c(0,50,150,300),date2=c(25,75,175,325),date3=c(30,30,30,30),
                     ## date4=c(0,1,0,1))
#dat2 <- dat[,cbind(rownum=1:1000,.SD),by=1:nrow(dat)]



twoSplit <- function(indat # inddato with id/in/out/event - and possibly other variables
                    ,splitdat # Data with id and dates
                    ,invars #names of id/in/out/event - in that order
                    ,splitvars #Nmes var date-vars to split by
                     ){
    ## browser()  
    require(data.table)
    require(zoo)
    copyindat <- copy(indat)
    #Tests of data
    if (!is.data.table(indat) | !is.data.table(splitdat)) stop("Input not data tables")
    INDAT <- copyindat[,invars,with=F] # Ncessary variables for split
    setnames(INDAT,invars,c("pnr","inn","out","dead"))
    RESTDAT <- copyindat[,(invars[2:4]):=NULL]# Other variables to be added at end
    RESTDAT <- unique(RESTDAT) # Assuming prior splits with identical other variables
    setnames(RESTDAT,invars[1],"pnr")
    OUT <- INDAT[,c("pnr","inn"),with=F] # Prepare output start
    setnames(splitdat,invars[1],"pnr")
    for(name in splitvars){
        selected <- splitdat[,c("pnr",name),with=F]
        toSplit <- merge(INDAT,selected,by="pnr",all.x=T)
        .pnr <- toSplit[["pnr"]]
        .in <- toSplit[["inn"]]
        .out <- toSplit[["out"]]
        .event <- toSplit[["dead"]]
        .dato <- toSplit[[name]]
        if (name != splitvars[1]) OUT[,(c("out","dead")):=NULL]
        INDAT <- heaven::split2(.pnr,.in,.out,.dato,.event)  # Call to split-function
        OUT <- merge(INDAT,OUT,by=c("pnr","inn"),all=TRUE) 
        OUT <- OUT[,tail(.SD,1),by=c("pnr","inn","out")]
        #    OUT <- OUT[,tail(.SD,1),by=c("pnr","inn","out")] # Yes twice
        INDAT[,.dato:=NULL]
        setnames(OUT,".dato",name)
    }
    OUT[,(splitvars) := na.locf(.SD, na.rm = F), by = "pnr", .SDcols = splitvars]  
    OUT <- merge(OUT,RESTDAT,by="pnr")
    setnames(OUT,c("pnr","inn","out","dead"),invars)
    OUT
}



## temp <- twoSplit(dat # inddato with id/in/out/event
        ## ,split # Data with id and dates
        ## ,c("id","start","end","event") #names of id/in/out/event - in that order
        ## ,c("date1","date2","date3","date4")) #Nmes var date-vars to split by

#Rprof(tmp <- tempfile())
#splitdat <- dat2[,cbind(split2(inn,out,dato,d.event,dead),.SD),by=1:nrow(dat2)]
#Rprof()
#summaryRprof(tmp)


