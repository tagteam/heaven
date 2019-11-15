## Part of the \code{medicinMacro}
##
## @title Calculates whether patients are exposed or not at a given time
## @param drugs 
## @param drugdb 
## @param admdb 
## @param time.points 
## @param window 
## @param method 
## @param stash 
## @param cap.values 
## @return 
## @author Anders Munch
mm1 <- function(drugs,
                drugdb,
                admdb,
                time.points,
                window,
                method="default.doses",
                stash=0,
                cap.values=TRUE,
                verbose=FALSE){
    time=last.potential.purch=eksd=pnr=exposed.at.time=drug.supply.days=h.days.since.potential.purch=hospitalized.at.time=NULL
    ind.periods <- copy(time.points)
    ind.periods[,":="(start=time-window, end=time)][,time:=NULL]
    exp.calc <- mm2(drugdb=drugdb,drugs=drugs,periods=ind.periods,admdb=admdb,method=method,stash=stash,cap.values=TRUE,pre.window=0,verbose=verbose)
    if(method=="number.of.days" & stash > 0)
        return(exp.calc) ## Should be NULL, warning given from mm2
    ## NB: _potential_ purchases, becuase a ptt's might not purchase anything in the time frame -- this gives a total.drug and drug.supply.days of 0
    last.dates <- exp.calc[,last.potential.purch:=max(eksd),by=pnr][last.potential.purch==eksd]
    cols.to.keep <- c("pnr","end","last.potential.purch",
                      "total.drug","average.drug","drug.strength.estimate",
                      "min.dose","max.dose","def.dose",
                      "estimated.daily.dose","drug.supply.days")    
    if(!is.null(admdb)){
        cols.to.keep <- c(cols.to.keep,"hospitalized.at.end","h.days")
        out <- last.dates[,cols.to.keep,with=FALSE]
        setnames(out, c("end","hospitalized.at.end","h.days"), c("time","hospitalized.at.time","h.days.since.potential.purch"))
        out[,exposed.at.time:=(time<=(last.potential.purch+drug.supply.days+h.days.since.potential.purch))]
        out[hospitalized.at.time==TRUE,exposed.at.time:=TRUE]
    }
    else{
        out <- last.dates[,cols.to.keep,with=FALSE]
        setnames(out, "end", "time")
        out[,exposed.at.time:=(time<=(last.potential.purch+drug.supply.days))]
    }
    return(out[])
}
