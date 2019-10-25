##' Part of the \code{medicinMacro}
##' 
##' @title Calculates exposure periods in a given time span
##' @param drugs 
##' @param drugdb 
##' @param admdb 
##' @param periods 
##' @param method 
##' @param stash 
##' @param cap.values 
##' @param pre.window 
##' @param print.time 
##' @return data.table
##' @author Anders Munch
mm2 <- function(drugs,
                drugdb,
                admdb,
                periods,
                method="default.doses",
                stash=0,
                cap.values=TRUE,
                pre.window=365.25*2,
                print.time=FALSE,
                verbose=FALSE){
    if(!(method %in% c("default.doses", "number.of.days")))
        stop("Choose either method = \"default.doses\", or \"number.of.days\".")
    ## Helper functions
    T <- as.numeric(proc.time()[3])
    cap <- function(x, min, max){
        ## restricts x to lie within min and max
        ifelse(x < min, min,
        ifelse(x > max, max,
               x))
    }
    discretize <- function(x, grid, lower=TRUE){
        ## Find smallest value in the grid, that is larger than x
        grid <- sort(grid)
        if(lower==TRUE){
            values <- grid[grid <= x]
            if(length(values)>0)
                out <- tail(values, 1)
            else
                out <- grid[1]
        }else{
            print("not implemented")
        }
        return(out)
    }  
    #### 1: Take out the relevant period and calculate non-hopsitalized days
    ## Probably should be a better way than this?:
    ## Restrict the data
    t <- as.numeric(proc.time()[3])
    drugdb2 <- merge(drugdb, periods, by = "pnr", all.x=TRUE)
    restr.drugdb <- drugdb2[start-pre.window <= eksd & eksd <= end]
    if(is.null(restr.drugdb)) stop("No data in the specified periods.")
    ## Make periods unique
    unique.drugdb <- restr.drugdb[, .(n.purchases = .N,
                                  total.drug = sum(strnum*packsize*apk),
                                  average.drug = sum(strnum*packsize*apk)/sum(packsize*apk),
                                  start=start[1],
                                  end=end[1]),
                              by = .(pnr,eksd)]
    ## Only keep latest purchase before start
    unique.drugdb[,keep:=1]
    setorder(unique.drugdb, pnr, eksd) 
    unique.drugdb[eksd<=start, keep:=c(rep(0,.N-1),1), by=pnr]
    unique.drugdb <- unique.drugdb[keep==1][,keep:=NULL]
    ## Add end of each period (= beginning of the next so far)
    setorder(unique.drugdb, pnr, eksd) 
    unique.drugdb[, eksd.end:=c(.SD[,eksd][-1], .SD[,end][.N]), by=pnr]
    ## Add ptt.'s with no purhcases (which might still be hospitalized in the period!):
    no.purch <- drugdb2[!(drugdb2$pnr %in% unique(unique.drugdb$pnr))]
    no.purch <- no.purch[,.SD[1],by=pnr]
    no.purch[,c("atc","strnum","packsize","apk"):=NULL]
    no.purch[,":="(eksd=start,n.purchases=0,total.drug=0,average.drug=0,start=start,end=end,eksd.end=end)]
    ## Also use this place to add 0-exposure periods to patient with a gap from start to first purchase
    no.purch2 <- copy(unique.drugdb)
    no.purch2 <- no.purch2[,first.purch:=min(eksd),by=pnr][first.purch > start][first.purch==eksd][,first.purch:=NULL]
    no.purch2[,eksd.end:=eksd]
    no.purch2[,eksd:=start]
    no.purch2[,c("n.purchases","total.drug","average.drug"):=rep(list(0),3)]
    unique.drugdb <- rbind(unique.drugdb, no.purch, no.purch2)
    setcolorder(unique.drugdb, c(names(unique.drugdb)[1:2], names(unique.drugdb)[length(names(unique.drugdb))], names(unique.drugdb)[3:(length(names(unique.drugdb))-1)]))
    unique.drugdb[,days.in.period:=eksd.end-eksd]   
    ## The following is for handling edge case with hospitalization after start date within first purchase period.
    unique.drugdb[eksd<start, pre.period:=1]
    unique.drugdb[is.na(pre.period), pre.period:=0]
    if(print.time) print(paste("Restricting and find unique purch. data took:", as.numeric(proc.time()[3])-t))
    ## If supplied, calculate non-hospitalized days and add the info
    if(!is.null(admdb)){
        t <- as.numeric(proc.time()[3])
        ## TODO: maybe remove the hospitalization dates outside the periods before forming the cartesian product
        hos.periods <- merge(unique.drugdb[,.(pnr,eksd,eksd.end,days.in.period,start,pre.period,end)], admdb[,.(pnr,inddto,uddto)], by = "pnr", all.x=TRUE, allow.cartesian = TRUE)
        hos.periods[,hospitalized.at.end:=(inddto<=end & end <=uddto)] 
        hos.periods[!is.na(inddto),
                    ":="(hos.days=cap(uddto, eksd, eksd.end)-cap(inddto, eksd, eksd.end),
                         hos.days.after.start=ifelse(pre.period==1,
                                                     cap(uddto, start, eksd.end)-cap(inddto, start, eksd.end),
                                                     as.numeric(NA)))]
        hos.periods[is.na(hos.days),hos.days:=0]
        hos.periods[pre.period==1 & is.na(hos.days.after.start),hos.days.after.start:=0]
        hos.periods.unique <- hos.periods[,.(h.days = sum(hos.days),
                                             h.days.after.start = sum(hos.days.after.start),
                                             non.h.days=days.in.period[1]-sum(hos.days),
                                             non.h.days.after.start=(eksd.end-start)[1]-sum(hos.days.after.start),
                                             hospitalized.at.end=any(hospitalized.at.end,na.rm=TRUE)),
                                          by=.(pnr,eksd)]
        all.data <- merge(unique.drugdb, hos.periods.unique, by = c("pnr", "eksd"))
        if(print.time) print(paste("Finding hospitalization periods took:", as.numeric(proc.time()[3])-t))
    }else{
        all.data <- unique.drugdb
        all.data[,":="(h.days = 0,
                       non.h.days=days.in.period)]
        all.data[pre.period==1,":="(h.days.after.start = 0,
                                    non.h.days.after.start=eksd.end-start)]
    }
    ## Add on the drug info
    t <- as.numeric(proc.time()[3])
    all.data[, drug.strength.estimate:=
                   sapply(average.drug, function(x) discretize(x, drugs$doses$value))]
    all.data[,ind:=sapply(drug.strength.estimate,
                          function(x) which(x==drugs$doses$value))]
    all.data[,c("min.dose", "max.dose", "def.dose"):=
                  lapply(drugs$doses[-1], function(y) y[ind])][,ind:=NULL]
    ## Overwrite/special case for no purchasers:
    set0 <- c("drug.strength.estimate","min.dose","max.dose","def.dose")
    all.data[n.purchases==0, (set0):=rep(list(0),length(set0))]
    if(print.time) print(paste("Adding drug info took:", as.numeric(proc.time()[3])-t))
    #### 2: Calculate end points and daily dose in different cases
    t <- as.numeric(proc.time()[3])
    if(method=="number.of.days" & stash > 0){
        if(verbose){
            cat("\nCalculating exposure based on purchased drug strength and number of days.")
            cat("\nIt is assumed that the patients can have a stash of", stash, ".")
        }
        warning("\nMethod for calculating with maxdepot > 0 is not implemented yet, returning NULL.")
        return(NULL)
    }else{
        if(method=="default.doses"){
            if(verbose) cat("Calculating exposure based on the default dosis only.")
            all.data[,estimated.daily.dose:=def.dose]
            ## NB: non-purchases or total.drug==0 is might be given default doses here, but this is restored below.
        }else{
            # In this case, use number.of.days with stash=0:
            if(verbose){
                cat("\nCalculating exposure based on purchased drug strength and number of days.")
                cat("\nIt is assumed that the patients have no stash of drugs.")
            }
            all.data[,estimated.daily.dose:=ifelse(as.numeric(non.h.days)==0, def.dose, total.drug/as.numeric(non.h.days))]
            if(cap.values)
                all.data[,estimated.daily.dose:= cap(estimated.daily.dose, min.dose, max.dose)]
            ## The last period should be based on the previous estimate, as we don't know, when the period ends (so can't give a proper daily dose estimate in this case)
            ## So overwrite previous estimate from last period.
            ## Handle case with only one period (set the estimate to the default dosis)
            all.data[,n.periods:=.N,by=pnr]
            setorder(all.data, pnr, eksd)
            all.data[,estimated.daily.dose:=ifelse(n.periods>1,
                                                   c(estimated.daily.dose[-length(estimated.daily.dose)],
                                                     max(def.dose[length(estimated.daily.dose)], estimated.daily.dose[length(estimated.daily.dose)-1])),
                                                   def.dose[1]),
                     by=.(pnr)]
            ## NB: non-purchases or total.drug==0 might be given default doses here, but this is restored below.
            all.data[,n.periods:=NULL]
        }
        ## Clean up for the edge case of the pre.period: Re-estimate the total drug at start point by substracting the estimated amount of drug spend before start date.
        all.data[pre.period==1,total.drug:=pmax(0,total.drug - estimated.daily.dose * (non.h.days-non.h.days.after.start))]
        ## Then overwrite and drop .after.start info for the pre.periods. 
        all.data[pre.period==1,":="(eksd=start,
                                    days.in.period = eksd.end-start,
                                    h.days = h.days.after.start,
                                    non.h.days = non.h.days.after.start)][,":="(h.days.after.start=NULL,
                                                                                non.h.days.after.start=NULL,
                                                                                pre.period=NULL)]
        ## Also overwrite drug info for the cases with no drug left at the start date to not cause confusion. 
        all.data[total.drug==0, c("n.purchases","average.drug","drug.strength.estimate","min.dose","max.dose","def.dose","estimated.daily.dose"):=rep(list(0),7)]
        ## From this we calculate other summary measures:
        all.data[,drug.supply.days:=total.drug/estimated.daily.dose]
        ## Set n.purchases==0 to estimated dose and days.supply  = 0 (as they are capped or set to default above above):
        all.data[n.purchases==0, c("estimated.daily.dose","drug.supply.days"):=rep(list(0),2)]
        ## Also leave out period with 0 days (appears because of the ending dates):
        all.data <- all.data[eksd!=eksd.end]
        all.data[,":="(leftover = pmax(0,total.drug - non.h.days*estimated.daily.dose),
                       prop.covered.period = as.numeric(pmin(days.in.period, h.days+drug.supply.days)/as.numeric(days.in.period)))]
        all.data[,prop.covered.total := as.numeric(sum(pmin(days.in.period, h.days+drug.supply.days))/as.numeric(sum(days.in.period))), by=.(pnr)]
        out <- all.data[,n.purchases:=NULL]
    }
    if(verbose) cat("\n")
    if(print.time) print(paste("Summarizing took:", as.numeric(proc.time()[3])-t))
    if(print.time) print(paste("Overall time=", as.numeric(proc.time()[3])-T))
    setorder(out, pnr, eksd)
    return(out[])
}

