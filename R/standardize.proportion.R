standardize.proportion <- function(vars,
                                   agevar,
                                   byvar,
                                   data,
                                   ref=2,
                                   formula,
                                   cause,
                                   times,
                                   level=0.95,
                                   ...){
    N <- NROW(data)
    agedist <- data[,.(weight=.N/N),by=c(agevar)]
    setkeyv(agedist,agevar)
    if (missing(cause)) cause <- 1
    if (!missing(formula)){
        prop <- data[,{
            varout <- unlist(lapply(.SD[,vars,with=FALSE],function(x){
                m <- mean(x==levels(x)[ref])
                v <- m*(1-m)/.N
                c(m,v)
            }))
            names(varout) <- paste0(c("prop.","varprop."),rep(vars,rep(2,length(vars))))
            fit <- prodlim(formula)
            tind <- predict(fit,times=times,type="list")$indices$time
            fout <- unlist(lapply(cause,function(cr){
                if (!(cr %in% attr(fit$model.response,"states"))){
                    warning("Age group with zero events detected.")
                    c(risk=rep(0,length(tind)),varrisk=rep(NA,length(tind)))
                } else{
                    if (fit$model=="competing.risks"){
                        c(risk=fit$cuminc[[cr]][tind],
                          varrisk=(fit$se.cuminc[[cr]][tind])^2)
                    } else {
                        c(risk=1-fit$surv[tind],
                          varrisk=(fit$se.surv[tind])^2)
                    }
                }}))
            names(fout) <- paste0(names(fout),".",rep(times,rep(2,length(times))))
            as.list(c(varout,fout))
        },.SDcols=c(vars,all.vars(formula)),by=c(byvar,agevar)]
    }else{
        prop <- data[,{
            varout <- unlist(lapply(.SD[,vars,with=FALSE],function(x){
                m <- mean(x==levels(x)[ref])
                v <- m*(1-m)/.N
                c(m,v)
            }))
            names(varout) <- paste0(c("prop.","varprop."),rep(vars,rep(2,length(vars))))
            as.list(varout)},.SDcols=vars,by=c(byvar,agevar)]
    }
    setkeyv(prop,agevar)
    prop <- prop[agedist]
    setkeyv(prop,agevar)
    riskvars <- grep("^risk|^prop",names(prop),value=TRUE)
    varriskvars <- grep("^varrisk|^varprop",names(prop),value=TRUE)
    standardized <- prop[,lapply(.SD,function(x){100*sum(x*weight)}),.SDcols=riskvars,by=c(byvar)]
    se.standardized <- prop[,lapply(.SD,function(x){sqrt(sum(x*weight^2))}),.SDcols=varriskvars,by=c(byvar)]
    pos.byvar <- match(byvar,names(standardized),nomatch=0)
    lower.standardized <- standardized[,-pos.byvar,with=FALSE]+100*qnorm((1-level)/2)*se.standardized[,-pos.byvar,with=FALSE]
    upper.standardized <- standardized[,-pos.byvar,with=FALSE]-100*qnorm((1-level)/2)*se.standardized[,-pos.byvar,with=FALSE]
    setnames(lower.standardized,paste0("lower.",names(lower.standardized)))
    setnames(upper.standardized,paste0("upper.",names(upper.standardized)))
    out <- cbind(standardized,lower.standardized,upper.standardized)
    neworder <- c(byvar,sapply(1:length(riskvars),function(j){c(names(standardized)[-pos.byvar][j],names(lower.standardized)[j],names(upper.standardized)[j])}))
    setcolorder(out,neworder)
    out
}
