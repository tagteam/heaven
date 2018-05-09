##' Function to plot dates etc
##' 
##' Function to plot dates etc
##' @title Plot function for data preprocessing object
##' @param x Data preprocessing object obtained with \code{dpp}
##' @param drug If specified, only data for this object is plotted.
##' @param ... not used
##' @examples
##' library(ggplot2)
##' drugdata <- simPrescriptionData(10)
##' admdata <- simAdmissionData(10)
##' 
##' d <- dpp()
##' 
##' drugdb(d) <- drugdata
##' admdb(d) <- admdata
##' 
##' plot(d)
##' 
##' @return A plot is returned, nothing is changed about the preprocessing object. 
##' @author Helene Charlotte Rytgaard
##' @export
plot.dpp <- function(x, drug=NULL,...) {
    
    if (length(drug) == 0) {
        j <- (1:length(x$drugs))
    } else
        j <- (1:length(x$drugs))[names(x$drugs) == drug]
    
    if (j[length(j)] > 0) {
        atc <- unlist(sapply(j, function(j) x$drugs[[j]]$atc))
    } else 
        atc <- unique(x$drugdb$atc)
    
    if (length(j) == 0) {
        print(paste("no drug named", drug, "in specified input"))
    } else {
        d1 <- data.frame(x$drugdb)
        d1 <- d1[d1$atc %in% atc, ]
        a1 <- data.frame(x$admdb)
        
        # d1 <- d1[x$period[1] <= d1$pdate & d1$pdate <= x$period[2], ]
        d1 <- d1[order(d1$id, d1$pdate), ]
        #-- sort after first date
        
        d1$pfirst <- as.Date(unlist(lapply(unique(d1$id), function(x) rep(d1$pdate[d1$id == x][1], sum(d1$id == x)))), 
                             origin = "1970-01-01")
        d1$plast  <- as.Date(unlist(sapply(unique(d1$id), function(x) rep(d1$pdate[d1$id == x][length(d1$pdate[d1$id == x])], 
                                                                          sum(d1$id == x)))), origin = "1970-01-01")
        
        d1 <- d1[order(d1$pfirst, d1$id, d1$pdate), ]
        
        d1$idorder <- unlist(sapply(1:length(unique(d1$id)), function(i) rep(i, sum(d1$id == unique(d1$id)[i]))))
        
        a1$idorder <- sapply(a1$id, function(x) unique(d1$idorder[d1$id == x]))
        a1$pfirst <- sapply(a1$id, function(x) unique(d1$pfirst[d1$id == x]))
        a1$plast <- sapply(a1$id, function(x) unique(d1$plast[d1$id == x]))
        
        a1 <- a1[order(a1$idorder), ]
        a1 <- a1[a1$pfirst <= a1$uddto & a1$inddto <= a1$plast, ]
        
        d1$idorder <- factor(d1$idorder, labels = unique(d1$id))
        
        natc <- length(unique(d1$atc))
        col <- topo.colors(natc)
        #colorRampPalette(c("darkblue", "darkgreen"))(natc)
        
        if (length(drug) > 0) {
            title <- paste("prescription dates for treatment", drug)
        } else 
            title <- "prescription dates"
        # Check if there actually is some admdates -- might need a better fix if no admdb is allowed later
        if (dim(a1)[1]==0){
            adm.segment <- NULL
            adm.guide <- NULL
        }else{
            adm.segment <- ggplot2::geom_segment(data = a1,
                                                 ggplot2::aes(x = inddto,  xend = uddto,
                                                              y = idorder, yend = idorder, col = "admission periods"))
            adm.guide <- ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape=c(rep(16, natc), NA), 
                                                                                           linetype=c(rep(0, natc), 1))))
        }
        ggplot2::ggplot(data = d1, ggplot2::aes(x = pdate, y = idorder)) +
            ggplot2::geom_point(size = 1.1, aes(col = atc)) + 
                adm.segment +
                ggplot2::xlab("time") +
                ggplot2::ylab("individual") +
                adm.guide +
                ggplot2::scale_color_manual("", values = c(col, "red")) +
                ggplot2::theme_bw() +
                ggplot2::theme(legend.position = "bottom")
    }
} 
