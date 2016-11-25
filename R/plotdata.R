##' Function to plot dates etc
##' 
##' Function to plot dates etc
##' @title Update database
##' @param dpp a
##' @param id a
##' @param trace a
##' @author Helene Charlotte Rytgaard
##' @export
plot.dpp <- function(dpp, drug=NULL) {
  
  if (length(drug) == 0) {
    j <- (1:length(dpp$drugs))
  } else
    j <- (1:length(dpp$drugs))[names(dpp$drugs) == drug]
  
  if (j[length(j)] > 0) {
    atc <- unlist(sapply(j, function(j) dpp$drugs[[j]]$atc))
  } else 
    atc <- unique(dpp$drugdb$atc)
  
  if (length(j) == 0) {
    print(paste("no drug named", drug, "in specified input"))
  } else {
    d1 <- data.frame(dpp$drugdb)
    d1 <- d1[d1$atc %in% atc, ]
    a1 <- data.frame(dpp$admdb)
    
    d1 <- d1[dpp$period[1] <= d1$pdate & d1$pdate <= dpp$period[2], ]
    d1 <- d1[order(d1$id, d1$pdate), ]
    #-- sort after first date
    
    d1$pfirst <- as.Date(unlist(lapply(unique(d1$id), function(x) rep(d1$pdate[d1$id == x][1], sum(d1$id == x)))), 
                         origin = "1970-01-01")
    d1$plast  <- as.Date(unlist(sapply(unique(d1$id), function(x) rep(d1$pdate[d1$id == x][length(d1$pdate[d1$id == x])], 
                                                                      sum(d1$id == x)))), origin = "1970-01-01")
    
    d1 <- d1[order(d1$pfirst, d1$id, d1$pdate), ]
    d1
    d1$idorder <- unlist(sapply(1:length(unique(d1$id)), function(i) rep(i, sum(d1$id == unique(d1$id)[i]))))
    
    a1$idorder <- sapply(a1$id, function(x) unique(d1$idorder[d1$id == x]))
    a1$pfirst <- sapply(a1$id, function(x) unique(d1$pfirst[d1$id == x]))
    a1$plast <- sapply(a1$id, function(x) unique(d1$plast[d1$id == x]))
    
    a1 <- a1[order(a1$idorder), ]
    a1 <- a1[a1$pfirst <= a1$uddto & a1$inddto <= a1$plast, ]
    
    d1$idorder <- factor(d1$idorder, labels = unique(d1$id))
    
    natc <- length(unique(d1$atc))
    col <- topo.colors(natc)
    
    if (length(drug) > 0) {
      title <- paste("prescription dates for treatment", drug)
    } else 
      title <- "prescription dates"
    
    ggplot(data = d1, aes(x = pdate, y = idorder)) + geom_point(size = 1.1, aes(col = atc)) + 
      geom_segment(data = a1, aes(x = inddto,  xend = uddto,
                                  y = idorder, yend = idorder, col = "admission periods")) + 
      xlab("time") + ylab("individual") +
      guides(color = guide_legend(override.aes = list(shape=c(rep(16, natc), NA), 
                                                      linetype=c(rep(0, natc), 1)))) + 
      scale_color_manual("", values = c(col, "red")) + theme_bw() +
      theme(legend.position = "bottom")
  }
} 
