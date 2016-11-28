##' Function to plot output etc
##' 
##' Function to plot output etc
##' @title Update database
##' @param dpp a
##' @param id a
##' @param trace a
##' @author Helene Charlotte Rytgaard
##' @export
plot.dppout <- function(out, drug=NULL, id=NULL, idmax=9, trace=FALSE, fix_x=FALSE, normalize=FALSE) {

  if (length(drug) == 0) {
    j <- (1:length(out)) 
  } else
    j <- (1:length(out))[names(out) == drug]

  drugout <- data.frame(rbindlist(lapply(j, function(j) {
    dat <- out[[j]]
    dat$drug <- rep(names(out)[j], dim(dat)[1])
    return(dat)})))
  
  if (length(id) == 0)
    id <- unique(drugout$id[drugout$id %in% unique(drugout$id)[1:min(idmax, length(unique(drugout$id)))]])
    
  drugout <- drugout[drugout$id %in% id, ]
  
  period <- as.Date(c(max(attr(out, "period")[1], min(drugout$B)), 
                      min(attr(out, "period")[2], max(drugout$E))), origin="1970-01-01")
  
  natc <- length(j)
  col <- #topo.colors(natc)
    colorRampPalette(c("darkblue", "darkgreen"))(natc)
  
  if (length(drug) > 0) {
    title <- paste("final estimated doses for treatment", drug)
  } else 
    title <- "final estimated doses"

  drugout$idname <- paste("id =", drugout$id)
      
  if (attr(out, "out_data")) {
    
    if (normalize) {
      drugout$final <- sapply(1:dim(drugout)[1], function(i)
        drugout$X[i] / max(drugout$X[drugout$drug == drugout$drug[i]]))
    } else
      drugout$final <- drugout$X
    
    ylab <- "estimated dosis per day"
    
    if (normalize)
      ylab <- paste(ylab, "(normalized)")
    
    gout <- ggplot(data = drugout) + 
      geom_rect(data = drugout, aes(xmin = B, xmax = E, ymin = 0, ymax = final, fill = drug), alpha = 0.5) + 
      facet_wrap( ~ idname, scales = "free") + theme_bw() +
      guides(fill = guide_legend(override.aes = list(size=c(rep(10.8, natc))))) +
      scale_fill_manual("treatment: ", values = col) +  
      #ggtitle(title) + 
      theme(legend.position = "bottom") + 
      xlab("purchase dates") + 
      ylab(ylab) + theme(strip.background = element_rect(fill="white"))
      
    if (fix_x) {
      gout <- gout + scale_x_date(limits = period, labels = date_format("%y/%m"))
    } else 
      gout <- gout + scale_x_date(labels = date_format("%b %Y"))
      
    drugdb <- data.frame(attr(out, "drugdb"))
    
    if (trace & length(drugdb) > 0 & length(id) == 1) {
      
      dat <- drugdb[drugdb$id == id, ]
      if (length(drug) > 0)
        dat <- dat[dat$treatment == drug, ]
      dat$y <- rep(0, dim(dat)[1])
      dat$D <- dat$strength * dat$npack * dat$ppp
      dat$D <- sapply(1:(dim(dat)[1]), function(i) dat$D[i] / max(dat$D[dat$treatment == dat$treatment[i]]))

      gout <- gout + theme(plot.margin = unit(c(1, 1, 0, 1), "lines")) + 
        #scale_y_continuous(limits = c(-10, 100)) + 
        #scale_y_continuous(expand = c(0, 0)) + 
        geom_hline(yintercept = 0) + 
        geom_point(data = dat, aes(x = pdate, y = y, size = D, col = treatment, alpha = 0.5)) +
        guides(color = guide_legend(override.aes = list(size  = c(rep(8.0, natc)),
                                                        alpha = c(rep(0.5, natc))))) +
        scale_color_manual("treatment (size of bubbles indicative of total amount purchased): ", values = col) + 
        guides(fill = FALSE) + 
        guides(size = FALSE) + guides(alpha = FALSE)
    }
  
  } else {
    
    print("lala")
  }
  
  
  return(gout)
}

