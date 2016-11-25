##' Function to plot output etc
##' 
##' Function to plot output etc
##' @title Update database
##' @param dpp a
##' @param id a
##' @param trace a
##' @author Helene Charlotte Rytgaard
##' @export
plot.dppout <- function(out, drug=NULL, id=NULL, idmax=9) {

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
  
  natc <- length(unique(d1$atc))
  col <- topo.colors(natc)
  
  if (length(drug) > 0) {
    title <- paste("final estimated doses for treatment", drug)
  } else 
    title <- "final estimated doses"

  drugout$idname <- paste("id =", drugout$id)
      
  if (attr(out, "type")) {

    gout <- ggplot(data = drugout, aes(xmin = B, xmax = E, ymin = 0, ymax = X)) + 
      geom_rect(aes(fill = drug), alpha = 0.5) + 
      facet_wrap( ~ idname, scales = "free") + theme_bw() +
      scale_fill_manual("treatment: ", values = col) + 
      ggtitle(title) + theme(legend.position = "bottom") +
      scale_x_date()
  
  } else {
    
    print("lala")
  }
  
  
  
  return(gout)
}

