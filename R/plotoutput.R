##' Function to plot output etc
##' 
##' Function to plot output etc
##' @title Plot output of process()
##' @param x The object outputted from process()
##' @param drug If specified, only the values for these drugs are
##'     plotted.
##' @param id If specified, only the values for these ids are plotted.
##' @param idmax The maximum number of individuals to plot in same
##'     window.
##' @param trace If TRUE, more messages are printed to the user.
##' @param fix_x If TRUE, the x axis is fixed for all considered
##'     individuals. Default is FALSE.
##' @param normalize If TRUE, the values for the drugs are normalized
##'     to the same scale.
##' @param ... not used
##' @examples
##' library(ggplot2)
##' drugdata <- simPrescriptionData(10)
##' admdata <- simAdmissionData(10)
##' 
##' d <- dpp()
##' drugdb(d) <- drugdata
##' admdb(d) <- admdata
##' drug(d, "treatment1") <- atc("A12B")
##' drug(d, "treatment1") <- pack(c(750, 75), 
##'                               min = c(250, 25), 
##'                               max = c(1000, 100),
##'                               def = c(750, 100))
##' 
##' pwindow(d, drug="treatment2") <- 2
##' period(d, drug = "treatment1") <- as.Date(c("1997-01-01", "2012-12-31"))
##' maxdepot(d, drug="treatment1") <- 500
##' 
##' out <- process(d)
##' plot(out)
##' 
##' out <- process(d)
##' plot(out)
##' 
##' @author Helene Charlotte Rytgaard
##' @export
plot.dppout <- function(x, drug=NULL, id=NULL, idmax=9, trace=FALSE, fix_x=FALSE, normalize=FALSE,...) {

  if (length(drug) == 0) {
    j <- (1:length(x)) 
  } else
    j <- (1:length(x))[names(x) == drug]

  drugout <- data.frame(rbindlist(lapply(j, function(j) {
    dat <- x[[j]]
    dat$drug <- rep(names(x)[j], dim(dat)[1])
    return(dat)})))
  
  if (length(id) == 0)
    id <- unique(drugout$id[drugout$id %in% unique(drugout$id)[1:min(idmax, length(unique(drugout$id)))]])
    
  drugout <- drugout[drugout$id %in% id, ]
  
  #period <- as.Date(c(max(attr(out, "period")[1], min(drugout$B)), 
   #                   min(attr(out, "period")[2], max(drugout$E))), origin="1970-01-01")

  period <- as.Date(c(min(drugout$B), 
                      max(drugout$E)), origin="1970-01-01")
  
    
  natc <- length(j)
  col <- #topo.colors(natc)
    colorRampPalette(c("darkblue", "darkgreen"))(natc)
  
  if (length(drug) > 0) {
    title <- paste("final estimated doses for treatment", drug)
  } else 
    title <- "final estimated doses"

  drugout$idname <- paste("id =", drugout$id)
      
  if (attr(x, "out_data")) {
    
    if (normalize) {
      drugout$final <- sapply(1:dim(drugout)[1], function(i)
        drugout$X[i] / max(drugout$X[drugout$drug == drugout$drug[i]]))
    } else
      drugout$final <- drugout$X
    
    ylab <- "estimated dosis per day"
    
    if (normalize)
      ylab <- paste(ylab, "(normalized)")
    
    gout <- ggplot(data = drugout) + 
      ggplot2::geom_rect(data = drugout, aes(xmin = B, xmax = E, ymin = 0, ymax = final, fill = drug), alpha = 0.5) + 
      facet_wrap( ~ idname, scales = "free") + theme_bw() +
      guides(fill = guide_legend(override.aes = list(size=c(rep(6.8, natc))))) +
      scale_fill_manual("treatment: ", values = col) +  
      #ggtitle(title) + 
      theme(legend.position = "bottom") + 
      xlab("purchase dates") + 
      ylab(ylab) + theme(strip.background = element_rect(fill="white"))
      
    if (fix_x) {
      gout <- gout + scale_x_date(limits = period, labels = date_format("%y/%m"))
    } else 
      gout <- gout + scale_x_date(labels = date_format("%b %Y"))
      
    drugdb <- data.frame(attr(x, "drugdb"))
    
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
        ggplot2::geom_hline(yintercept = 0) + 
        ggplot2::geom_point(data = dat, aes(x = pdate, y = y, size = D, col = treatment, alpha = 0.5)) +
        guides(color = guide_legend(override.aes = list(size  = c(rep(8.0, natc)),
                                                        alpha = c(rep(0.5, natc))))) +
        scale_color_manual("treatment (size of bubbles indicative of total amount purchased): ", values = col) + 
        guides(fill = FALSE) + 
        guides(size = FALSE) + guides(alpha = FALSE)
    }
  
  } else {
    
    drugout$B1 <- as.Date(c(sapply(1:(dim(drugout)[1]-1), function(i) {
      if (drugout$u[i] == 1) {
        drugout$B[i+1]
      } else
        drugout$B[i] + drugout$nk[i] + drugout$DH[i]
    }), drugout$B[dim(drugout)[1]] + drugout$nk[dim(drugout)[1]]), origin = "1970-01-01")
    
    B2 <- rbindlist(lapply(1:(dim(drugout)[1]), function(i) {
      points <- seq(drugout$B[i], drugout$B1[i], length = max(as.numeric((drugout$B1[i]-drugout$B[i])/10), 3))
      data.frame(points,
                 drug = rep(drugout$drug, length(points)), 
                 id   = rep(drugout$id, length(points)))
    }))
    
    gout <- ggplot(data = drugout, aes(x = B, y = factor(id))) + ggplot2::geom_point(aes(col = drug)) +
      ggplot2::geom_point(data = B2, aes(x = points, y = factor(id), col = drug)) +
      ggplot2::scale_color_manual("treatment: ", values = col) + 
      xlab("purchase dates") + ylab("id") + 
      ggplot2::geom_segment(data = drugout, aes(x = B,  xend = B1,
                                       y = id, yend = id, col = drug)) +
      ggplot2::facet_wrap( ~ drug, scales = "free") + theme_bw() + 
      ggplot2::geom_vline(xintercept = as.numeric(drugout$B), linetype = "longdash", alpha = 0.3) + 
      ggplot2::theme(legend.position = "bottom") 
      gout
  }
  
  return(gout)
}

