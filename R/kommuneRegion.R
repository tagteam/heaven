#' @title kommuneRegion - Converts kommune number to Region
#' 
#' @description  
#' Converts 3-digit kommune number to text representing one of the five 
#' administrative regions.
#' @usage kommuneRegion(x)
#' @param x 3-digit kommune number
#' @author Christian Torp-Pedersen 
#' @details 
#' This function converts the 3-digit kommune number to a text corresponding to
#' which of the five regions in Denmark a kommune belongs to. The function is
#' adapted from a DST SAS-format and the conversions are valid from 2007. There 
#' is no very easy way to convert older kommune-numbers to the current regions
#' since some kommune were split in the processof developing the five regions
#' @export
#' @examples
#' kommuneRegion(410) # Sydjylland
kommuneRegion <- function(kommune){
  if(!is.numeric(kommune)) stop('Input to kommuneRegion must be integer')
  region <-data.table::fifelse(kommune %in% c(773,787,810,813,820,825,840,846,849,851,860),'Nordjylland',
        data.table::fifelse(kommune %in% c(101,147,151,153,155,157,159,161,163,165,167,169,173,175,183,185,187,190,
          201,210,217,219,223,230,240,250,260,270,400,411),"Hovedstaden",
        data.table::fifelse(kommune %in% c(253,259,265,269,306,316,320,326,329,330,336,340,350,360,370,376,390)
          ,'Sj?lland',
        data.table::fifelse(kommune %in% c(615,657,661,665,671,706,707,710,727,730,740,741,746,751,756,760,766,
          779,791),'Midtjylland',
        data.table::fifelse(kommune %in% c(410,420,430,440,450,461,479,480,482,492,510,530,540,550,561,563,573,575,
          580,607,621,630),'Sydjylland',
      '')))))
  region
}