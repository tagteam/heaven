#' @title kommuneRegion - Converts kommune number to Region
#'
#' @description
#' Converts a 3-digit kommune number to text representing one of the five
#' administrative regions.
#'
#' @param kommune 3-digit kommune number.
#'
#' @details
#' This function converts a 3-digit kommune number to text corresponding to
#' one of the five regions in Denmark. The conversion is adapted from a DST
#' SAS format and is valid from 2007. There is no easy way to convert older
#' kommune numbers to the current regions, since some municipalities were split
#' in the process of creating the five regions.
#'
#' @author Christian Torp-Pedersen
#'
#' @examples
#' kommuneRegion(410) # Sydjylland
#'
#' @export
kommuneRegion <- function(kommune){
  if(!is.numeric(kommune)) stop('Input to kommuneRegion must be integer')
  region <- data.table::fifelse(kommune %in% c(773,787,810,813,820,825,840,846,849,851,860), 'Nordjylland',
                                data.table::fifelse(kommune %in% c(101,147,151,153,155,157,159,161,163,165,167,169,173,175,183,185,187,190,
                                                                   201,210,217,219,223,230,240,250,260,270,400,411), "Hovedstaden",
                                                    data.table::fifelse(kommune %in% c(253,259,265,269,306,316,320,326,329,330,336,340,350,360,370,376,390),
                                                                        'Sjaelland',
                                                                        data.table::fifelse(kommune %in% c(615,657,661,665,671,706,707,710,727,730,740,741,746,751,756,760,766,
                                                                                                           779,791), 'Midtjylland',
                                                                                            data.table::fifelse(kommune %in% c(410,420,430,440,450,461,479,480,482,492,510,530,540,550,561,563,573,575,
                                                                                                                               580,607,621,630), 'Sydjylland',
                                                                                                                '')))))
  region
}