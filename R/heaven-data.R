#' SKS codes
#'
#' Several sections of the Sygehusvaesnets Klassifikationssystem (SKS).
#' Retrieved from SKS in October 2018. Consists of B, D, K, M, and U codes.

#' @format A data table with 27864 rows and 2 variables
#' \describe{
#'    \item{kode}{A character vector of the code}
#'    \item{tekst}{A description of the code (in Danish)}
#'    }
#'
#' @source http://medinfo.dk/sks/
#' @docType data
#' @keywords datasets
#' @name d_codes

"sks_codes"

#' diseasecode
#'
#' List with pre-specified disease codes (ATC, ICD10, ICD8)

#' @format A list
#' @docType data
#' @keywords datasets
#' @name diseasecode

"diseasecode"


#' atccodes
#'
#' data.table with unique atc codes
#' @format A data.table
#' @docType data
#' @keywords datasets
#' @name atccodes

"atccodes"

#' icdcodes
#'
#' data.table with unique icd codes
#' @format A data.table
#' @docType data
#' @keywords datasets
#' @name icdcodes

"icdcodes"

#' "edu_code"
#' 
#' data.table with education codes
#' @format A data.table
#' @docType data
#' @keywords datasets
#' @name edu_code
 
"edu_code"

#' "Framingham"
#' 
#' data.frame with Framingham data
#' @format A data.frame
#' @docType data
#' @keywords datasets
#' @name Framingham

"HypertensionATC"

#' hypertensionATC
#'
#' List with pre-specified ATC codes for classes of antihypertensive medication

#' @format A list
#' @docType data
#' @keywords datasets
#' @name hypertensionATC
