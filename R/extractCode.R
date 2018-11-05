#' @title Extraction of diseases by diagnoses
#' @description Filtering of LPR registry data according to a given set of diseases, or filtering of medical prescription data according to given atc codes.
#' @param dat Dataset containing diagnoses/atc codes, entrydate, patient id, patient type, record number and optionally index date. If extraction of atc codes is desired, patient type and record number is not expected in the input data (see lmdb).
#' @param disease Characterstring containing pre-specified name of diseases. See \href{../doc/predefined_diseases.pdf}{definitions of diseases}. 
#' @param inclusions Characterstring, where additional diagnoses/atc codes can be included. If disease is not specified, inclusions will be the extracted codes.
#' @param exclusions Characterstring, specifying codes to be omitted. If disease and inclusions are not specified, all codes except exclusions will be extracted.
#' @param keep Takes the values "first" or "last". Specifies if only the first or last record of each patient should be output. 
#' If multiple diseases are chosen and keep="first" the first code of each disease specified will be extracted.
#' @param p.in Date of period start, if a specific period of time is desired. Characterstring in the format "YYYY-MM-DD". 
#' @param p.out Date of period end. See p.in.
#' @param pat Number or vector defining types of patients to include (pattype: 0,1,2,3), default is all types.
#' @param prefix Character string of prefix name for the resulting date variable of disease. If not specified, and disease is specified, the name in disease is chosen as prefix.
#' @param entryvar Name of the variable in data that contains the entrydate/prescription date of a diagnosis/atc code.
#' @param id Name of the variable in data that contains patient id.
#' @param codevar Name of the variable in data that contains diagnoses or atc codes.
#' @param patvar Name of the variable in data that contains the type of patient.
#' @param indexvar Name of the variable in data that contains the index date.
#' @param index.int Numeric. Number of days before or after an index date to search for specific diagnoses/atc codes. If negative, days before index date. If positve, days after index date (i.e. outcome).
#' @param lmdb Logical. If true, data with atc codes is expected, and and patvar are not expected to be in the data.
#' @details Extracts specific selected ICD- or ATC codes, or predefined diseases by diagnoses. If specified by keep, only the first or last occurrence of the code is extracted.
#' @return A list of three elements. data: the extracted data. codes: contains the codes specified 
#' to be extracted. unique.codes: contains every unique code extracted. If exclusions are specified, these are contained in an additional list element: excl. 
#' @examples 
#' \dontrun{
#' 
#' # Simulated LPR-registry data
#' set.seed(2976)
#' lpr.data <- simAdmissionData(100)
#' 
#' # Extract diagnoses related to heart failure
#' dat.extracted <- extractCode(lpr.data,disease=c("hf"),id='pnr')
#' 
#' # View first 6 lines of extracted data 
#' head(dat.extracted$data)
#' 
#' # Codes specified to be extracted
#' dat.extracted$codes
#' 
#' # Unique codes extracted
#' dat.extracted$unique.codes
#'
#' # Extract diagnoses related to cancer after the date 01-01-2007.
#' dat.extracted <- extractCode(lpr.data,disease=c("cancer"),p.in='2007-01-01',id='pnr')
#'
#' # Extract diagnoses related to liver disease or cancer.
#' dat.extracted <- extractCode(lpr.data,disease=c("liver","cancer"),prefix='diag',id='pnr')
#' 
#' # Extract cancer diagnoses, and keep only diagnosis of patients of type 3
#' dat.extracted <- extractCode(lpr.data,disease="cancer",pat=3,id='pnr')
#'
#' # Extract all diagnoses begining with 'DI', and exclude all diagnoses with 'DI22' and 'DI9'.
#' dat.extracted <- extractCode(lpr.data,inclusions='DI',
#'                              exclusions=c('DI22','DI9'),prefix='i',id='pnr')
#'
#' # Extract ischemic heart disease diagnoses within three years after index date
#' dat.extracted <- extractCode(lpr.data,inclusions=c("DI"),indexvar='indexdate',
#'                               index.int=365*3,id='pnr')
#' 
#' # Extract bleeding diagnoses within one year before index date
#' dat.extracted <- extractCode(lpr.data,inclusions=c("DN"),indexvar='indexdate',
#'                              index.int=-365,id='pnr')
#' }
#' @export
#' @author Regitze Kuhr Skals <r.skals@rn.dk>

extractCode <- function(dat,disease=NULL,inclusions=NULL,exclusions=NULL,p.in=NULL,p.out=NULL,
                        keep='',pat=NULL,prefix='',entryvar='inddto',id='PNR',codevar='diag',
                        patvar='pattype',indexvar=NULL,index.int=NULL,lmdb=FALSE){
    .I = row1=.N=rowN=entrydate=index=pattype=pnr=dis=.SD=NULL
    
  # definition of diseases
  

  
  ##  Make into data.table 
  requireNamespace("data.table")
  d <- copy(dat)
  data.table::setDT(d)
  
  data.table::setnames(d,old=c(codevar,id,entryvar,patvar),
                       new=c('diag','pnr','entrydate','pattype'))
  if(!is.null(indexvar)){
    data.table::setnames(d,indexvar,'index')
  }
    
  if(!is.null(p.in)){ 
    if(class(try(as.Date(p.in),silent=TRUE))=='try-error'){
      warning("Argument 'p.in' is not a date") 
      return(NA)
    }
  }
  
  if(!is.null(p.out)){
    if(class(try(as.Date(p.out),silent=TRUE))=='try-error'){
    warning("Argument 'p.out' is not a date") 
    return(NA)
    }
  }
  
    ## extraction of diagnoses
    utils::data(diseasecode)
    if(!is.null(disease)){
        diags_for_extraction <- unlist(diseasecode[disease])
        if(!is.null(inclusions)){
            diags_for_extraction <- c(diags_for_extraction,inclusions)
        }
    }
    else{
        diags_for_extraction <- inclusions
    }
  
  setkey(d,diag) #Sort by key
  icdcodes <- d[data.table::data.table(unique(diag)),data.table::data.table(diag),mult="first"] #unique diagnoses
  icdcodes <- icdcodes[unlist(lapply(paste('^?',diags_for_extraction,sep=''),grep,diag))] #the unique diagnoses of interest
  if(!is.null(exclusions)){
    ex.diag <- grep(paste(paste('^?',exclusions,sep=''),collapse='|'),icdcodes$diag)
    if(length(ex.diag)==0){
      warning('exclusions do not exist in data')
    }
    else{
      icdcodes <- icdcodes[-ex.diag]
      diags_for_extraction <- setdiff(diags_for_extraction,exclusions)
    }
  }
  out <- d[icdcodes,nomatch = 0L] #Patients with unique diagnoses of interest
  
  #Restrict diagnoses to specific period in time
  if(!is.null(p.in)&!is.null(p.out)){
    p.in <- as.Date(p.in)
    p.out <- as.Date(p.out)
    out <- out[p.in<=entrydate&entrydate<=p.out]
  }
  
  if(!is.null(p.in)&is.null(p.out)){
    p.in <- as.Date(p.in)
    out <- out[p.in<=entrydate]
  }
  
  if(is.null(p.in)&!is.null(p.out)){
    p.out <- as.Date(p.out)
    out <- out[p.out>=entrydate]
  }

  # Find diagnoses in specific interval before an index date
  if(!is.null(indexvar)&!is.null(index.int)){
    if(index.int<0){
      out <- out[index>=entrydate&(index+index.int)<=entrydate]
    }
    if(index.int>0){
      out <- out[index<=entrydate&(index+index.int)>=entrydate]
    }
  }
  
  #Restrict to specific type of patient (pattype)
  if(!is.null(pat)){
    out <- out[pattype %in% pat]
  }

  #Take first diagnosis if more than one for each patient
  if(keep=="first"& is.null(disease)){
    #Order data by pnr and increasing entrydate
    setkey(out,pnr,entrydate)
    #out <- out[data.table::data.table(unique(pnr)),,mult="first"] 
    out <- out[,.SD[1],by="pnr"] 
  }
  
  #Take last diagnosis if more than one for each patient
  if(keep=="last"& is.null(disease)){
    #Order data by pnr and increasing entrydate
    setkey(out,pnr,entrydate)
    #out <- out[data.table::data.table(unique(pnr)),,mult="last"] 
    out <- out[,.SD[.N],by="pnr"] 
  }
  
  #Take first diagnosis if more than one for each patient
  if(keep=="first"&length(disease)==1){
    #Order data by pnr and increasing entrydate
    setkey(out,pnr,entrydate)
    #out <- out[data.table::data.table(unique(pnr)),,mult="first"] 
    out <- out[,.SD[1],by="pnr"] 
  }
  
    #Take last diagnosis if more than one for each patient
    if(keep=="last"&length(disease)==1){
        #Order data by pnr and increasing entrydate
        setkey(out,pnr,entrydate)
        #out <- out[data.table::data.table(unique(pnr)),,mult="last"] 
        out <- out[,.SD[.N],by="pnr"] 
    }
  
    # Takes first diagnosis of each disease if multiple diseases are chosen for each patient.
  if(keep=="first"&length(disease)>1){
    
    # Mark different diseases
    out[,dis:=""]
    names_dis <- names(diseasecode[disease])
    sygdomme <- diseasecode[disease]
    
    idx <- lapply(sygdomme,function(var){grep(paste(paste('^?',unlist(var),sep=''),collapse='|'),out$diag)})
    
    for(x in seq_along(idx)){
      data.table::set(out,i=idx[[x]],j='dis',value=names_dis[x])
    }
    
    # Order data by pnr, disease and increasing entrydate
    data.table::setkey(out,pnr,dis,entrydate)
    
    # For each patient keeps only first diagnosis of each different disease.
    out <- out[out[,list(row1=.I[1]),by=list(pnr,dis)][,row1]] 
  }
  
  # Takes last diagnosis of each disease if multiple diseases are chosen for each patient.
  if(keep=="last"&length(disease)>1){
    
    # Mark different diseases
    out[,dis:=""]
    names_dis <- names(diseasecode[disease])
    sygdomme <- diseasecode[disease]
    
    idx <- lapply(sygdomme,function(var){grep(paste(paste('^?',unlist(var),sep=''),collapse='|'),out$diag)})
    
    for(x in seq_along(idx)){
      data.table::set(out,i=idx[[x]],j='dis',value=names_dis[x])
    }
    
    # Order data by pnr, disease and increasing entrydate
    data.table::setkey(out,pnr,dis,entrydate)
    
    # For each patient keeps only last diagnosis of each different disease.
    out <- out[out[,list(rowN=.I[.N]),by=list(pnr,dis)][,rowN]] 
  }
  
  # Takes every diagnosis of each disease if multiple diseases are chosen and marks each disease by
  # it's prespecified name.
   if(keep==''&length(disease)>1){
    # Mark different diseases
    out[,dis:='']
    names_dis <- names(diseasecode[disease])
    sygdomme <- diseasecode[disease]
    
    idx <- lapply(sygdomme,function(var){grep(paste(paste('^?',unlist(var),sep=''),collapse='|'),out$diag)})
    for(x in seq_along(idx)){
      data.table::set(out,i=idx[[x]],j='dis',value=names_dis[x])
    }
  }
  
  setkey(out,pnr,entrydate)
  
  if(!is.null(disease)&length(disease)==1&prefix==''){
  date_name <- paste(disease,'date',sep='_')
  } 
  else if(prefix!=""){
    date_name <- paste(prefix,'date',sep='_')
  } 
  else{date_name <- entryvar}
  
  unique.codes=as.character(unique(out$diag))
  
  data.table::setnames(out,new=c(codevar,id,date_name,patvar),
                       old=c('diag','pnr','entrydate','pattype'))
  if(!is.null(indexvar)){
    data.table::setnames(out,new=indexvar,old='index')
  }
  
  names(diags_for_extraction) <- NULL
  
  if(!is.null(exclusions)){
    return(list(data=out,codes=diags_for_extraction,unique.codes=unique.codes,excl=exclusions))
  }
  else{
    return(list(data=out,codes=diags_for_extraction,unique.codes=unique.codes))  
  }
  
}

