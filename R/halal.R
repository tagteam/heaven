#' @title Extraction of diseases by diagnoses
#' @description Filtering of LPR registry data according to a given set of diseases
#' @param dat Data with diagnoses.
#' @param disease Characterstring containing pre-specified name of disease after HALAL definitions. Can be one or more of: "af",
#'              "rheum_heart_valve_disease", "dvt_pe","stroke_tci_systemic_embolism_thrombosis",
#'              "bleeding_without_hemstroke","cardiomyopathy", "hf", "lung_edema", "HF", "ihd","ap","ami","perart",
#'              "cancer","chronic_kidney_disease","copd","liver_disease","alcohol".   
#' @param inclusions Characterstring, where additional diagnoses can be included. If disease is not defined, inclusions will be the extracted diagnoses.
#' @param exclusions Characterstring, specifying diagnoses to be omitted.
#' @param first.pnr Logical. Specifies if only the first record of each patient should be output. 
#' If mult=TRUE and first.pnr=TRUE the first diagnosis of each disease specified will be extracted.
#' @param mult Logical. Specifies if multiple diseases have been chosen.
#' @param p.in Date of period start.
#' @param p.out Date of period end.
#' @param pat Number or vector defining types of patients to include (pattype: 0,1,2,3), default is all types.
#' @param prefix Character string of prefix name for the resulting date variable of disease.
#' @param entryvar Name of the variable in data that contains the entrydate of diagnosis.
#' @param outvar Name of the variable in data that contains the exitdate of diagnosis.
#' @param id Name of the variable in data that contains patient id.
#' @param codevar Name of the variable in data that contains diagnoses.
#' @param patvar Name of the variable in data that contains the type of patient.
#' @param record.id Name of the variable in data that contains the record number for each patient.
#' @details Extracts specific selected or predefined diagnoses. If specified by first.pnr only the first occurrence of the diagnoses are extracted.
#' Diagnoses in a specific period of time can also be extracted by p.in and p.out.
#' @return A list of three elements. The first one is the extracted data. The second element contains the diagnoses specified 
#' to be extracted. The third element contains every unique diagnosis extracted. 
#' @author Regitze Kuhr Skals

halal <- function(dat,disease=NULL,inclusions=NULL,exclusions=NULL,p.in=NULL,p.out=NULL,mult=FALSE,
                  first.pnr=FALSE,pat=NULL,prefix='',entryvar='inddto',id='pnr',outvar='uddto',codevar='diag',patvar='pattype',record.id='recnum'){
  
  # HALAL definition of diseases
  
  halal_defs <- list(af='I48',rheum_heart_valve_disease=c(as.character(c(4240,4241,39500:39502,39508,39509,39600:39604,39608,39609)),'Z952','Z954','I05','I06','I080A','I082A','I083A'),
                     dvt_pe=paste('I',as.character(c(801:803,808,809,821:823,828,829,26)),sep=''),
                     stroke_tci_systemic_embolism_thrombosis=c('I63','I64','I60','I61','I62','S064','S065','S066','G458','G459','I74'),
                     bleeding_without_hemstroke=c('I312','N02','R31','R04','H313','H356','H431','H450','H052A','K228F','K250','K252',
                                                  'K254','K256','K260','K262','K264','K266','K270','K272','K274','K276','K280','K282',
                                                  'K284','K286','K290','K298A','K625','K638B','K638C','K661','K838F','K868G','K920','K921','K922',
                                                  'KI850','I864A','S064','S065','S066','G951A','S368D','J942','D500','D62'),
                     cardiomyopathy='I42',
                     hf=c('I50','I110'),
                     lung_edema='J81',
                     HF=c('I42','I50','I110','J81'),
                     ihd=paste('I',as.character(c(20:25)),sep=''),
                     ap='I20',
                     ami=c('I21','I22'),
                     perart='I70',
                     cancer='C',
                     chronic_kidney_disease=c('N02','N03','N04','N05','N06','N07','N08','N11','N12','N14','N18','N19','N26','N158','N159',
                                              'N160','N162','N163','N164','N168','Q61','E102','E112','E132','E142','I120','M321B'),
                     copd=c('J42','J43','J44'),
                     liver_disease=c('B15','B16','B17','B18','B19','C22','K70','K71','K72','K73','K74','K75','K76','K77','Z944','I982','D684C'),
                     
                     alcohol=c('F10','K70','E52','T51','K860','E244','G312','I426','O354','Z714','Z721','G621','G721','K292','L278A'))
  
  ##  Make into data.table and change relevant variable names to lower case
  d <- as.data.table(dat)
  var.names <- tolower(colnames(d)) 
  colnames(d) <- var.names 
  
  setnames(d,codevar,'diag')
  setnames(d,id,'pnr')
  setnames(d,entryvar,'inddto')
  setnames(d,outvar,'uddto')
  setnames(d,patvar,'pattype')
  setnames(d,record.id,'recnum')
  
  if(!is.null(p.in) & class(p.in)!='Date'){
    stop('p.in is not a date')    
  }
  
  if(!is.null(p.out) & class(p.out)!='Date'){
    stop('p.out is not a date')    
  }
  
  ## Substraction of diagnoses
  if(!is.null(disease)){
    diags_for_substraction <- unlist(halal_defs[disease])
    if(!is.null(inclusions)){
      diags_for_substraction <- c(diags_for_substraction,inclusions)
    }
  }
  else{
    diags_for_substraction <- inclusions
  }
  
  setkey(d,diag) #Sort by key
  icdcodes <- d[.(unique(diag)),.(diag),mult="first"] #unique diagnoses
  icdcodes <- icdcodes[unlist(lapply(paste('^D?',diags_for_substraction,sep=''),grep,diag))] #the unique diagnoses of interest
  if(!is.null(exclusions)){
    ex.diag <- grep(paste(paste('^D?',exclusions,sep=''),collapse='|'),icdcodes$diag)
    icdcodes <- icdcodes[-ex.diag]
    diags_for_substraction <- setdiff(diags_for_substraction,exclusions)
  }
  out <- d[icdcodes,nomatch = 0L] #Patients with unique diagnoses of interest
  
  #Restrict diagnoses to specific period in time
  if(!is.null(p.in)&!is.null(p.out)){
    out <- out[p.in<inddto&inddto<p.out]
  }
  
  if(!is.null(p.in)&is.null(p.out)){
    out <- out[p.in<inddto]
  }
  
  if(is.null(p.in)&!is.null(p.out)){
    out <- out[p.out>inddto]
  }
  
  #Restrict to specific type of patient (pattype)
  if(!is.null(pat)){
    out <- out[pattype %in% pat]
  }
  
  #Take first diagnosis if more than one for each patient
  if(first.pnr==T&mult==F){
    #Order data by pnr and increasing inddto
    setkey(out,pnr,inddto)
    out <- out[.(unique(pnr)),,mult="first"] 
  }
  
  # Takes first diagnosis of each disease if multiple diseases are chosen for each patient.
  if(first.pnr==T&mult==T){
    
    # Mark different diseases
    out[,dis:=""]
    names_dis <- names(halal_defs[disease])
    sygdomme <- halal_defs[disease]
    
    idx <- lapply(sygdomme,function(var){grep(paste(paste('^D?',unlist(var),sep=''),collapse='|'),out$diag)})
    
    for(x in seq_along(idx)){
      set(out,i=idx[[x]],j='dis',value=names_dis[x])
    }
    
    # Order data by pnr, disease and increasing inddto
    setkey(out,pnr,dis,inddto)
    
    # For each patient keeps only first diagnosis of each different disease.
    out <- out[out[,list(row1=.I[1]),by=list(pnr,dis)][,row1]] 
  }
  
  # Takes every diagnosis of each disease if multiple diseases are chosen and marks each disease by
  # it's prespecified name.
  
  if(first.pnr==F&mult==T){
    
    # Mark different diseases
    out[,dis:=""]
    names_dis <- names(halal_defs[disease])
    sygdomme <- halal_defs[disease]
    
    idx <- lapply(sygdomme,function(var){grep(paste(paste('^D?',unlist(var),sep=''),collapse='|'),out$diag)})
    
    for(x in seq_along(idx)){
      set(out,i=idx[[x]],j='dis',value=names_dis[x])
    }
  }
  
  if(mult==T){
    out <- out[,.(pnr,recnum,inddto,diag,pattype,dis)] # keeps marker of disease when multiple diseases are chosen.
  }
  else{
    out <- out[,.(pnr,recnum,inddto,diag,pattype)]
  }
  
  date_name <- paste(prefix,'date',sep='_')
  
  setnames(out,'inddto',date_name)
  setnames(out,'diag',codevar)
  setnames(out,'pnr',id)
  setnames(out,'pattype',patvar)
  setnames(out,'recnum',record.id)
  
  return(list(data=out,diagnoses=diags_for_substraction,unique.icd=icdcodes$diag))
}

