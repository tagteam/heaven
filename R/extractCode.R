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
#' @param record.id Name of the variable in data that contains the record number for each patient.
#' @param indexvar Name of the variable in data that contains the index date.
#' @param index.int Numeric. Number of days before or after an index date to search for specific diagnoses/atc codes. If negative, days before index date. If positve, days after index date (i.e. outcome).
#' @param lmdb Logical. If true, data with atc codes is expected, and record.id and patvar are not expected to be in the data.
#' @details Extracts specific selected ICD- or ATC codes, or predefined diseases by diagnoses. If specified by keep, only the first or last occurrence of the code is extracted.
#' @return A list of three elements. data: the extracted data. codes: contains the codes specified 
#' to be extracted. unique.codes: contains every unique code extracted. If exclusions are specified, these are contained in an additional list element: excl. 
#' @examples 
#' \dontrun{
#' 
#' # Simulated LPR-registry data
#' data(lpr.data)
#' 
#' # Extract diagnoses related to heart failure
#' dat.extracted <- extractCode(lpr.data,disease=c("hf"))
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
#' # Extract diagnoses related to heart failure after the date 01-01-2007.
#' dat.extracted <- extractCode(lpr.data,disease=c("hf"),p.in='2007-01-01')
#'
#' # Extract diagnoses related to liver disease or cancer, and only include first
#' # diagnosis of each disease for each patient.
#' dat.extracted <- extractCode(lpr.data,disease=c("liver","cancer"),keep='first',prefix='diag')
#' 
#' # Extract cancer diagnoses, and keep only first diagnosis and patients of type 3
#' dat.extracted <- extractCode(lpr.data,disease="cancer",keep='first',pat=3)
#'
#' # Extract all diagnoses begining with an 'DI', and exclude all diagnoses with 'DI21' and 'DI8'.
#' dat.extracted <- extractCode(lpr.data,inclusions='DI',exclusions=c('DI21','DI8'),prefix='i')
#'
#' # Extract ischemic heart disease without DI21 and DI22:
#' dat.extracted <- extractCode(lpr.data,disease='ihd',exclusions=c('DI21','DI22'),prefix='ihd_excl_ami')
#' 
#' # Extract ischemic heart disease diagnoses within three years after index date
#' dat.extracted <- extractCode(lpr.data,disease=c("ihd"),indexvar='indexdate',index.int=365*3)
#' 
#' # Extract bleeding diagnoses within one month before index date
#' dat.extracted <- extractCode(lpr.data,disease=c("bleeding"),indexvar='indexdate',index.int=-365)
#' }
#' @export
#' @author Regitze Kuhr Skals <r.skals@rn.dk>

extractCode <- function(dat,disease=NULL,inclusions=NULL,exclusions=NULL,p.in=NULL,p.out=NULL,
                  keep='',pat=NULL,prefix='',entryvar='inddto',id='PNR',codevar='diag',
                  patvar='pattype',record.id='recnum',indexvar=NULL,index.int=NULL,lmdb=FALSE){
  
  # definition of diseases
  
  disease_defs <- list(af=c("DI48","42793", "42794"),rheumvalve=c(as.character(c(39400, 39401, 39402, 
                      39408, 39409, 39500, 39501, 39502, 39508, 39509, 39600, 39601, 39602, 39603, 39604, 39608, 39609,
                      99751, 4240, 4241)),paste0('D',c('Z952','Z954','I05','I06','I080A','I081A','I082A','I083A'))),
                     dvtpe=paste('DI',as.character(c(801:803,808,809,821:823,828,829,26)),sep=''),
                     stroketci=c(as.character(c(43309, 43399, 43409, 43499, 43600, 43601, 43609, 43690, 43699, 43700,
                                               43701, 43708, 43709, 43790, 43791, 43798, 43799, 43809, 43899, 43509, 43599)),
                     paste0('D',c('I63','I64','G458','G459'))),
                     bleeding=c(as.character(c(85200, 85201, 85202, 85203, 85209, 85210, 85211, 85212, 85290, 85291, 
                                               85292, 43008, 43009, 43098, 43099, 43100, 43101, 43108, 43109, 43190,
                                               43191, 43198, 43199, 28000, 45601, 53098, 53190, 53192, 53195, 53290, 
                                               53390, 53490, 53501, 56319, 56904, 56915)),paste0('D',
                     c('I60','I61','I62','I312','N02','R31','R04','H313','H356','H431','H450','H052A','K228F','K250',
                       'K252','K254','K256','K260','K262','K264','K266','K270','K272','K274','K276','K280','K282',
                                                  'K284','K286','K290','K298A','K625','K638B','K638C','K661','K838F',
                       'K868G','K920','K921','K922','KI850','I864A','S064','S065','S066','G951A','S368D','J942','D500'
                       ,'D62'))),
                     cardiomyopathy='DI42',
                     lungedema='DJ81',
                     hf=c('DI42','DI50','DI110','DJ81',as.character(c(42599, 42709, 42710, 42711, 42719))),
                     ihd=c(paste('DI',as.character(c(20:25)),sep=''),as.character(c(41009, 41099, 41109, 41199, 41209,
                                                                        41299, 41309, 41399, 41409, 41499))),
                     mi=c('DI21','DI22',as.character(c(41009, 41099))),
                     pad=c('DI70',as.character(c(44009, 44019, 44020, 44021, 44028, 44029, 44030, 44039, 44099))),
                     cancer=c(paste0('DC0',0:9),paste0('DC',10:97),as.character(c(109:140))),
                     ckd=c(paste0('D',c('N02','N03','N04','N05','N06','N07','N08','N11','N12','N14',
                                              'N18','N19','N26','N158','N159','N160','N162','N163','N164','N168','Q61',
                                              'E102','E112','E132','E142','I120','M321B','Q612','Q613','Q615','Q619')),
                                              as.character(c(24902, 25002,58200,
                                               58201, 58202, 58208, 58209, 58300, 58301, 58302, 58308, 58309, 58499, 
                                               59009, 59320, 75310, 75311, 75319, 79299, 40399, 40499, 44609, 44629))),
                     copd=c('DJ42','DJ43','DJ44',as.character(c(49100, 49101, 49102, 49103, 49104, 49108, 49109, 49200,
                                                                49201, 49208, 49209))),
                     liver=c(paste0('D',c('B15','B16','B17','B18','B19','C22','K70','K71','K72','K73','K74',
                                                  'K75','K76','K77','Z944','I982','D684C','Q618A')),
                                     as.character(c(15509, 15519, 15589, 45600, 45601, 57109, 57110, 57111, 57119, 
                                                    57190, 57191, 57192, 57193, 57194, 57199, 57200, 57201, 57209, 
                                                    57300, 57301, 57302, 57303, 57304, 57305, 57309))),
                     
                     alcohol=c(paste0('D',c('F10','K70','E52','T51','K860','E244','G312','I426','O354','Z714','Z721',
                                            'G621','G721','K292','L278A')),'E8609',
                               as.character(c(97909, 97919, 97929, 97939, 97949, 97959, 98009, 98019, 98029,
                                              98099, 29109, 29119, 29129, 29139, 29199, 30309, 30319, 30320, 30328,
                                              30329, 30390, 30391, 30399, 57710, 57109, 57110))),
                     dm=c(paste0('DE',10:14),'250'),
                     ht_diag=c(paste0('DI',10:15),as.character(c(40009, 40019, 40029, 40039, 40099, 40199, 40299,
                                                                      40399, 40499))),
                     ulcus=c(paste0('DK',25:27),as.character(c(53100,53101,53108,53109,53190,53191,53192,53193,53194,
                                                               53195,53196,53198,53199,53209,53290,53291,53299,53309,
                                                               53390,53391,53399,53409,53490,53491,53499))),
                     acetylsalicylic_acid=c("B01AC06","N02BA01"),
                     asappi=c("B01AC56"),amiodarone="C01BD01",
                     betablockers=c("C07A","C07B","C07C","C07D","C07F"),class_ic_antiarrytmic_drugs="C01BC",
                     cardiac_glycosides="C01A",
                     diu=c("C02DA", "C02L", "C03A", "C03B", "C03D", "C03E", "C03X", "C07B", "C07C", "C07D", "C07F", "C08G", "C09BA", "C09DA", "C09XA52"),
                     dronedarone="C01BD07",
                     diabetes="A10",
                     ht_med=c("C02A", "C02B", "C02C", "C02DA", "C03A", "C03B", "C03D", "C03E", "C03X", "C02DB", "C02DD", "C02DG", "C07A", "C08", "C09AA", "C09XA02",
                              "C09CA", "C07B","C07C", "C07D", "C07F", "C08G", "C09BA", "C09DA", "C09XA52", "C02L", "C09BB", "C09DB"),
                     loop_diu="C03C",
                     nsaid="M01A",
                     ras=c("C09AA", "C09BA", "C09BB", "C09CA", "C09DA", "C09DB", "C09XA02", "C09XA52"),
                     statins="C10AA",
                     vka=c("B01AA03", "B01AA04"),
                     xa_inhibitors=c("B01AF02", "B01AF03", "B01AF01"),
                     thrombin_inhibitors="B01AE07",
                     adp_inhibitors=c("B01AC04", "B01AC22", "B01AC24"))
  
  ##  Make into data.table 
  d <- as.data.table(dat)
  
  if(is.null(indexvar)&lmdb==FALSE){
    d <- d[,c(id,record.id,entryvar,codevar,patvar),with=FALSE]
    setnames(d,codevar,'diag')
    setnames(d,id,'pnr')
    setnames(d,entryvar,'entrydate')
    setnames(d,patvar,'pattype')
    setnames(d,record.id,'recnum')
  }
  if(!is.null(indexvar)&lmdb==FALSE){
    d <- d[,c(id,record.id,entryvar,codevar,patvar,indexvar),with=FALSE]
    setnames(d,codevar,'diag')
    setnames(d,id,'pnr')
    setnames(d,entryvar,'entrydate')
    setnames(d,patvar,'pattype')
    setnames(d,record.id,'recnum')
    setnames(d,indexvar,'index')
  }
  if(is.null(indexvar)&lmdb==TRUE){
    d <- d[,c(id,entryvar,codevar),with=FALSE]
    setnames(d,codevar,'diag')
    setnames(d,id,'pnr')
    setnames(d,entryvar,'entrydate')
  }
  
  if(!is.null(indexvar)&lmdb==TRUE){
    d <- d[,c(id,entryvar,codevar,indexvar),with=FALSE]
    setnames(d,codevar,'diag')
    setnames(d,id,'pnr')
    setnames(d,entryvar,'entrydate')
    setnames(d,indexvar,'index')
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
  if(!is.null(disease)){
    diags_for_extraction <- unlist(disease_defs[disease])
    if(!is.null(inclusions)){
      diags_for_extraction <- c(diags_for_extraction,inclusions)
    }
  }
  else{
    diags_for_extraction <- inclusions
  }
  
  setkey(d,diag) #Sort by key
  icdcodes <- d[.(unique(diag)),.(diag),mult="first"] #unique diagnoses
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
  if(keep=="first"&length(disease)==1){
    #Order data by pnr and increasing entrydate
    setkey(out,pnr,entrydate)
    out <- out[.(unique(pnr)),,mult="first"] 
  }
  
  #Take last diagnosis if more than one for each patient
  if(keep=="last"&length(disease)==1){
    #Order data by pnr and increasing entrydate
    setkey(out,pnr,entrydate)
    out <- out[.(unique(pnr)),,mult="last"] 
  }
  
  # Takes first diagnosis of each disease if multiple diseases are chosen for each patient.
  if(keep=="first"&length(disease)>1){
    
    # Mark different diseases
    out[,dis:=""]
    names_dis <- names(disease_defs[disease])
    sygdomme <- disease_defs[disease]
    
    idx <- lapply(sygdomme,function(var){grep(paste(paste('^?',unlist(var),sep=''),collapse='|'),out$diag)})
    
    for(x in seq_along(idx)){
      set(out,i=idx[[x]],j='dis',value=names_dis[x])
    }
    
    # Order data by pnr, disease and increasing entrydate
    setkey(out,pnr,dis,entrydate)
    
    # For each patient keeps only first diagnosis of each different disease.
    out <- out[out[,list(row1=.I[1]),by=list(pnr,dis)][,row1]] 
  }
  
  # Takes last diagnosis of each disease if multiple diseases are chosen for each patient.
  if(keep=="last"&length(disease)>1){
    
    # Mark different diseases
    out[,dis:=""]
    names_dis <- names(disease_defs[disease])
    sygdomme <- disease_defs[disease]
    
    idx <- lapply(sygdomme,function(var){grep(paste(paste('^?',unlist(var),sep=''),collapse='|'),out$diag)})
    
    for(x in seq_along(idx)){
      set(out,i=idx[[x]],j='dis',value=names_dis[x])
    }
    
    # Order data by pnr, disease and increasing entrydate
    setkey(out,pnr,dis,entrydate)
    
    # For each patient keeps only last diagnosis of each different disease.
    out <- out[out[,list(rowN=.I[.N]),by=list(pnr,dis)][,rowN]] 
  }
  
  # Takes every diagnosis of each disease if multiple diseases are chosen and marks each disease by
  # it's prespecified name.
   if(keep==''&length(disease)>1){
    # Mark different diseases
    out[,dis:='']
    names_dis <- names(disease_defs[disease])
    sygdomme <- disease_defs[disease]
    
    idx <- lapply(sygdomme,function(var){grep(paste(paste('^?',unlist(var),sep=''),collapse='|'),out$diag)})
    for(x in seq_along(idx)){
      set(out,i=idx[[x]],j='dis',value=names_dis[x])
    }
  }
  
  if(!is.null(disease)&length(disease)==1&prefix==''){
  date_name <- paste(disease,'date',sep='_')
  }
  
  else{
    date_name <- paste(prefix,'date',sep='_')
  }
  
  unique.codes=as.character(unique(out$diag))
  
  if(is.null(indexvar)&lmdb==FALSE){
    setnames(out,'entrydate',date_name)
    setnames(out,'diag',codevar)
    setnames(out,'pnr',id)
    setnames(out,'pattype',patvar)
    setnames(out,'recnum',record.id)
  }
  if(!is.null(indexvar)&lmdb==FALSE){
    setnames(out,'entrydate',date_name)
    setnames(out,'diag',codevar)
    setnames(out,'pnr',id)
    setnames(out,'pattype',patvar)
    setnames(out,'recnum',record.id)
    setnames(out,'index',indexvar)
  }
  if(is.null(indexvar)&lmdb==TRUE){
    setnames(out,'entrydate',date_name)
    setnames(out,'diag',codevar)
    setnames(out,'pnr',id)
  }
  
  if(!is.null(indexvar)&lmdb==TRUE){
    setnames(out,'entrydate',date_name)
    setnames(out,'diag',codevar)
    setnames(out,'pnr',id)
    setnames(out,'index',indexvar)
  }
  
  names(diags_for_extraction) <- NULL

  if(!is.null(exclusions)){
    return(list(data=out,codes=diags_for_extraction,unique.codes=unique.codes,excl=exclusions))
  }
  else{
    return(list(data=out,codes=diags_for_extraction,unique.codes=unique.codes))  
  }
  
}

