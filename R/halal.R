halal <- function(dat,disease=NULL,p.in=NULL,p.out=NULL,first.pnr=FALSE,pat=NULL,diags=NULL,prefix=''){
  
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
  
  ##  Make into data.table and change all variables to lower case
  d <- as.data.table(dat)
  var.names <- tolower(colnames(d)) 
  colnames(d) <- var.names 
  
  ## Substraction of diagnoses
  if(!is.null(diags)){
    diags_for_substraction <- diags
  }else{
    diags_for_substraction <- unlist(halal_defs[disease])
  }
  #a1=proc.time()[3]
    setkey(d,diag) #Sort by key
    browser()
    icdcodes <- d[.(unique(diag)),.(diag),mult="first"] #unique diagnosis
    icdcodes <- icdcodes[unlist(lapply(paste('^D',diags_for_substraction,sep=''),grep,diag))] #the unique diagnosis of interest
    out <- d[icdcodes,nomatch = 0L] #Patients with unique diagnosis of interest
  
  #a2=proc.time()[3]
  #print(a2-a1)
  #b1=proc.time()[3]
   # out2=d[grep(paste0("^D?",paste0(diags_for_substraction,collapse="|")),diag)]
    #b2=proc.time()[3]
    #print(b2-b1)
    #browser()
    all.equal(out,out2)
  if(!is.null(p.in)&!is.null(p.out)){
    #Restrict diagnoses to specific period in time
    
    out <- out[p.in<inddto&inddto<p.out]
  }
  
  #Restrict to specific type of patient (pattype)
  if(!is.null(pat)){
    out <- out[pattype %in% pat]
  }
  
  #Take first diagnosis if more than one for each patient
  if(first.pnr){
    #Order data by pnr and increasing inddto
    setkey(out,pnr,inddto)
    out <- out[.(unique(pnr)),,mult="first"] 
  }
  
  
  date_name <- paste(prefix,'inddto',sep='_')
  
  out <- out[,.(pnr,recnum,inddto,diag)]
  setnames(out,'inddto',date_name)
  
  return(list(data=out,diagnoses=diags_for_substraction))
}
