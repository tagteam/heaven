#' @title Extraction of simple procedures
#'
#' @description Extracting simple procedures given from a list of procedure codes from a data file.
#'              The procedures must be the only procedure occuring within a given time-frame.
#' @param data Input data with containing procedures
#' @param time.frame An integer value setting time-window in number of days between procedures in 
#'                   order to denote them as simple. Default is one day.
#' @param procedure A list of procedure codes to be extracted. 
#'                  Default is c('KFNG','KFN','KFK','KFM','KFG','KFJ', 'KFP','UXU', 'UXCC00A','UXCC00')
#'                  corresponding to PCI, CABG, klapoperation, DC konvertering, arbejds EKG, 
#'                  CT hjerte and CT Thorax
#' @param incombination indikator of whether combined or simple procedures are sought. Default is Null.               
#' @param record.id Name of patient identifier. Default is RECNUM corresponding to the name in OPR.
#' @param procedure.codes Name of procedure codes
#' @param out.date Name of procedure date. Default is ODTO corresponding to the name in OPR.
#'
#' @return A list containg the filtered data.
#' @export
#'
#' @examples
#'\dontrun{
#' Generating test-data that are OPR a like.
#'RECNUM = 1:20
#'atc = rep(c("KFN9884","ZZl0986","UX9004","UXCC001", "UXCC00A", "KFNG978"), each =50)
#'eksd = rep(c("2002-01-15","2002-01-16","2004-02-15","2002-02-17", "2004-02-18"), each =10)
#'
#'testdat <- data.table(RECNUM, atc, eksd) 
#'
#'
#'} 
subtract_procedures <- function(data,
                                time.frame=NULL,
                                procedure=NULL,
                                incombination=NULL,
                                record.id = "RECNUM",
                                procedure.codes = "OPR",
                                out.date = "ODTO"){

    # In the following, we make sure that the input data gets to be in data.table format
    data <- as.data.table(data)
    
    # Setting inputted variable names to match the default
    setnames(data, record.id , "RECNUM")
    setnames(data, procedure.codes, "OPR")
    setnames(data, out.date, "ODTO")

    # Notes for default procedures 
    # PCI ='KFNG', 
    # CABG = 'KFN', 
    # klapoperation=c('KFK','KFM','KFG','KFJ'), 
    # DC_konvertering = 'KFP', arbejds_EKG = 'UXU',
    # CT_hjerte = 'UXCC00A', CT_thorax='UXCC00')
    pre_def_procedures_list = c('KFNG','KFN','KFK','KFM','KFG','KFJ', 'KFP',
                                'UXU', 'UXCC00A','UXCC00')
    
    # Setting the default time.frame to be one day.
    if(is.null(time.frame)){
        time.frame = 1
    }
    
    # Setting the default OPR-code to search for if there is no procedure input. Otherwise, the OPR's to search for are 
    # inputted in procedure.
    if(!is.null(procedure)){
        procedures_for_subtraction = procedure
    }
    if(is.null(procedure)){
        procedures_for_subtraction = pre_def_procedures_list
    }
    
    # Making the list of procedures into a regex.
    # We only want the elements of the list in the beginning 
    edited_procedures_for_subtraction <- paste("^(",procedures_for_subtraction,")", sep ="")
    regex_for_subtraction <- paste(edited_procedures_for_subtraction, collapse = "|")

    # printing the search criterias
    print("We make the following procedure search:")
    print(procedures_for_subtraction)
    print(regex_for_subtraction)
    
    # We also format the date variable
    data$ODTO <- as.Date(data[, ODTO])

    # Reduce by discarding those with multiple operations within a time-frame 
    # when no incombinations are wanted
    if(is.null(incombination)){
        setkey(data, RECNUM, ODTO)
        
        # The following gives the number of observartions for each RECNUM
        test_numberby <- data[, count:= .N , by=.(RECNUM, ODTO)]
        
        # keeping only those patients with one operation a day
        one_op_a_day <- test_numberby[count == 1]
        
        # calculate the time_difference between consecutive operations
        setkey(one_op_a_day, RECNUM, ODTO)

        # We give names to the shifted columns, which are added at the end. They will just be called lead_ in front of their previous name.
        cols <- c("OPR", "ODTO")
        shiftcol <- paste("lead", cols , sep="_")
        
        # We then shift all operations up one row for each RECNUM, i.e. we can 
        # compare two consecutive dates for the same patient
        one_op_a_day[, (shiftcol):= shift(.SD, 1, 0, "lead"), .SDcols = cols, by= RECNUM]
        
        print("Type of lead ODTO")
        print(typeof(one_op_a_day[,lead_ODTO]))
        
        # Now we make the comparison by substraction to get the time difference in number of days.
        test_data_difference <- one_op_a_day[,.(RECNUM, OPR, ODTO, time_difference = as.Date(lead_ODTO, origin = "2001-02-03")-ODTO)]
        
        # we sort by RECNUM
        setkey(test_data_difference,RECNUM)
        
        # keeping only those with more than a time-frame between operations
        singular_ops <- test_data_difference[test_data_difference$time_difference > time.frame] 
    }
    
    # 3: Find the inputtet OPR-procedures
    # the following is a compact way to use regex via grep for the procedure-list for subtraction on the singular_ops$OPR
    # There are some problems here. It does not find all procedures.
    #outdata <- singular_ops[unlist(lapply(procedures_for_subtraction, grep, OPR))]
    
    # we try again. Building it from the bottom
    outdata <- singular_ops[grep(regex_for_subtraction,OPR)]
    
    setnames(outdata, "RECNUM", record.id)
    setnames(outdata, "ODTO", out.date)
    
    # 4: output
    return(list(data=outdata))
    
}

