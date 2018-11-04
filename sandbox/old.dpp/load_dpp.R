load.dpp <- function(dpp){
    load.names <- c()
    for(i in 1:length(dpp)){
        if(class(dpp[[i]])=="dppObject"){
            if(attr(dpp[[i]], "status")=="nominal"){
                message(paste("Loading data for", names(dpp)[i], "\n"))
                tmp.intLoad <- internalLoadData(dpp[[i]])
                dpp[[i]] <- tmp.intLoad$dppObject
                message(tmp.intLoad$message)
                load.names <- c(load.names, names(dpp)[i])
            }
        }
    }
    if(length(load.names)>0){
        message(paste0("Data for the following object(s) were loaded: ",
                       paste(load.names, collapse=", ")))
    }
    else{
        message("No data was loaded.")
    }
    return(dpp)
}
