print.dppObject <- function(dppObject){
    .status <- attr(dppObject,"status")
    if(.status=="empty"){
        message("No data registered.")
    }
    if(.status=="nominal"){
        .nom <- dppObject$nominal
        mess.out <- "No data is loaded yet, but the following nominal data is registered:\n\n"
        mess.out <- paste0(mess.out, "The data is to be read from: ", .nom$path, "\n")
        name.changes <- .nom$names[names(.nom$names)!=as.vector(unlist(.nom$names))]
        if(length(name.changes)>0){
            mess.out <- paste0(mess.out, "The following name(s) are to be updated:\n")
            for(.n in 1:length(name.changes)){
                mess.out <- paste0(mess.out, "   ", names(name.changes)[.n], " = ", name.changes[[.n]], "\n")
            }
        }
        if(length(.nom$import.info)>0){
            mess.out <- paste0(mess.out, "Other stored import information is:\n")
            for(.n in 1:length(.nom$import.info)){
                mess.out <- paste0(mess.out, "   ", names(.nom$import.info)[.n], " = ", .nom$import.info[[.n]], "\n")
            }
        }
        message(mess.out)
    }
    if(.status=="loaded"){
        format(dppObject=dppObject, autoformat=FALSE)
        print(dppObject$data)
    }
}
