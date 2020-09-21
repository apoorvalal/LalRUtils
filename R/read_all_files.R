####################################################
#' Reads in all datasets in given location
#' @param path of file/s
#' @export
#' @keywords read stata sas spss
#' @examples
#' read_all_files(extension='dta',path='~/data/')

##########################################################################
# loads all the datasets in a location to memory - only for small datasets
##########################################################################

read_all_files <- function(extension,location){
    suppressMessages(library(haven))
    setwd(location)
    file_pattern = paste0("\\.",extension,"$")
    obj          = list.files(pattern=file_pattern)
    pos          = regexpr(extension,obj)
    objs         = substr(obj,1,pos-2)
    if(extension=='Rdata'){
        df=lapply(obj,load,envir=.GlobalEnv)
    }
    else if(extension=='dta'){
        for (n in 1:length(objs)){
            assign(paste0(objs[n]),haven::read_dta(obj[n]),envir = .GlobalEnv)
        }
    }
    else if(extension=='sav'){
        for (n in 1:length(objs)){
            assign(paste0(objs[n]),haven::read_sav(obj[n]),envir = .GlobalEnv)
        }
    }
    return(objs)
}
