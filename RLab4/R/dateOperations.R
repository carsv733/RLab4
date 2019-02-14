
dateOperations <- setRefClass("dateOperations", methods=list(
  date_func = function() {
    return(paste("This function was called",Sys.Date()))
  }
))
