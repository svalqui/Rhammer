DiscoverListNames <- function(ObjList, spacing = ""){
  # Initilaize index counter
  idx <- 0
  spaces <- spacing
  for (element in ObjList){
    idx <- idx + 1
    if (typeof(element) == "list"){
      print(paste0(spaces, names(ObjList[idx])))
      #print(spaces + names(ObjList[idx]))
      DiscoverListNames(element, "  ")
    } # End if
    
  } # End for
  
} # End function


DiscoverListContent <- function(ObjList, spacing = ""){
  # Initilaize index counter
  idx <- 0
  spaces <- spacing
  for (element in ObjList){
    idx <- idx + 1
    if (typeof(element) == "list"){
      print(paste0(spaces, names(ObjList[idx])))
      DiscoverListNames(element, "  ")
    } else if (typeof(element) == "logical") {
      print(paste0(spaces, names(ObjList[idx]), " - logical"))
    } else if (typeof(element) == "integer") {
      print(paste0(spaces, names(ObjList[idx]), " - integer"))
    } else if (typeof(element) == "double") {
      print(paste0(spaces, names(ObjList[idx]), " - double"))
    } else if (typeof(element) == "complex") {
      print(paste0(spaces, names(ObjList[idx]), " - complex"))
    } else if (typeof(element) == "character") {
      print(paste0(spaces, names(ObjList[idx]), " - character"))
    } else if (typeof(element) == "raw") {
      print(paste0(spaces, names(ObjList[idx]), " - raw"))
    }

  } # End for

} # End function


DiscoverObj <- function(Obj, spacing = ""){
  spaces <- spacing
  if (typeof(Obj) == "list"){
    print(paste0(spaces, deparse(substitute(Obj)), " - list"))
    spaces <- paste0(spaces, "  ")
    for (element in Obj){
      DiscoverObj(element, spaces)
    } # End for
  } else if (typeof(Obj) == "logical"){
    print(paste0(spaces, deparse(substitute(Obj)), " - logical"))
  } else if (typeof(Obj) == "integer"){
    print(paste0(spaces, deparse(substitute(Obj)), " - integer"))
  } else if (typeof(Obj) == "double"){
    print(paste0(spaces, deparse(substitute(Obj)), " - double"))
  } else if (typeof(Obj) == "complex"){
    print(paste0(spaces, deparse(substitute(Obj)), " - complex"))
  } else if (typeof(Obj) == "character"){
    print(paste0(spaces, deparse(substitute(Obj)), " - character"))
  } else if (typeof(Obj) == "raw"){
    print(paste0(spaces, deparse(substitute(Obj)), " - raw"))
  }

} # End function