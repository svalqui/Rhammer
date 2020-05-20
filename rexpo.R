DiscoverListNames <- function(ObjList, spacing = ""){
  # Initilaize index counter
  idx <- 0
  spacesLN <- spacing
  spacesLN <- paste0(spacesLN, "  ")
  for (element in ObjList){
    idx <- idx + 1
    if (typeof(element) == "list"){
      print(paste0(spacesLN, names(ObjList[idx]), " - list on LN"))
      DiscoverListNames(element, spacesLN)
    } # End if
    
  } # End for
  
} # End function


DiscoverListContent <- function(ObjList, spacing = ""){
  # Initilaize index counter
  idx <- 0
  spacesLC <- spacing
  for (element in ObjList){
    idx <- idx + 1
    if (typeof(element) == "list"){
      print(paste0(spacesLC, names(ObjList[idx]), " - list on LC"))

      DiscoverListNames(element, spacesLC)
    } else if (typeof(element) == "logical") {
      print(paste0(spacesLC, names(ObjList[idx]), " - logical on LC"))
    } else if (typeof(element) == "integer") {
      print(paste0(spacesLC, names(ObjList[idx]), " - integer on LC"))
    } else if (typeof(element) == "double") {
      print(paste0(spacesLC, names(ObjList[idx]), " - double on LC"))
    } else if (typeof(element) == "complex") {
      print(paste0(spacesLC, names(ObjList[idx]), " - complex on LC"))
    } else if (typeof(element) == "character") {
      print(paste0(spacesLC, names(ObjList[idx]), " - character on LC"))
    } else if (typeof(element) == "raw") {
      print(paste0(spacesLC, names(ObjList[idx]), " - raw on LC"))
    }

  } # End for

} # End function


DiscoverObj <- function(Obj, spacing = ""){
  spacesDO <- spacing
  if (typeof(Obj) == "list"){
    idx <- 0
    print(paste0(spacesDO, deparse(substitute(Obj)), " - list On DO"))
    spacesDO <- paste0(spacesDO, "  ")
    DiscoverListContent(Obj, spacesDO)

  } else if (typeof(Obj) == "logical"){
    print(paste0(spacesDO, deparse(substitute(Obj)), " - logical On DO"))
  } else if (typeof(Obj) == "integer"){
    print(paste0(spacesDO, deparse(substitute(Obj)), " - integer On DO"))
  } else if (typeof(Obj) == "double"){
    print(paste0(spacesDO, deparse(substitute(Obj)), " - double On DO"))
  } else if (typeof(Obj) == "complex"){
    print(paste0(spacesDO, deparse(substitute(Obj)), " - complex On DO"))
  } else if (typeof(Obj) == "character"){
    print(paste0(spacesDO, deparse(substitute(Obj)), " - character On DO"))
  } else if (typeof(Obj) == "raw"){
    print(paste0(spacesDO
      , deparse(substitute(Obj)), " - raw On DO"))
  }

} # End function