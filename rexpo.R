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
