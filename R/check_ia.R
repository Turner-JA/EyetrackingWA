check_ia <- function(data) {
  
  # Check if PupilPre is installed
  #.check_for_PupilPre(type="NotAvailable")
  
  # Check right eye
  Rias <- data %>% 
    rename(RIGHT_IA_ID = RIGHT_INTEREST_AREA_ID, 
           RIGHT_IA_LABEL = RIGHT_INTEREST_AREA_LABEL) %>% 
    group_by(RIGHT_IA_LABEL, RIGHT_IA_ID) %>% 
    summarise() %>% arrange(RIGHT_IA_ID) %>% 
    mutate(N = n())
  
  R_bad_id <- 0
  for (x in 1:nrow(Rias)) {
    if (Rias[x, "RIGHT_IA_ID"] >= 0 & Rias[x, "RIGHT_IA_ID"] <= 8) {
    } else {
      R_bad_id <- R_bad_id + 1
    }
  }
  
  R_bad_labels <- filter(Rias, N > 1)
  
  # Check left eye
  Lias <- data %>% 
    rename(LEFT_IA_ID = LEFT_INTEREST_AREA_ID, 
           LEFT_IA_LABEL = LEFT_INTEREST_AREA_LABEL) %>% 
    group_by(LEFT_IA_LABEL, LEFT_IA_ID) %>% 
    summarise() %>% arrange(LEFT_IA_ID) %>% 
    mutate(N = n())
  
  L_bad_id <- 0
  for (x in 1:nrow(Lias)) {
    if (Lias[x, "LEFT_IA_ID"] >= 0 & Lias[x, "LEFT_IA_ID"] <= 8) {
    } else {
      L_bad_id <- L_bad_id + 1
    }
  }
  
  L_bad_labels <- filter(Lias, N > 1)
  
  # Print mappings
  message(paste(utils::capture.output(print(as.data.frame(Rias[,2:1]), row.names=FALSE)), collapse = "\n"))
  message(paste(utils::capture.output(print(as.data.frame(Lias[,2:1]), row.names=FALSE)), collapse = "\n"))
  
  # Determine messages
  if (R_bad_id > 0) {
    Rstop <- "Interest Area IDs for the right eye are not between 0 and 8. Please recode before proceeding with data processing."
    Rmsg <- NULL
  } else {
    Rmsg <- "Interest Area IDs for the right eye are coded appropriately between 0 and 8."
    Rstop <- NULL
  }
  
  if (L_bad_id > 0) {
    Lstop <- "Interest Area IDs for the left eye are not between 0 and 8. Please recode before proceeding with data processing."
    Lmsg <- NULL
  } else {
    Lmsg <- "Interest Area IDs for the left eye are coded appropriately between 0 and 8."
    Lstop <- NULL
  }
  
  if(nrow(R_bad_labels) > 0) {
    Rstop2 <- "Interest Area ID and label combinations for the right eye are not consistent. Please correct before proceeding with data processing."
    Rmsg2 <- NULL
  } else {
    Rmsg2 <- "Interest Area ID and label mapping combinations for the right eye are consistent."
    Rstop2 <- NULL
  }
  
  if(nrow(L_bad_labels) > 0) {
    Lstop2 <- "Interest Area ID and label combinations for the left eye are not consistent. Please correct before proceeding with data processing."
    Lmsg2 <- NULL
  } else {
    Lmsg2 <- "Interest Area ID and label mapping combinations for the left eye are consistent."
    Lstop2 <- NULL
  }
  
  # Print messages
  if(!is.null(Rmsg)){
    msg <- Rmsg
  } else{
    msg <- character()
  }
  if(!is.null(Lmsg)){
    if(length(msg)>0){
      msg <- paste(msg, Lmsg, sep = "\n")
    } else{
      msg <- Lmsg
    }
  }
  if(!is.null(Rmsg2)){
    if(length(msg)>0){
      msg <- paste(msg, Rmsg2, sep = "\n")
    } else{
      msg <- Rmsg2
    }
  }
  if(!is.null(Lmsg2)){
    if(length(msg)>0){
      msg <- paste(msg, Lmsg2, sep = "\n")
    } else{
      msg <- Lmsg2
    }
  }
  if(length(msg)==0){
    msg <- NULL
  } else {message(msg)
  }
  
  # Print errors
  if(!is.null(Rstop)){
    stp <- Rstop
  } else{
    stp <- character()
  }
  if(!is.null(Lstop)){
    if(length(stp)>0){
      stp <- paste(stp, Lstop, sep = "\n")
    } else{
      stp <- Lstop
    }
  }
  if(!is.null(Rstop2)){
    if(length(stp)>0){
      stp <- paste(stp, Rstop2, sep = "\n")
    } else{
      stp <- Rstop2
    }
  }
  if(!is.null(Lstop2)){
    if(length(stp)>0){
      stp <- paste(stp, Lstop2, sep = "\n")
    } else{
      stp <- Lstop2
    }
  }
  if(length(stp)==0){
    stp <- NULL
  } else {stop(stp)
  }
}