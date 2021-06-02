make_eyetrackingr_data <- function(data, 
                                   participant_column,
                                   trackloss_column,
                                   time_column,
                                   trial_column,
                                   aoi_columns,
                                   treat_non_aoi_looks_as_missing,
                                   item_columns = NULL) {
  ## Helpers:
  as.numeric2 <- function(x) as.numeric(as.character(x))
  is.logical2 <- function(x) {
    if (is.logical(x)) {
      return(TRUE)
    } else if (is.numeric(x)) {
      return(FALSE)
    } else {
      stop("One of your columns (", col, ") could not be converted to the correct format (TRUE/FALSE), ",
           "please do so manually.", call. = FALSE)
    }
  }
  check_then_convert <- function(x, checkfunc, convertfunc, colname) {
    if (!checkfunc(x)) {
      message("Converting ", colname, " to proper type.")
      x <- convertfunc(x)
    } 
    if (colname=="Trackloss" & any(is.na(x))) {
      warning("Found NAs in trackloss column, these will be treated as TRACKLOSS=FALSE.", immediate. = TRUE, call. = FALSE)
      x <- if_else(is.na(x), FALSE, x)
    }
    return(x)
  }
  
  ## Data Options:
  data_options = list(participant_column = participant_column,
                      trackloss_column = trackloss_column,
                      time_column = time_column,
                      trial_column = trial_column,
                      item_columns = item_columns,
                      aoi_columns = aoi_columns,
                      treat_non_aoi_looks_as_missing = treat_non_aoi_looks_as_missing)
  
  ## Check for Reserved Column Name:
  if (data_options$time_column == "Time") {
    stop("We apologize for the inconvenience, but your `time_column` cannot be called 'Time' ",
         "(this is a reserved name that eyetrackingR uses). Please rename.")
  } 
  if ("Time" %in% colnames(data)) {
    warning("Your dataset has a column called 'Time', but this column name is reserved for eyetrackingR. Will rename to 'TimeOriginal'...")
    data$TimeOriginal <- data$Time
    data$Time <- NULL
  }
  
  ## Verify Columns:
  out <- data
  col_type_converter <- list(participant_column = function(x) check_then_convert(x, is.factor, as.factor, "Participants"),
                             time_column = function(x) check_then_convert(x, is.numeric, as.numeric2, "Time"),
                             trial_column = function(x) check_then_convert(x, is.factor, as.factor, "Trial"),
                             trackloss_column = function(x) check_then_convert(x, is.logical2, as.logical, "Trackloss"),
                             item_columns = function(x) check_then_convert(x, is.factor, as.factor, "Item"),
                             aoi_columns = function(x) check_then_convert(x, is.logical2, as.logical, "AOI"))
  
  for (col in names(col_type_converter)) {
    for (i in seq_along(data_options[[col]])) {
      if (is.null(out[[data_options[[col]][i]]]))
        stop("Data are missing: ", col)
      out[[data_options[[col]][i]]] <- col_type_converter[[col]](out[[data_options[[col]][i]]])
    }
  }
  
  ## Deal with Non-AOI looks:
  if (treat_non_aoi_looks_as_missing) {
    any_aoi <- rowSums(as.matrix(out[,data_options$aoi_columns,drop=FALSE]), na.rm = TRUE) > 0
    out[[data_options$trackloss_column]][!any_aoi] <- TRUE
  }
  
  ## Set All AOI rows with trackloss to NA:
  # this ensures that any calculations of proportion-looking will not include trackloss in the denominator
  for (aoi in data_options$aoi_columns) {
    out[[aoi]][ out[[data_options$trackloss_column]] ] <- NA
  }
  
  # Check for duplicate values of Trial column within Participants
  #duplicates <- out %>%
  #  group_by_at(.vars = c(data_options$participant_column, data_options$trial_column, data_options$time_column) ) %>%
  #  count() %>%
  #  ungroup() %>%
  #  filter(n > 1)
  
  #if (nrow(duplicates) > 0) {
  # print(duplicates)
  #  stop("It appears that `trial_column` is not unique within participants. See above for a summary ",
  #       "of which participant*trials have duplicate timestamps. eyetrackingR requires that each participant ",
  #       "only have a single trial with the same `trial_column` value. If you repeated items in your experiment, ",
  #       "use `item_column` to specify the name of the item, and set `trial_column` to a unique value ",
  #       "(e.g., the trial index).")
  #}
  
  ## Assign attribute:
  out <- arrange_at(.tbl = out, .vars = c(participant_column, trial_column, time_column))
  out <- as_tibble(out)
  class(out) <- c("eyetrackingR_data", "eyetrackingR_df", class(out))
  attr(out, "eyetrackingR") <- list(data_options = data_options)
  return(out)
  
}