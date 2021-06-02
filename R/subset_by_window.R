subset_by_window <- function(data, 
                             rezero = TRUE,
                             remove = TRUE,
                             window_start_msg = NULL, window_end_msg = NULL, msg_col = NULL,
                             window_start_col = NULL, window_end_col = NULL,
                             window_start_time= NULL, window_end_time= NULL,
                             quiet = FALSE
) {
  
  ## Helper:
  .safe_msg_checker = function(msg_vec, msg, time_vec, ppt_vec, trial_vec) {
    bool = (msg_vec==msg)
    if (length(which(bool)) != 1) {
      warning("The message ", msg, 
              " does not appear *exactly* one time for participant '", ppt_vec[1], 
              "' on trial '", trial_vec[1], "'. Trial will be removed from dataset.")
      if (is.integer(time_vec)) return(NA_integer_)
      else return(NA_real_)
    }
    return(time_vec[which(bool)])
  }
  
  # Prelims:
  orig_classes <- class(data)
  data_options <- attr(data, "eyetrackingR")$data_options
  colname_symbols <- purrr::map(data_options[grep(names(data_options), pattern="column$")], as.name) 
  #print(colname_symbols)
  
  if (is.null(data_options)) {
    stop("It appears your dataframe doesn't have information that eyetrackingR needs. ",
         "Did you run `make_eyetracking_r` data on it originally?",
         "If so, this information has been removed. This can happen when using functions that ",
         "transform your data significantly, like dplyr::summarise or dplyr::select.")
  }
  if (!(rezero | remove)) stop("If both 'rezero' and 'remove' are FALSE, then this function doesn't do anything!")
  
  # Which method?
  start_method_num <- !purrr::map_lgl( list(window_start_msg, window_start_col, window_start_time), is.null )
  stop_method_num <- !purrr::map_lgl( list(window_end_msg, window_end_col, window_end_time), is.null )
  if ( sum(start_method_num) > 1 | sum(stop_method_num) > 1 ) {
    stop("Please use exactly one of the methods for start/stop time (msg, column, or time).")
  }
  
  # Start Time:
  if (!any(start_method_num)) {
    if (rezero) stop("Rezero must be set to FALSE if no start time specified.")
    start_method_num <- c(FALSE, FALSE, TRUE)
    window_start_time <- -Inf
  }
  if (which(start_method_num) == 1) {
    # Message:
    if (!is.character(msg_col)) stop("Please enter a column name for the message column (in quotes).", call. = FALSE)
    data[[msg_col]] <- as.character(data[[msg_col]])
    data <- group_by_at(.tbl = data, .vars = c(data_options$participant_column, data_options$trial_column))
    
    #print(msg_col)
    data <- mutate(.data = data, 
                   .WindowStart = .safe_msg_checker(msg_vec = !!as.name(msg_col),
                                                    #msg_vec = msg_col,
                                                    msg = window_start_msg,
                                                    time_vec = !!colname_symbols$time_column,
                                                    ppt_vec = !!colname_symbols$participant_column,
                                                    #ppt_vec = participant_column,
                                                    trial_vec = !!colname_symbols$trial_column )
    )
    data <- ungroup(data)
  } else if (which(start_method_num) == 2) {
    # Column:
    data$.WindowStart <- data[[window_start_col]]
  } else if (which(start_method_num) == 3) {
    # Single Number:
    data$.WindowStart <- window_start_time
  } 
  
  # Stop Time:
  if (!any(stop_method_num)) {
    stop_method_num <- c(FALSE, FALSE, TRUE)
    window_end_time <- Inf
  }
  if (which(stop_method_num) == 1) {
    # Message:
    if (!is.character(msg_col)) stop("Please enter a column name for the message column (in quotes).", call. = FALSE)
    data[[msg_col]] <- as.character(data[[msg_col]])
    data <- group_by_at(.tbl = data, .vars = c(data_options$participant_column, data_options$trial_column))
    data <- mutate(.data = data,
                   .WindowEnd = .safe_msg_checker(msg_vec = !!as.name(msg_col),
                                                  #msg_vec = msg_col,
                                                  msg = window_end_msg,
                                                  time_vec = !!colname_symbols$time_column,
                                                  ppt_vec = !!colname_symbols$participant_column,
                                                  trial_vec = !!colname_symbols$trial_column )
    )
    data <- ungroup(data)
  } else if (which(stop_method_num) == 2) {
    # Column:
    data$.WindowEnd <- data[[window_end_col]]
  } else if (which(stop_method_num) == 3) {
    # Single Number:
    data$.WindowEnd <- window_end_time
  } 
  
  #
  if (!quiet) {
    new_len <- round(mean(data$.WindowEnd - data$.WindowStart, na.rm=TRUE),2)
    if (!is.infinite(new_len)) message("Avg. window length in new data will be ", new_len) 
  }
  
  # Subset
  df_subsetted <- filter(.data = data,
                         !is.na(.WindowEnd),
                         !is.na(.WindowStart))
  
  if (remove) {
    after_start <- df_subsetted[[data_options$time_column]] >= df_subsetted$.WindowStart
    before_end <- df_subsetted[[data_options$time_column]] < df_subsetted$.WindowEnd
    df_subsetted <- df_subsetted[which(after_start&before_end), , drop=FALSE]
  } 
  
  # Rezero
  if (rezero) {
    df_grouped <- group_by_at(df_subsetted, .vars = c(data_options$participant_column, data_options$trial_column))
    df_rezeroed <- mutate(.data = df_grouped, 
                          !!colname_symbols$time_column := (!!colname_symbols$time_column) - .WindowStart)
    out <- ungroup(df_rezeroed)
  } else {
    out <- df_subsetted
  }
  
  out <- select(out, -.WindowStart, -.WindowEnd)
  
  attr(out, "eyetrackingR") <- list(data_options = data_options)
  class(out) <- orig_classes
  
  out
}