make_time_sequence_data <- function(data, predictor_columns, aoi_columns, timecolumn, time_bin_size){
  ## add time bins
  mintime=min(data[,timecolumn])
  maxtime=max(data[,timecolumn])
  maxbinnumber=(maxtime-mintime)/time_bin_size
  timecolumn2=data[,timecolumn]
  data$TimeBin=NA
  data$BinStart=NA
  data$BinEnd=NA
  data$Time=NA
  for (i in 0:maxbinnumber){
    lowerlim=mintime+(time_bin_size*i)
    upperlim=mintime+(time_bin_size*(i+1))
    data$TimeBin[timecolumn2>=lowerlim & timecolumn2<upperlim]=i
    data$BinStart[timecolumn2>=mintime+(time_bin_size*i) & timecolumn2<mintime+(time_bin_size*(i+1))]=lowerlim
    data$BinEnd[timecolumn2>=mintime+(time_bin_size*i) & timecolumn2<mintime+(time_bin_size*(i+1))]=upperlim
    data$Time[timecolumn2>=mintime+(time_bin_size*i) & timecolumn2<mintime+(time_bin_size*(i+1))]=lowerlim+(upperlim-lowerlim)/2
  }
  timecolumn=substitute(timecolumn)
  groups = c("TimeBin", "BinStart", "BinEnd", "Time", predictor_columns)
  no.of.IAs=length(aoi_columns)
  for (j in 1:no.of.IAs){
    IA <-aoi_columns[[j]]
    Summed <-paste("Summed", j, sep = "") 
    Reduce <- ddply(data, groups, .fun = function(xx){
      c(Prop = mean(xx[,IA],na.rm=TRUE))})
    Reduce$AOI=IA
    assign(Summed, Reduce)
  }
  FinalSummed=rbind(Summed1, Summed2, Summed3, Summed4, Summed5)
  time_bin_column <- FinalSummed$TimeBin
  max_degree <- min( length(unique(time_bin_column))-1 , 7 )
  if (max_degree < 7) warning("Fewer time bins than polynomial degrees-- consider decreasing size of time bin.")
  orthogonal_polynomials <- poly(sort(as.vector(unique(time_bin_column))), max_degree)
  time_codes <- data.frame(
    sort(as.vector(unique(time_bin_column))),
    orthogonal_polynomials[, c(1:max_degree)]
  )
  colnames(time_codes) <- c('TimeBin',paste0("ot", 1:max_degree))
  Summed_Data <- merge(FinalSummed, time_codes, by='TimeBin')
  return(Summed_Data)
}