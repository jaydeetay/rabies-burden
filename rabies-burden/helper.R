# The transpose function t() barfs on an empty data frame - give it
# something to chew on.
row_slice_or_empty<-function(data_frame, row_key) {
  if (is.null(row_key)) {
    return(data.frame(c("123")))
  } else {
    return(data_frame[row_key,])
  }
}

# Merges two data tables with 'extra' taking precendence
merge_data<-function(base, extra, key) {
  commonNames <- names(base)[which(colnames(base) %in% colnames(extra))]
  commonNames <- commonNames[commonNames != key]
  dfmerge<- merge(extra,base,by=key,all=T)
  for(i in commonNames){
    left <- paste(i, ".x", sep="")
    right <- paste(i, ".y", sep="")
    dfmerge[is.na(dfmerge[left]),left] <- dfmerge[is.na(dfmerge[left]),right]
    dfmerge[right]<- NULL
    colnames(dfmerge)[colnames(dfmerge) == left] <- i
  }
  return(dfmerge)
}