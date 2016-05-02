# The transpose function t() barfs on an empty data frame - give it
# something to chew on.
row_slice_or_empty<-function(data_frame, row_key) {
  if (is.null(row_key)) {
    return(data.frame(c("123")))
  } else {
    return(data_frame[row_key,])
  }
}