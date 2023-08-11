percent_positive <- function(model_result) {
  perc_pos <- (sum(model_result > 0) / 
                 length(model_result)) * 100
return( cat(perc_pos, "%", "positive outcomes"))
}
