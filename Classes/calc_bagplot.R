calc_bagplot <- function(aplDF)
{
  outlier <- vector()
  plt <- bagplot(cbind(aplDF$x,aplDF$y),pch=16,cex=2)
  list_Temp <- plt$pxy.outlier
  
  for (i in 1:length(list_Temp)/2)
  {
    outlier <-  c(outlier, which(aplDF$x == list_Temp[i,1] & aplDF$y == list_Temp[i,2]))
  }
  return(unique(outlier))
}
