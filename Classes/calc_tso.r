calc_tso <- function (count, sentiment)
{
  sentiments.ts <- as.ts(abs(sentiment))
  counts.ts <- as.ts(count)
  resSent <- tso(y = sentiments.ts, types = c("SLC","AO","TC"), discard.method = "bottom-up", tsmethod = "auto.arima", args.tsmethod = list(allowdrift = FALSE, D=1, ic = "bic")) 
  resCount<- tso(y = counts.ts, types = c("SLC","AO", "TC"), discard.method = "bottom-up", tsmethod = "auto.arima", args.tsmethod = list(allowdrift = FALSE, D=1, ic = "bic")) 
  plot(resSent)
  plot(resCount)
  
  res_tso_UNION = union(resSent$outliers$ind, resCount$outliers$ind)
  res_tso_INTERSECT = intersect(resSent$outliers$ind, resCount$outliers$ind)
  
  return(list(A=resCount$outliers$ind,B=resSent$outliers$ind,union=res_tso_UNION,intersect=res_tso_INTERSECT, s_p = resSent, c_p = resCount))
}