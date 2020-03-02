calc_anomalize <- function (date, count, sentiment, alpha)
{
  scaled.sentiments.tbl <- as.tbl(data.frame(date =as.Date(date), sent= abs(sentiment)))
  scaled.counts.tbl <- as.tbl(data.frame(date =as.Date(date), count= count))
  tryCatch(
    expr = {
      res_anomalize_sent <- scaled.sentiments.tbl %>%
        time_decompose(sent, method = "twitter", frequency = "auto", trend = "auto") %>%
        anomalize(remainder, method = "gesd", alpha = alpha, max_anoms = 0.2) %>%
        time_recompose() 
      
      plot(res_anomalize_sent %>%
             plot_anomalies(time_recomposed = TRUE, ncol = 2, alpha_dots = 0.5))
    },
    error=function(e){
      print('Anomalize not applicable')
      res_anomalize_sent <- NULL
    }
  )
  
  tryCatch(
    expr = {
      res_anomalize_count <- scaled.counts.tbl %>%
        time_decompose(count) %>%
        anomalize(remainder, alpha = alpha) %>%
        time_recompose() 
      
      plot(res_anomalize_count %>%
             plot_anomalies(time_recomposed = TRUE, ncol = 2, alpha_dots = 0.5))
    },
    error=function(e){
      print('Anomalize not applicable')
      res_anomalize_count <- NULL
    }
  )
  
  cID <- which(res_anomalize_count$anomaly=="Yes")
  sID <- which(res_anomalize_sent$anomaly=="Yes")
  
  res_anomalize_union<-union(cID,sID)
  res_anomalize_intersect<-intersect(cID,sID)
  
  return(list(A=cID,B=sID,union=res_anomalize_union,intersect=res_anomalize_intersect))
}