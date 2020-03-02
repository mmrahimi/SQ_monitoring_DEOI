calc_shesd <- function(date, count,sentiment, alpha)
{
  scaled.sentiments <- data.frame(date =as.POSIXct(date), sent= sentiment)
  scaled.count <- data.frame(date =as.POSIXct(date), count= count)
  scaled.finalDF<-data.frame(date = as.POSIXct(date), sentiment = scaled.sentiments, count = scaled.count)
  
  res_Sentiment_SHESD = NULL
  tryCatch(
    expr = {
      res_Sentiment_SHESD = AnomalyDetectionTs(scaled.sentiments, max_anoms=0.1,alpha = alpha , direction='both', plot=TRUE)
      plot(res_Sentiment_SHESD$plot, main="S-H-ESD on Sentiment Time-Series")
    }, 
    error=function(e){
      print('Anom detection not applicable')
    }
  )
  
  res_Count_SHESD = NULL
  tryCatch(
    expr = {
      res_Count_SHESD = AnomalyDetectionTs(scaled.count, max_anoms=0.1,alpha = alpha , direction='both', plot=TRUE)
      plot(res_Count_SHESD$plot,main="S-H-ESD on Frequency Time-Series")
    }, 
    error=function(e){
      print('Anom detection not applicable')
    }
  )
  
  res_SHESD_UNION = NULL
  tryCatch(
    expr = {
      res_SHESD_UNION = union(res_Count_SHESD$anoms$timestamp, res_Sentiment_SHESD$anoms$timestamp)
    },
    error = function(e){ 
      res_SHESD_UNION = NULL
    }
  )
  res_SHESD_Intersect = intersect(res_Count_SHESD$anoms$timestamp,res_Sentiment_SHESD$anoms$timestamp)
  
  return(list(A=res_Count_SHESD$anoms$timestamp, B=res_Sentiment_SHESD$anoms$timestamp, union=res_SHESD_UNION,intersect=res_SHESD_Intersect,s_plot=res_Sentiment_SHESD$plot, c_plot = res_Count_SHESD$plot))
}