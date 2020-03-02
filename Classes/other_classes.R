asDate <- function(dt)
{
  if (!(is.null(dt))){
    return(as.Date(dt))
  }
  else{
    return(NA)
  }
}