#' Title
#'
#' @param ensemble
#' @param x
#' @param y
#' @param q
#'
#' @return
#' @export
#'
#' @examples
predict_ipip<- function(ensemble, x, y, q = 0.5){

  min<- levels(y)[which.min(table(y))]
  max<- levels(y)[which.max(table(y))]

  if(type=="class"){
  pred <- as.data.frame(lapply(ensemble, function(e) FILM::predict_film(e,x,y,type="class")))
  pred <- apply(pred, 1, function(x) prop.table(table(x))[max])
  pred<-ifelse(is.na(pred) | pred<q, min, max)
  return(pred)
  }
  if(type=="prob"){
    pred <- as.data.frame(lapply(ensemble, function(e) FILM::predict_film(e,x,y,type="prob")))
    pred <- rowSums(pred)/sum(unlist(lapply(ensemble,length)))
    return(pred)
  }
}
