#' Title
#'
#' @param ensemble
#' @param x
#' @param y
#' @param type
#' @param q
#'
#' @return
#' @export
#'
#' @examples
predict_film<-function(ensemble, x, y, type="class", q = 0.75){

  min<- levels(y)[which.min(table(y))]
  max<- levels(y)[which.max(table(y))]

  if(type=="class"){
  pred <- data.frame(matrix(nrow=nrow(x),ncol=0))
  for(modelo in ensemble) pred <- cbind(pred, predict(modelo,x))
  pred <- apply(pred, 1, function(x) prop.table(table(x))[max])
  ifelse(is.na(pred) | pred<q, min, max)
  }
  if(type=="prob"){
    pred <- data.frame(matrix(nrow=nrow(x),ncol=0))
    for(modelo in ensemble) pred <- cbind(pred, predict(modelo,x,type="prob")[,max])
    pred<- rowSums(pred)
    pred
  }
}
