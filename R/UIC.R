#' UIC metric function
#'
#' @param metrics: Metric names
#' @param metric_values: Metric values in a list to calculate the UIC value
#' @param props: Proportion of minority class of each dataset to get the UIC value
#' @param fun: Function to obtain the weights of each metric
#'
#' @return number
#' @export
#'
#' @examples
#'
UIC<-function(metrics,metric_values,props,fun=NULL){
  value <- c()

  aux <- data.frame()
  aux<-cbind(aux,props)
  colnames(aux)<-"p_min"

  for(j in metrics){
    c<-colnames(aux)
    aux <- cbind(aux,data.frame(v=metric_values))
    colnames(aux)<-c(c,j)
  }
  cor.pe <- cor(aux,method=c("pearson"))
  cor_pmin<- as.double(cor.pe["p_min",metrics])
  if(is.null(fun)){
    fun=FILM::gaussian(cor_pmin,1,0,0.15)
  }
  weights<-fun(cor_pmin)

  value<-as.double(c(list_accumulative_metrics[[v]][metrics])) %*% weights


  return(value)
}
