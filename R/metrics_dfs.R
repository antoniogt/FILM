#' Title
#'
#' @param models
#' @param formula
#' @param model_names
#' @param val
#' @param metrics
#'
#' @return
#' @export
#'
#' @examples
metrics_dfs <- function(models, formula ,model_names, val,metrics){

  list_metrics <- list()

  class<- gsub(" ", "", unlist(strsplit(format(formula), split = "~"))[1])

  if(model_names %in% c("IPIP_RF","IPIP_RLOG")){
    for(i in 1:length(val)){
    list_metrics[[i]] <- metricas.todas(data.frame(
      obs = as.factor(val[[i]][,class]),
      pred= factor(FILM::predict_ipip(models[[i]], val[[i]][-which(colnames(val[[i]])==class)],val[[i]][,class],type="class"),labels=levels(val[[i]][,class])),
      prob= FILM::predict_ipip(models[[i]], val[[i]][-which(colnames(val[[i]])==class)],val[[i]][,class],type="prob"),
      obs.prob = as.numeric(ifelse(val[[i]][, class] == max, 1, 0))
    ))[metrics]
    }
  }
  else{
  for(i in 1:length(val)){
    list_metrics[[i]] <- metricas.todas(data.frame(
      obs = as.factor(val[[i]][,class]),
      pred= factor(predict(models[[i]], val[[i]][-which(colnames(val[[i]])==class)]),labels=levels(val[[i]][,class])),
      prob= predict(models[[i]], val[[i]][-which(colnames(val[[i]])==class)],type = "prob")[,max],
      obs.prob = as.numeric(ifelse(val[[i]][, class] == max, 1, 0))
    ))[metrics]
    }
  }
  return(list_metrics)
}
