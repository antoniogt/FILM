#' Function to get the metric for all dfs created on FILM methodology.
#'
#' @param ml: List of models
#' @param formula: Formula to test. Style: 'target_variable ~ .'
#' @param model_names: Model names to identify if it is an IPIP method.
#' @param val: List of validation datasets.
#' @param metrics: Vector of the wanted metric values.
#'
#' @return Named vector of metric values.
#' @export
#'
#' @examples
metrics_dfs <- function(ml, formula ,model_names, val,metrics){

  max<-levels(val[[1]][,class])[which.max(table(val[[1]][,class]))]

  list_metrics <- list()

  class<- gsub(" ", "", unlist(strsplit(format(formula), split = "~"))[1])

  if(model_names %in% c("IPIP_RF","IPIP_RLOG")){
    for(i in 1:length(val)){
      list_metrics[[i]] <- FILM::metric_probs(data.frame(
        obs = as.factor(val[[i]][,class]),
        pred= factor(FILM::predict_ipip(ml[[i]], val[[i]][-which(colnames(val[[i]])==class)],val[[i]][,class],type="class"),labels=levels(val[[i]][,class])),
        prob= FILM::predict_ipip(ml[[i]], val[[i]][-which(colnames(val[[i]])==class)],val[[i]][,class],type="prob"),
        obs.prob = as.numeric(ifelse(val[[i]][, class] == max, 1, 0)) # Arreglar lo de max por el nombre del level de la clase mayoritaria
      ))[metrics]
    }
  }
  else{
    for(i in 1:length(val)){
      list_metrics[[i]] <- FILM::metric_probs(data.frame(
        obs = as.factor(val[[i]][,class]),
        pred= factor(predict(ml[[i]], val[[i]][-which(colnames(val[[i]])==class)]),labels=levels(val[[i]][,class])),
        prob= predict(ml[[i]], val[[i]][-which(colnames(val[[i]])==class)],type = "prob")[,max],
        obs.prob = as.numeric(ifelse(val[[i]][, class] == max, 1, 0))
      ))[metrics]
    }
  }
  return(list_metrics)
}

