#' BAIC metric function
#'
#' @param metrics: Metric names
#' @param metric_values: Metric values in a list to calculate the BAIC value
#' @param props: Proportion of minority class of each dataset to get the BAIC value
#' @param fun: Function to obtain the weights of each metric
#'
#' @return number
#' @export
#'
#' @examples
#'
BAIC <- function (metrics, metric_values, props, fun = NULL) {
  value <- c()
  aux <- data.frame(props = props)
  colnames(aux) <- "p_min"
  for (j in metrics) {
    c <- colnames(aux)
    aux <- cbind(aux, data.frame(v = as.double(unlist(metric_values)[which(sub(".*\\.", "", names(unlist(metric_values))) == 
                                                                             j)])))
    colnames(aux) <- c(c, j)
  }
  aux<- scale(aux)
  cor.pe <- cor(aux, method = c("pearson"))
  cor_pmin <- as.double(cor.pe["p_min", metrics])
  if (is.null(fun)) {
    weights = FILM::f_gauss(cor_pmin, 1, 0, 0.15)
  }
  else {
    weights <- fun(cor_pmin)
  }
  value <- as.double(c(unlist(metric_values)[which(sub(".*\\.", "", names(unlist(metric_values))) %in% metrics & sub(".*_DF(\\d+).*", "\\1",names(unlist(metric_values))) == "1")])) %*% 
    weights
  return(list(
    BAIC_value = value,
    cor_pmin  = cor_pmin
  ))
}
