#' Metrics
#'
#' @param data: Data frame with the original values in column 'obs', predictions in 'pred' column
#' @param lev
#' @param model
#'
#' @return Metric values for the given predictions and original values.
#' @export
#'
#' @examples

metrics<-function(data, lev = levels(as.factor(data$obs)), model = NULL){
  c(
    ACCURACY = MLmetrics::Accuracy(data[, "pred"], data[, "obs"]),
    SENS = sensitivity(data[, "pred"],data[, "obs"],positive=levels(as.factor(data$obs))[which.max(table(data$obs))],negative=levels(as.factor(data$obs))[which.min(table(data$obs))]),
    SPEC = specificity(data[, "pred"], data[, "obs"],positive=levels(as.factor(data$obs))[which.max(table(data$obs))],negative=levels(as.factor(data$obs))[which.min(table(data$obs))]),
    PPV = posPredValue(data[, "pred"], data[, "obs"],positive=levels(as.factor(data$obs))[which.max(table(data$obs))],negative=levels(as.factor(data$obs))[which.min(table(data$obs))]),
    NPV = negPredValue(data[, "pred"], data[, "obs"],positive=levels(as.factor(data$obs))[which.max(table(data$obs))],negative=levels(as.factor(data$obs))[which.min(table(data$obs))]),
    KAPPA = psych::cohen.kappa(cbind(data[, "obs"],data[, "pred"]))$kappa,
    BAL_ACC = (sensitivity(data[, "pred"],data[, "obs"],positive=levels(as.factor(data$obs))[which.max(table(data$obs))],negative=levels(as.factor(data$obs))[which.min(table(data$obs))]) + specificity(data[, "pred"], data[, "obs"],positive=levels(as.factor(data$obs))[which.max(table(data$obs))],negative=levels(as.factor(data$obs))[which.min(table(data$obs))]))/2,
    F1 = MLmetrics::F1_Score(data[,"pred"],data[,"obs"]),
    MCC = mltools::mcc(data[,"pred"],data[,"obs"]),
    GEOM = sqrt(sensitivity(data[, "pred"],data[, "obs"],positive=levels(as.factor(data$obs))[which.max(table(data$obs))],negative=levels(as.factor(data$obs))[which.min(table(data$obs))])*specificity(data[, "pred"], data[, "obs"],positive=levels(as.factor(data$obs))[which.max(table(data$obs))],negative=levels(as.factor(data$obs))[which.min(table(data$obs))]))
  )
}
