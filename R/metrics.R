#' Title
#'
#' @param data
#' @param lev
#' @param model
#'
#' @return
#' @export
#'
#' @examples
metrics<-function(data, lev = levels(as.factor(data$obs)), model = NULL){
  c(
    ACCURACY = MLmetrics::Accuracy(data[, "pred"], data[, "obs"]),
    SENS = sensitivity(data[, "pred"],data[, "obs"],positive="dos",negative="uno"),
    SPEC = specificity(data[, "pred"], data[, "obs"],positive="dos",negative="uno"),
    PPV = posPredValue(data[, "pred"], data[, "obs"],positive="dos",negative="uno"),
    NPV = negPredValue(data[, "pred"], data[, "obs"],positive="dos",negative="uno"),
    KAPPA = psych::cohen.kappa(cbind(data[, "obs"],data[, "pred"]))$kappa,
    BAL_ACC = (sensitivity(data[, "pred"],data[, "obs"],positive="dos",negative="uno") + specificity(data[, "pred"], data[, "obs"],positive="dos",negative="uno"))/2,
    F1 = MLmetrics::F1_Score(data[,"pred"],data[,"obs"]),
    AUC = MLmetrics::AUC(data[,"prob"],data[,"obs.prob"]),
    PR = MLmetrics::PRAUC(data[,"prob"],data[,"obs.prob"]),
    MCC = mltools::mcc(data[,"pred"],data[,"obs"]),
    GEOM = sqrt(sensitivity(data[, "pred"],data[, "obs"],positive="dos",negative="uno")*specificity(data[, "pred"], data[, "obs"],positive="dos",negative="uno"))
  )
}
