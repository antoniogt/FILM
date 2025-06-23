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
metrics_dfs <- function(ml, formula, model_names, val, metrics) 
{
  class <- gsub(" ", "", unlist(strsplit(format(formula), split = "~"))[1])
  max   <- levels(val[[1]][, class])[which.max(table(val[[1]][, class]))]
  list_metrics <- list()
  
  if (any(model_names %in% c("IPIP_RF", "IPIP_RLOG"))) {
    for (i in 1:length(val)) {
      predic<- factor(
            FILM::predict_ipip(ml[[i]], val[[i]][-which(colnames(val[[i]]) == class)], 
                               val[[i]][, class], type = "class"), 
            labels = levels(val[[i]][, class])
          )
      levels(predic)<- levels(val[[i]][, class])
      list_metrics[[i]] <- FILM::metric_probs(
        data.frame(
          obs  = as.factor(val[[i]][, class]),
          pred = predic,
          prob     = FILM::predict_ipip(ml[[i]], val[[i]][-which(colnames(val[[i]]) == class)], 
                                        val[[i]][, class], type = "prob"),
          obs.prob = as.numeric(ifelse(val[[i]][, class] == max, 1, 0))
        )
      )[metrics]
    }
    
    
    } else if (any(model_names %in% c("Smoteboost_RF","Smoteboost_RLOG"))) {
    
    for (i in seq_len(length(val))) {
      
      modelBst_obj  <- ml[[i]]
      list_model    <- modelBst_obj[[1]]  # weak learners
      a             <- modelBst_obj[[2]]  # errorEstimation
      
      a <- log(1/a, base = exp(1)) / sum(log(1/a, base = exp(1)))
      
      newdata_sinY <- val[[i]][, -which(colnames(val[[i]]) == class), drop=FALSE]
      
      prob_list <- vector("list", length(list_model))
      
      for (k in seq_along(list_model)) {
        w_model <- list_model[[k]]
        
        if (w_model$method[1]== "ranger") {
          tmp_prob <- predict(w_model, newdata_sinY, type = "prob")[, max]
          prob_list[[k]] <- tmp_prob
          
        } else if (w_model$method[1]== "glm") {
          tmp_prob <- predict(w_model, newdata_sinY, type = "prob")[, max]
          prob_list[[k]] <- tmp_prob
          
        } else {
          stop("Unavailable weak learner for SmoteBoost.")
        }
      } 
      
      prob_mat <- do.call(cbind, prob_list)  # cbind cada vector columna
      weighted_prob_mat <- sweep(prob_mat, 2, a, `*`)
      prob_final <- rowSums(weighted_prob_mat)
      
      pred_class <- ifelse(prob_final > 0.5, max, setdiff(levels(val[[i]][, class]),max))
      pred_class <- as.factor(pred_class)
      
      predic<- factor(pred_class, labels = levels(val[[i]][, class]))
      levels(predic)<- levels(val[[i]][, class])
      df_pred <- data.frame(
        obs      = as.factor(val[[i]][, class]),
        pred     = predic,  # Ajusta niveles si quieres
        prob     = prob_final,
        obs.prob = as.numeric(ifelse(val[[i]][, class] == max, 1, 0))
      )
      
      list_metrics[[i]] <- FILM::metric_probs(df_pred)[metrics]
    } # fin for i
    
  } else if (any(model_names %in% c("Underbagging_RF","Underbagging_RLOG"))) {
    
    for (i in seq_len(length(val))) {
      
      # Recuperamos el objeto con class "UnderBagModel"
      bag_obj     <- ml[[i]]
      list_models <- bag_obj$weakLearners
      
      # 'max' es la clase mayoritaria en val[[i]] (como en tu pipeline),
      #   class es la variable dependiente
      newdata_sinY <- val[[i]][, -which(colnames(val[[i]]) == class), drop=FALSE]
      
      # Acumulamos las probabilidades de 'max'
      prob_list <- vector("list", length(list_models))
      for(k in seq_along(list_models)){
        m_k <- list_models[[k]]
        
        # Al ser caretaker models => predict(..., type="prob") da un DF c/col = niveles
        prob_k <- predict(m_k, newdata_sinY, type="prob")[, max]
        prob_list[[k]] <- prob_k
      }
      # Creamos una matriz: filas = instancias, cols = #models
      prob_mat <- do.call(cbind, prob_list)
      
      # Voto uniforme: la prob final = average => rowMeans(prob_mat)
      prob_final <- rowMeans(prob_mat)
      
      # Predicción de clase => si prob_final >= 0.5 => "max", si no => la otra
      pred_class <- ifelse(prob_final >= 0.5, max, setdiff(levels(val[[i]][, class]), max))
      pred_class <- factor(pred_class, levels = levels(val[[i]][, class]))
      levels(pred_class)<- levels(val[[i]][, class])
      # Construimos df_pred para metric_probs
      df_pred <- data.frame(
        obs      = as.factor(val[[i]][, class]),
        pred     = pred_class,
        prob     = prob_final,  # prob de 'max'
        obs.prob = as.numeric(val[[i]][, class] == max)
      )
      
      # Calcular las métricas
      list_metrics[[i]] <- FILM::metric_probs(df_pred)[metrics]
    }
    
  } else {
    
    for (i in 1:length(val)) {
      predic<- factor(predict(ml[[i]], val[[i]][-which(colnames(val[[i]]) == class)]),
                        labels = levels(val[[i]][, class]))
      levels(predic)<-levels(val[[i]][, class])
      list_metrics[[i]] <- FILM::metric_probs(
        data.frame(
          obs  = as.factor(val[[i]][, class]),
          pred = predic,
          prob = predict(ml[[i]], val[[i]][-which(colnames(val[[i]]) == class)], 
                         type = "prob")[, max],
          obs.prob = as.numeric(ifelse(val[[i]][, class] == max, 1, 0))
        )
      )[metrics]
    }
  }
  
  return(list_metrics)
}
