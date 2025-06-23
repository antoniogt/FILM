#' FILM function for doing the entire methodology and get the best model accordin
#'
#' @param dataset: Dataset of the binary classification problem
#' @param formula: Style: 'target_variable ~ .'
#' @param df_aux: Number of auxiliar datasets with different minority class function
#' @param IAAs: Imbalance Aware Approaches such as 'SMOTE', 'IPIP', 'ROSE', 'Upsample' and 'Downsample'
#' @param models: ML techniques to be trained after using some IAA: 'glm' or 'ranger'.
#' @param hyperparameters: Hiperparameters for 'ranger' method.
#' @param metrics: Vector of metrics to get in order to calculate BAIC metric. Possible values: 'ACCURACY' ,'SENS', 'SPEC', 'PPV', 'NPV', 'KAPPA' , 'BAL_ACC',  'F1', 'AUC', 'PR', 'MCC', 'GEOM'
#' @param cv: Number K of folds for a K-Folds Cross Validation.
#' @param prop_min: Minimum of proportion of the minority class for the resample auxiliar datasets.
#' @param prop_max: Maximum of proportion of the minority class for the resample auxiliar datasets.
#' @param tC: Train Control of caret train() function.
#' @param metric_max: Metric from 'metrics' to maximize when training ML techniques.
#' @param num.trees: Number of trees of the 'ranger' caret method.
#'
#' @return BAIC values, best model, all models
#' @export
#'
#' @examples
FILM <- function (dataset, formula, df_aux = 6, IAAs = c("IPIP", "SMOTE", 
                                                              "ROSE"), models = c("ranger", "glm"), hyperparameters = NULL, 
                       metrics = c("ACCURACY", "KAPPA", "F1"), cv = 5, prop_min = 0.05, 
                       prop_max = 0.4, tC = trainControl(summaryFunction = FILM::metrics, 
                                                         allowParallel = TRUE, classProbs = TRUE), metric_max = "KAPPA", 
                       num.trees = 200, return.baic = T) 
{
  
  class <- gsub(" ", "", unlist(strsplit(format(formula), split = "~"))[1])
  dataset[, class] <- as.factor(dataset[, class])
  models_trained <- list()
  names_models <- c()
  min <- levels(dataset[, class])[which.min(table(dataset[, 
                                                          class]))]
  max <- levels(dataset[, class])[which.max(table(dataset[, 
                                                          class]))]
  p0 <- prop.table(table(dataset[, class]))[min]
  props <- c()
  if (p0 < prop_min) {
    for (i in 1:(df_aux)) {
      props <- c(props, p0 + i * (prop_max - p0)/(df_aux))
    }
  }else {
    for (i in 1:(df_aux/2)) {
      props <- c(props, p0 + i * (prop_max - p0)/(df_aux/2))
    }
    for (i in 1:(df_aux/2)) {
      props <- c(props, p0 - i * (p0 - prop_min)/(df_aux/2))
    }
  }
  props <- as.double(props)
  subset_min <- dataset[which(dataset[, class] == min), ]
  subset_max <- dataset[which(dataset[, class] == max), ]
  subdatasets <- list(data.frame(dataset))
  if (p0 < prop_min) {
    for (i in 1:(df_aux)) {
      n <- round(nrow(subset_min)/props[i]) - nrow(subset_min)
      subset_max_down <- subset_max[sample(1:nrow(subset_max), 
                                           size = n, replace = F), ]
      subdatasets[[i + 1]] <- (data.frame(rbind(subset_max_down, 
                                                subset_min)))
    }
  }else {
    for (i in 1:(df_aux/2)) {
      n <- round(nrow(subset_min)/props[i]) - nrow(subset_min)
      subset_max_down <- subset_max[sample(1:nrow(subset_max), 
                                           size = n, replace = F), ]
      subdatasets[[i + 1]] <- (data.frame(rbind(subset_max_down, 
                                                subset_min)))
    }
    for (i in ((df_aux/2) + 1):df_aux) {
      n <- round(nrow(subset_max)/(1 - props[i])) - nrow(subset_max)
      subset_min_down <- subset_min[sample(1:nrow(subset_min), 
                                           size = n, replace = F), ]
      subdatasets[[i + 1]] <- (data.frame(rbind(subset_max, 
                                                subset_min_down)))
    }
  }
  props <- c(p0, props)
  folds.max_list <- list()
  folds.min_list <- list()
  list_metrics_total <- list()
  list_accumulative_metrics <- list()
  test_set<-list()
  for (i in 1:length(subdatasets)) {
    
       index<-createDataPartition(subdatasets[[i]][, class], 
                        p = 0.8, list = FALSE)
    
    test_set[[i]] <- subdatasets[[i]][-index,]
    subdatasets[[i]]<-subdatasets[[i]][index,]

  }
  
  for (i in 1:length(subdatasets)) {
    folds.max_list[[i]] <- createFolds(subdatasets[[i]][which(subdatasets[[i]][, 
                                                                               class] == max), class], k = cv)
    folds.min_list[[i]] <- createFolds(subdatasets[[i]][which(subdatasets[[i]][, 
                                                                               class] == min), class], k = cv)
  }
  for (df in 1:length(subdatasets)) {
    datasets <- list()
    datasets_val <- list()
    datasets_test <- list()
    for (folds in 1:cv) {
      train_set <- rbind(subdatasets[[df]][which(subdatasets[[df]][, 
                                                                   class] == max), ][-folds.max_list[[df]][[folds]], 
                                                                   ], subdatasets[[df]][which(subdatasets[[df]][, 
                                                                                                                class] == min), ][-folds.min_list[[df]][[folds]], 
                                                                                                                ])
      datasets[[length(datasets) + 1]] <- train_set
      datasets_val[[length(datasets_val) + 1]] <- rbind(subdatasets[[df]][which(subdatasets[[df]][, 
                                                                  class] == max), ][folds.max_list[[df]][[folds]], 
                                                                  ], subdatasets[[df]][which(subdatasets[[df]][, 
                                                                                                               class] == min), ][folds.min_list[[df]][[folds]], 
                                                                                                               ])
      datasets_test[[length(datasets_test) + 1]] <- test_set[[df]]
    }
    for (i in 1:cv) {
      if ("ROSE" %in% IAAs) {
        rose.data <- ROSE::ROSE(formula, data = datasets[[i]])$data
        if ("ranger" %in% models) {
          m <- train(formula, data = rose.data, num.trees = num.trees, 
                     importance = "impurity", method = "ranger", 
                     metric = metric_max, maximize = T, trControl = tC, 
                     tuneGrid = hyperparameters)
          models_trained[[length(models_trained) + 1]] <- (m)
          names_models <- c(names_models, "ROSE_RF")
        }
        if ("glm" %in% models) {
          m <- train(formula, data = rose.data, method = "glm", 
                     family = "binomial", maximize = T, metric = metric_max, 
                     trControl = tC)
          models_trained[[length(models_trained) + 1]] <- (m)
          names_models <- c(names_models, "ROSE_RLOG")
        }
      }
      if ("SMOTE" %in% IAAs) {
        smote.data <- DMwR::SMOTE(formula, data = as.data.frame(datasets[[i]]))
        if ("ranger" %in% models) {
          m <- train(formula, data = smote.data, num.trees = num.trees, 
                     importance = "impurity", method = "ranger", 
                     metric = metric_max, maximize = T, trControl = tC, 
                     tuneGrid = hyperparameters)
          models_trained[[length(models_trained) + 1]] <- (m)
          names_models <- c(names_models, "SMOTE_RF")
        }
        if ("glm" %in% models) {
          m <- train(formula, data = smote.data, method = "glm", 
                     family = "binomial", maximize = T, metric = metric_max, 
                     trControl = tC)
          models_trained[[length(models_trained) + 1]] <- (m)
          names_models <- c(names_models, "SMOTE_RLOG")
        }
      }
      if ("IPIP" %in% IAAs) {
        if ("ranger" %in% models) {
          m <- IPIP(formula = formula, dataset = datasets[[i]], 
                          val = datasets_val[[i]], model = "ranger",metric_max = metric_max,tC = tC,hiperparametros = hiperparametros,num.trees = num.trees)
          models_trained[[length(models_trained) + 1]] <- (m)
          names_models <- c(names_models, "IPIP_RF")
        }
        if ("glm" %in% models) {
          m <- IPIP(formula = formula, dataset = datasets[[i]], 
                          val = datasets_val[[i]], model = "glm",metric_max = metric_max,tC = tC,hiperparametros = hiperparametros,num.trees = num.trees)
          models_trained[[length(models_trained) + 1]] <- (m)
          names_models <- c(names_models, "IPIP_RLOG")
        }
      }
      if ("Upsample" %in% IAAs) {
        upsample.data <- caret::upSample(x = datasets[[i]][, 
                                                           -which(colnames(datasets[[i]]) == class)], 
                                         y = datasets[[i]][, class], yname = class)
        if ("ranger" %in% models) {
          m <- train(formula, data = upsample.data, num.trees = num.trees, 
                     importance = "impurity", method = "ranger", 
                     metric = metric_max, maximize = T, trControl = tC, 
                     tuneGrid = hyperparameters)
          models_trained[[length(models_trained) + 1]] <- (m)
          names_models <- c(names_models, "Upsample_RF")
        }
        if ("glm" %in% models) {
          m <- train(formula, data = upsample.data, method = "glm", 
                     family = "binomial", maximize = T, metric = metric_max, 
                     trControl = tC)
          models_trained[[length(models_trained) + 1]] <- (m)
          names_models <- c(names_models, "Upsample_RLOG")
        }
      }
      if ("Downsample" %in% IAAs) {
        downsample.data <- caret::downSample(x = datasets[[i]][, 
                                                               -which(colnames(datasets[[i]]) == class)], 
                                             y = datasets[[i]][, class], yname = class)
        if ("ranger" %in% models) {
          m <- train(formula, data = downsample.data, 
                     num.trees = num.trees, importance = "impurity", 
                     method = "ranger", metric = metric_max, maximize = T, 
                     trControl = tC, tuneGrid = hyperparameters)
          models_trained[[length(models_trained) + 1]] <- (m)
          names_models <- c(names_models, "Downsample_RF")
        }
        if ("glm" %in% models) {
          m <- train(formula, data = downsample.data, 
                     method = "glm", family = "binomial", maximize = T, 
                     metric = metric_max, trControl = tC)
          models_trained[[length(models_trained) + 1]] <- (m)
          names_models <- c(names_models, "Downsample_RLOG")
        }
      }
      if("Smoteboost" %in% IAAs){
        data_boost <- datasets[[i]]
        
        .wt.update <- function(probability, prediction, actual, wt, smooth = 1e-6){
          err <- sum(wt * (prediction != actual))
          
          alpha <- err / (1 - err)
          
          newWt <- wt * exp(ifelse(prediction != actual, 1, 0) * log(alpha))
          
          newWt <- newWt + smooth
          
          newWt <- newWt / sum(newWt)
          return(list(newWt, err))
        }
        
        
        smoteboost_train <- function(formula, data, size = 7,
                                     modelType = "ranger", rf.ntree = 50) {
          
          target <- gsub(" ", "", unlist(strsplit(format(formula), split = "~"))[1])
          data[, target] <- as.factor(data[, target])
          
          
          data$w <- rep(1 / nrow(data), nrow(data))
          
          
          label <- data[, target]
          
          min_label <- levels(data[, target])[which.min(table(data[, target]))]
          max_label <- setdiff(levels(data[, target]), min_label)
          
          list_model <- list()
          a <- 0
          for(iter_boost in seq_len(size)){
            
            smote_data <- DMwR::SMOTE(
              form = formula,
              data    = data
              # Si quieres setear perc.over, perc.under, k, etc., agrégalo aquí.
            )
            
            if(!("w" %in% colnames(smote_data))) {
              smote_data$w <- rep(1 / nrow(smote_data), nrow(smote_data))
            }
            smote_data$w[is.na(smote_data$w)] <- 1 / nrow(smote_data)
            
            smote_data$w <- smote_data$w / sum(smote_data$w)
            
            # Sampleamos según 'w' para entrenar el weak learner
            idx_samp <- sample(
              x       = seq_len(nrow(smote_data)),
              size    = nrow(smote_data),
              replace = TRUE,
              prob    = smote_data$w
            )
            train_samp <- smote_data[idx_samp, , drop = FALSE]
            
            if(modelType == "ranger"){
              
              m <- train(
                formula,
                data       = train_samp[, setdiff(colnames(train_samp), "w")],
                method     = "ranger",
                num.trees  = num.trees,
                importance = "impurity",
                metric     = metric_max,
                maximize   = TRUE,
                trControl  = tC,
                tuneGrid   = hyperparameters
              )
              
              list_model[[iter_boost]] <- m
              
              prob_mat <- predict(m, newdata = data, type = "prob")
              prob_min <- prob_mat[, min_label]
              
              pred_class <- ifelse(prob_min >= 0.5, min_label, max_label)
              
              updt <- .wt.update(prob_min, pred_class, data[, target], data$w)
              data$w <- updt[[1]]
              a[iter_boost] <- updt[[2]]
              
            } else if(modelType == "glm"){
              
              m <- train(
                formula,
                data     = train_samp[, setdiff(colnames(train_samp), "w")],
                method   = "glm",
                family   = "binomial",
                metric   = metric_max,
                maximize = TRUE,
                trControl= tC
              )
              
              list_model[[iter_boost]] <- m
              
              
              prob_mat <- predict(m, newdata = data, type = "prob")
              
              prob_min <- prob_mat[, min_label]
              
              pred_class <- ifelse(prob_min >= 0.5, min_label, max_label)
              
              updt <- .wt.update(prob_min, pred_class, data[, target], data$w)
              data$w <- updt[[1]]
              a[iter_boost] <- updt[[2]]
            }
            
          } 
          
          out <- list(
            weakLearners = list_model,
            errorEstimation = a,
            modelType    = modelType,
            min_label    = min_label,
            max_label    = max_label
          )
          class(out) <- "SmoteBoostModel"
          return(out)
        }
        
        
        
        if("ranger" %in% models){
          sb_rf <- smoteboost_train(
            formula   = formula,
            data      = data_boost,
            size      = 7,     # número de iteraciones
            modelType = "ranger",
            rf.ntree  = num.trees
          )
          models_trained[[length(models_trained) + 1]] <- sb_rf
          names_models <- c(names_models, "Smoteboost_RF")
        }
        
        if("glm" %in% models){
          sb_glm <- smoteboost_train(
            formula   = formula,
            data      = data_boost,
            size      = 7,
            modelType = "glm"
          )
          models_trained[[length(models_trained) + 1]] <- sb_glm
          names_models <- c(names_models, "Smoteboost_RLOG")
        }
        
      }
      if("Underbagging" %in% IAAs){
        
        # 1. Dataset para este fold
        data_ub <- datasets[[i]]
        
        # 2. Función auxiliar de undersampling
        #    El ratio "ir" marca cuántas instancias de la clase mayoritaria dejas 
        #    por cada 1 de la minoritaria.  (Es la idea de .ru en tu código original)
        .random_under <- function(formula, data, ir = 1) {
          target <- gsub(" ", "", unlist(strsplit(format(formula), split = "~"))[1])
          data[, target] <- as.factor(data[, target])
          
          tab_class <- table(data[, target])
          min_class <- names(which.min(tab_class))
          max_class <- names(which.max(tab_class))
          
          # Filas de la minoritaria
          min_idx <- which(data[, target] == min_class)
          # Filas de la mayoritaria
          max_idx <- which(data[, target] == max_class)
          
          # Queremos quedarnos con "ir * length(min_idx)" filas de la mayoritaria
          new_size_max <- round(ir * length(min_idx))
          
          # Si new_size_max > length(max_idx), no tiene sentido, ajustamos
          new_size_max <- min(new_size_max, length(max_idx))
          
          # Sample de la mayoritaria
          max_sample_idx <- sample(max_idx, new_size_max, replace = FALSE)
          
          out_data <- data[c(min_idx, max_sample_idx), ]
          return(out_data)
        }
        
        # 3. Definimos la función de underbagging (adaptación de tu `ub()`), 
        #    pero entrenando con caretaker::train
        underbagging_train <- function(formula, data, size = 7,
                                       modelType = "ranger",
                                       ir        = 1,
                                       rf.ntree  = 50) {
          
          target <- gsub(" ", "", unlist(strsplit(format(formula), split = "~"))[1])
          data[, target] <- as.factor(data[, target])
          
          # Aquí guardamos cada "bag"
          list_models <- vector("list", size)
          
          # Generamos 'size' muestras con undersampling y entrenamos un "weak learner" en cada una
          for(iter_bag in seq_len(size)){
            # Creamos el subconjunto undersampled
            train_i <- .random_under(formula, data, ir = ir)
            
            # Entrenamos un modelo caretaker
            if(modelType == "ranger"){
              m <- train(
                formula,
                data       = train_i,
                method     = "ranger",
                num.trees  = num.trees,
                importance = "impurity",
                metric     = metric_max,
                maximize   = TRUE,
                trControl  = tC,
                tuneGrid   = hyperparameters
              )
              list_models[[iter_bag]] <- m
              
            } else if(modelType == "glm"){
              m <- train(
                formula,
                data     = train_i,
                method   = "glm",
                family   = "binomial",
                metric   = metric_max,
                maximize = TRUE,
                trControl= tC
              )
              list_models[[iter_bag]] <- m
            }
          }
          
          # Devolvemos un objeto con clase custom
          out <- list(
            weakLearners = list_models,
            # weights = rep(1/size, size),  # si quisieras pesos => p.e. 1/size
            modelType   = modelType
          )
          class(out) <- "UnderBagModel"
          return(out)
        }
        
        # 4. Creamos los modelos underbagging para "ranger" y/o "glm"
        if("ranger" %in% models){
          ub_rf <- underbagging_train(
            formula   = formula,
            data      = data_ub,
            size      = 7,      # número de bags
            modelType = "ranger",
            rf.ntree  = num.trees,
            ir        = 1        # ratio de undersampling
          )
          models_trained[[length(models_trained)+1]] <- ub_rf
          names_models <- c(names_models, "Underbagging_RF")
        }
        if("glm" %in% models){
          ub_glm <- underbagging_train(
            formula   = formula,
            data      = data_ub,
            size      = 7,
            modelType = "glm",
            ir        = 1
          )
          models_trained[[length(models_trained)+1]] <- ub_glm
          names_models <- c(names_models, "Underbagging_RLOG")
        }
        
      }
    }
    for (v in 1:(length(models) * length(IAAs))) {
      
      new_metrics <- metrics_dfs(
        ml          = models_trained[as.double(v + ((length(models) * length(IAAs))) * (0:(cv - 1)))],
        formula     = formula,
        model_names = unique(names_models[as.double(v + ((length(models) * length(IAAs))) * (0:(cv - 1)))]),
        val         = datasets_test,
        metrics     = metrics
      )
      
      start_idx <- length(list_metrics_total) + 1
      
      list_metrics_total <- append(list_metrics_total, new_metrics)
      
      end_idx <- length(list_metrics_total)
      
      modelo_actual <- unique(
        names_models[ as.double(v + ((length(models) * length(IAAs))) * (0:(cv - 1))) ]
      )
      for (fold_k in seq_len(cv)) {
        idx <- start_idx + fold_k - 1
        fold_name <- paste0(modelo_actual, "_DF", df, "_Fold", fold_k)
        
        names(list_metrics_total)[idx] <- fold_name
      }
      
      sum <- list_metrics_total[[start_idx]]
      if (cv > 1) {
        for (l in (start_idx + 1):end_idx) {
          sum <- sum + list_metrics_total[[l]]
        }
        sum <- sum / cv
      }
      
      list_accumulative_metrics[[length(list_accumulative_metrics) + 1]] <- sum
      
      agg_name <- paste0(modelo_actual, "_DF", df)
      names(list_accumulative_metrics)[length(list_accumulative_metrics)] <- agg_name
    }
  }
  if (return.baic == F) {
    return(list(metrics = metrics, model_names = names_models, 
                metric_values = list_accumulative_metrics, props = props, 
                all_metrics = list_metrics_total))
  } else {
    agree_dis <- list()
    for (i in 1:(length(models) * length(IAAs))) {
      sequence <- c()
      start <- 1 + 5 * (i - 1)
      end <- 1 + 5 * (i - 1) + cv - 1
      skip <- length(models) * length(IAAs) * cv
      repeats <- df_aux + 1
      for (j in 1:repeats) {
        sequence <- c(sequence, seq(start, end, by = 1))
        start <- start + skip
        end <- end + skip
      }
      interv <- lapply(seq(1, length(sequence), by = 5), 
                       function(k) sequence[k:(k + 4)])
      agree_dis[[i]] <- lapply(interv, function(interv) list_metrics_total[interv])
    }
    BAIC_values <- c()
    BAIC_cor<-data.frame()
    for (i in 1:(length(models) * length(IAAs))) {
      BAIC_values <- c(BAIC_values, BAIC(metrics = metrics, 
                                           metric_values = list_accumulative_metrics[i + 
                                                                                       (length(models) * length(IAAs)) * (0:(df_aux))], 
                                           props = props)$BAIC_value)
      BAIC_cor <- rbind(BAIC_cor, t(as.data.frame(BAIC(metrics = metrics, 
                                                         metric_values = list_accumulative_metrics[i + 
                                                                                                     (length(models) * length(IAAs)) * (0:(df_aux))], 
                                                         props = props)$cor_pmin)))
    }
    df_results <- (data.frame(model_names = names_models[1:(length(models) * 
                                                              length(IAAs))], BAIC = BAIC_values))
    rownames(BAIC_cor)<-names_models[1:(length(models) * 
                                         length(IAAs))]
    colnames(BAIC_cor)<- metrics
    
    return(list(correlaciones =BAIC_cor, metrics = metrics, model_names = names_models, 
                metric_values = list_accumulative_metrics, props = props, 
                all_metrics = list_metrics_total,baic_results = df_results, best_model = models_trained[[which(BAIC_values == 
                                                                                                                max(BAIC_values))]], all_models = models_trained[1:(length(models) * 
                                                                                                                                                                     length(IAAs))]))
  }
}
