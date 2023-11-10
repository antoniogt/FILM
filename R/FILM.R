#' FILM function for doing the entire methodology and get the best model accordin
#'
#' @param dataset: Dataset of the binary classification problem
#' @param formula: Style: 'target_variable ~ .'
#' @param df_aux: Number of auxiliar datasets with different minority class function
#' @param IAAs: Imbalance Aware Approaches such as 'SMOTE', 'IPIP', 'ROSE', 'Upsample' and 'Downsample'
#' @param models: ML techniques to be trained after using some IAA: 'glm' or 'ranger'.
#' @param hyperparameters: Hiperparameters for 'ranger' method.
#' @param metrics: Vector of metrics to get in order to calculate UIC metric. Possible values: 'ACCURACY' ,'SENS', 'SPEC', 'PPV', 'NPV', 'KAPPA' , 'BAL_ACC',  'F1', 'AUC', 'PR', 'MCC', 'GEOM'
#' @param cv: Number K of folds for a K-Folds Cross Validation.
#' @param prop_min: Minimum of proportion of the minority class for the resample auxiliar datasets.
#' @param prop_max: Maximum of proportion of the minority class for the resample auxiliar datasets.
#' @param tC: Train Control of caret train() function.
#' @param metric_max: Metric from 'metrics' to maximize when training ML techniques.
#' @param num.trees: Number of trees of the 'ranger' caret method.
#'
#' @return UIC values, best model, all models
#' @export
#'
#' @examples
FILM <- function(dataset, formula, df_aux=6, IAAs=c("IPIP","SMOTE","ROSE"),models=c("ranger","glm"), hyperparameters=NULL, metrics=c("ACCURACY","KAPPA","F1"), cv=5, prop_min =0.05, prop_max = 0.4, tC=trainControl( summaryFunction = FILM::metrics,  allowParallel = TRUE,  classProbs = TRUE),metric_max="KAPPA",num.trees=200){

  class<- gsub(" ", "", unlist(strsplit(format(formula), split = "~"))[1])
  dataset[ ,class] <- as.factor(dataset[ ,class])

  models_trained<- list()
  names_models<-c()

  min<- levels(dataset[,class])[which.min(dataset[,class])]
  max<- levels(dataset[,class])[which.max(dataset[,class])]

  p0<-prop.table(table(dataset[,class]))[min]
  props<-c()
  for(i in 1:(df_aux/2)){

    props<-c(props,p0+i*(prop_max-p0)/df_aux)

  }

  for(i in 1:(df_aux/2)){

    props<-c(props,p0-i*(p0-prop_min)/df_aux)

  }

  props<- as.double(props)

  subset_min<- dataset[which(dataset[,class] == min),]
  subset_max<- dataset[which(dataset[,class] == max),]

  subdatasets<-list()
  subdatasets<-c(subdatasets,dataset)

  for(i in 1:(df_aux/2)){

    n<- round(nrow(subset_min)/props[i])-nrow(subset_min)
    subset_max_down<-subset_max[sample(1:nrow(subset_max),size = n,replace = F),]

    subdatasets<- c(subdatasets,rbind(subset_max_down,subset_min))
}

  for(i in ((df_aux/2)+1):df_aux){

    n<- round(nrow(subset_max)/(1-props[i]))-nrow(subset_max)
    subset_min_down<-subset_min[sample(1:nrow(subset_min),size = n,replace = F),]

    subdatasets<-c(subdatasets,rbind(subset_max,subset_min_down))
}

  props<-c(p0,props)


folds.max_list<-list()
folds.min_list<-list()

list_metrics_total<-list()
list_accumulative_metrics<-list()

for(i in 1:length(subdatasets)){

  folds.max_list[[i]]<-createFolds(subdatasets[[i]][which(subdatasets[[i]][,class] == max),class],k = cv)
  folds.min_list[[i]]<-createFolds(subdatasets[[i]][which(subdatasets[[i]][,class] == min),class],k = cv)
}

for(df in 1:length(subdatasets)){

  datasets<-list()
  datasets_val<-list()

  for(folds in 1:cv){

    datasets<-c(datasets,rbind(subdatasets[[df]][which(subdatasets[[df]][,class] == max),][folds.max_list[[df]][,folds],],subdatasets[[df]][which(subdatasets[[df]][,class] == min),][folds.min_list[[df]][,folds],]))
    datasets_val<-c(datasets_val,rbind(subdatasets[[df]][which(subdatasets[[df]][,class] == max),][-folds.max_list[[df]][,folds],],subdatasets[[df]][which(subdatasets[[df]][,class] == min),][-folds.min_list[[df]][,folds],]))
  }

  for(i in 1:cv){

    if("ROSE" %in% IAAs){
      rose.data <- ROSE::ROSE(formula,data=datasets[[i]])$data

      if("ranger" %in% models){
        m<-train(formula,
                 data = rose.data,
                 num.trees = num.trees,
                 importance = "impurity",
                 method = "ranger",
                 metric = metric_max,
                 maximize = T,
                 trControl = tC,
                 tuneGrid = hyperparameters
        )
        models_trained<-c(models_trained,m)
        names_models<-c(names_models,"ROSE_RF")
      }

      if("glm" %in% models){
        m <- train(formula,
                   data = rose.data,
                   method = "glm",
                   family = "binomial",
                   maximize = T,
                   metric = metric_max,
                   trControl = tC
        )

        models_trained<-c(models_trained,m)
        names_models<-c(names_models,"ROSE_RLOG")
      }


    }

    if("SMOTE" %in% IAAs){
      smote.data<- DMwR::SMOTE(formula,data=as.data.frame(datasets[[i]]))

      if("ranger" %in% models){
        m<- train(formula,
          data = smote.data,
          num.trees = num.trees,
          importance = "impurity",
          method = "ranger",
          metric = metric_max,
          maximize = T,
          trControl = tC,
          tuneGrid = hyperparameters
        )
        models_trained<-c(models_trained,m)
        names_models<-c(names_models,"SMOTE_RF")
      }

      if("glm" %in% models){
        m <- train(formula,
                   data = smote.data,
                   method = "glm",
                   family = "binomial",
                   maximize = T,
                   metric = metric_max,
                   trControl = tC
        )

        models_trained<-c(models_trained,m)
        names_models<-c(names_models,"SMOTE_RLOG")
      }


    }

    if("IPIP" %in% models){
      if("ranger" %in% models){

        m<-FILM::IPIP(formula=formula,dataset=datasets[[i]],val=datasets_val[[i]],model="ranger")

        models_trained<-c(models_trained,m)
        names_models<-c(names_models,"IPIP_RF")

      }

      if("glm" %in% models){

        m<-FILM::IPIP(formula=formula,dataset=datasets[[i]],val=datasets_val[[i]],model="glm")

        models_trained<-c(models_trained,m)
        names_models<-c(names_models,"IPIP_RLOG")


      }


    }


    if("Upsample" %in% IAAs){
      upsample.data<- caret::upSample(x=datasets[[i]][,-which(colnames(datasets[[i]])==class)], y=datasets[[i]][,class], yname= class)

      if("ranger" %in% models){
        m<- train(formula,
                  data = upsample.data,
                  num.trees = num.trees,
                  importance = "impurity",
                  method = "ranger",
                  metric = metric_max,
                  maximize = T,
                  trControl = tC,
                  tuneGrid = hyperparameters
        )
        models_trained<-c(models_trained,m)
        names_models<-c(names_models,"Upsample_RF")
      }

      if("glm" %in% models){
        m <- train(formula,
                   data = upsample.data,
                   method = "glm",
                   family = "binomial",
                   maximize = T,
                   metric = metric_max,
                   trControl = tC
        )

        models_trained<-c(models_trained,m)
        names_models<-c(names_models,"Upsample_RLOG")
      }


    }

    if("Downsample" %in% IAAs){
      downsample.data<- caret::downSample(x=datasets[[i]][,-which(colnames(datasets[[i]])==class)], y=datasets[[i]][,class], yname= class)

      if("ranger" %in% models){
        m<- train(formula,
                  data = downsample.data,
                  num.trees = num.trees,
                  importance = "impurity",
                  method = "ranger",
                  metric = metric_max,
                  maximize = T,
                  trControl = tC,
                  tuneGrid = hyperparameters
        )
        models_trained<-c(models_trained,m)
        names_models<-c(names_models,"Downsample_RF")
      }

      if("glm" %in% models){
        m <- train(formula,
                   data = downsample.data,
                   method = "glm",
                   family = "binomial",
                   maximize = T,
                   metric = metric_max,
                   trControl = tC
        )

        models_trained<-c(models_trained,m)
        names_models<-c(names_models,"Downsample_RLOG")
      }


    }
}

  for(v in 1:(length(models)*length(IAAs))){

    list_metrics_total<-c(list_metrics_total,FILM::metrics_dfs(models=models_trained[v+((length(models)*length(IAAs)))*(0:(cv-1))], formula =formula,model_names=unique(names_models[v+((length(models)*length(IAAs)))*(0:(cv-1))]), val=datasets_val,metrics=metrics)) #Sirve para hacer plots Jose Adrian
    sum<-list_metrics_total[[(v+((length(models)*length(IAAs)))*(0:(cv-1)))*df]][[1]]
    for(l in 1:(length(list_metrics_total[[(v+((length(models)*length(IAAs)))*(0:(cv-1)))*df]])-1)){
      sum<-sum+list_metrics_total[[(v+((length(models)*length(IAAs)))*(0:(cv-1)))*df]][[l]]
    }
    sum<-sum/length(list_metrics_total[[(v+((length(models)*length(IAAs)))*(0:(cv-1)))*df]])
    list_accumulative_metrics<-c(list_accumulative_metrics,sum) # For UIC
    }

}
  UIC_values<-c()

for(i in 1:(length(models)*length(IAAs))){
  UIC_values<-c(UIC_values,FILM::UIC(metrics=metrics,metric_values=as.double(unlist(list_accumulative_metrics[i+(length(models)*length(IAAs))*(0:(df_aux-1))])[which(names(unlist(list_accumulative_metrics[i+(length(models)*length(IAAs))*(0:(df_aux-1))])) %in% j)]),props=props))
  }

  df_results<-(data.frame(model_names=names_models[1:(length(models)*length(IAAs))],UIC=UIC_values))

  return(uic_results=df_results,best_model=models_trained[[which(UIC_values==max(UIC_values))]],all_models=models_trained[1:(length(models)*length(IAAs))])

}
