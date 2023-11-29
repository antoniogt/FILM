#' Title
#'
#' @param formula
#' @param dataset
#' @param val
#' @param pmin
#' @param bs
#' @param bE
#' @param model
#' @param mt
#' @param tC
#' @param hiperparametros
#' @param metric_max
#' @param num.trees
#'
#' @return
#' @export
#'
#' @examples
IPIP<- function(formula, dataset, val=NULL, pmin=0.45, bs=ceiling(log(.01)/(log(1-1/nrow(dataset[which(dataset[,class] == levels(dataset[,class])[which.min(dataset[,class])]),]))*round(nrow(dataset[which(dataset[,class] == levels(dataset[,class])[which.min(dataset[,class])]),])*0.75))), bE= ceiling(log(.01)/(log(1-1/round(nrow(dataset[which(dataset[,class] == levels(dataset[,class])[which.min(dataset[,class])]),])*0.75))*round(nrow(dataset[which(dataset[,class] == levels(dataset[,class])[which.min(dataset[,class])]),])*0.75))), model="glm", mt= function(n) { ceiling((bE-n) / 3) },tC=trainControl(summaryFunction = FILM::metrics,classProbs = TRUE), hiperparametros=NULL,metric_max="KAPPA",num.trees=200){

    class <- gsub(" ", "", unlist(strsplit(format(formula), split = "~"))[1])
    dataset[ ,class] <- as.factor(dataset[ ,class])

    if(is.null(val)){
      train_index<-createDataPartition(dataset[,class], p = .8, list = FALSE)
      train<-dataset[train_index,]
      test<-dataset[-train_index,]
    } else {
      train= dataset
      test<-val
      test[,class]<-as.factor(test[,class])
    }

    prop.maj<-1-pmin
    max_set <- train %>% filter(train[[class]] == levels(train[,class])[which.max(table(train[,class]))])
    min_set <-  train %>% filter(train[[class]] == levels(train[,class])[which.min(table(train[,class]))])

    dfs <- list()

    for(k in 1:bs){
      id.min_set <- sample(x = 1:nrow(min_set), size = round(nrow(train %>% filter(class == levels(train[,class])[which.min(train[,class])]))*0.75), replace = TRUE)
      id.max_set <- sample(x= 1:nrow(max_set), size = round(round(nrow(train %>% filter(class == levels(train[,class])[which.min(train[,class])]))*0.75)*prop.maj/(1-prop.maj)))
      dfs[[k]] <- rbind(max_set[id.max_set,],min_set[id.min_set,])
    }

    E <- list()

    for(k in 1:bs){
      Ek <- list()
      i <- 0

      df <- dfs[[k]]

      while(length(Ek)<=bE && i<mt(length(Ek))){
        ind.train <- c(
          sample(which(df[,class] == levels(train[,class])[which.max(table(train[,class]))]), size = round(nrow(train[which(train[,class] == levels(train[,class])[which.min(train[,class])]),])*0.75), replace = TRUE),
          sample(which(df[,class] == levels(train[,class])[which.min(table(train[,class]))]), size = round(nrow(train[which(daatset[,class] == levels(train[,class])[which.min(train[,class])]),])*0.75), replace = TRUE)
        )

        if(model== "glm"){
        m <- train(formula,
                   data = train[ind.train,],
                   method = "glm",
                   family = "binomial",
                   maximize = T,
                   metric = metric_max,
                   trControl = tC
        )

        }else if(model=="ranger"){
          m <- train(formula,
                     train[ind.train,],
            num.trees = num.trees,
            importance = "impurity",
            method = "ranger",
            metric = metric_max,
            maximize = T,
            trControl = tC,
            tuneGrid = hiperparametros
          )
        }

        # Evaluation
        metricas.ensemble <-
          if (length(Ek)==0){
            u <- -Inf
            names(u) <- metric_max
            u
          } else FILM::metric_probs(data.frame(
            obs = test[,class],
            pred = as.factor(FILM::predict_film(Ek, test[-which(colnames(test)==class)],test[,class],type="class")),
            prob = FILM::predict_film(Ek, test[-which(colnames(test)==class)],test[,class],type="prob")
          ))

        Ek[[length(Ek)+1]] <- m

        metricas.ensemble.2 <- FILM::metric_probs(data.frame(
          obs = test[,class],
          pred= as.factor(FILM::predict_film(Ek, test[-which(colnames(test)==class)],test[,class],type="class")),
          prob= FILM::predict_film(Ek, test[-which(colnames(test)==class)],test[,class],type="prob")
        ))

        if(metricas.ensemble.2[metric_max] <= metricas.ensemble[metric_max]){
          i <- i+1
          Ek[[length(Ek)]] <- NULL
        } else{
          i <- 0
        }

      }

      E[[length(E)+1]] <- Ek

    }

    return(E)
}
