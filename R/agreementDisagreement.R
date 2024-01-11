agreementDisagreement <- function(all_metrics, metric_names, model_names) {
  
  metrics = metric_names
  agreementsA_list <- list()
  
  for(i in 1:length(all_metrics)){
    assign(paste0("metricsA",i),all_metrics[[i]])
    assign(paste0("agreementsA",i),matrix(0, nrow = length(metrics), ncol = length(metrics)))
    assign(paste0("dfA",i),data.frame())
    agreementsA_list[[i]] <- get(paste0("agreementsA", i))
  }
  
  disagreements <- matrix(0, nrow = length(metrics), ncol = length(metrics))

for (i in 1:length(get(paste0("metricsA1")))) {
  dfA_list <- list()
  
  for (j in 1:length(all_metrics)) {
    assign(paste0("dfA", j), as.data.frame(t(as.data.frame(get(paste0("metricsA", j))[[i]]))))
    dfA_list[[j]] <- get(paste0("dfA", j))
    
  }
  
    for(j in 1:(length(metrics)-1)){
      for(k in (j+1):(length(metrics))){
        for(r in 1:length(all_metrics)){
          maxJ<- -Inf
          maxK<- -Inf
          
          for(u in 1:length(all_metrics)){
          maxJ <- max(dfA_list[[u]][[j]][r], maxJ)
          maxK <- max(dfA_list[[u]][[k]][r], maxK)
          }
          t<-FALSE
          for(u in 1:length(all_metrics)){
            
            if(maxJ==dfA_list[[u]][[j]][r] && t==FALSE){
              agreementsA_list[[u]][j,j]= agreementsA_list[[u]][j,j]+1
              t=TRUE
            }
          }
          t<-FALSE
          
          for(u in 1:length(all_metrics)){
            
            if(maxK==dfA_list[[u]][[k]][r] && t==FALSE){
              agreementsA_list[[u]][k,k]= agreementsA_list[[u]][k,k]+1
              t=TRUE
            }
          }
          
          t<-FALSE
          for(u in 1:length(all_metrics)){
            
            if(maxJ==dfA_list[[u]][[j]][r] && maxK==dfA_list[[u]][[k]][r] && t==FALSE){
              agreementsA_list[[u]][j,k]= agreementsA_list[[u]][j,k]+1
              agreementsA_list[[u]][k,j]= agreementsA_list[[u]][k,j]+1
              t=TRUE
            }
          }
          if(t==FALSE){
            disagreements[j,k] <- disagreements[j,k] + 1
            disagreements[k,j] <- disagreements[k,j] + 1
          }
        }
      }
    }
}

  disagreements <- disagreements / (length(metricsA1) * length(all_metrics))
  
  for(u in 1:length(all_metrics)){
    agreementsA_list[[u]][row(agreementsA_list[[u]]) != col(agreementsA_list[[u]])] <- agreementsA_list[[u]][row(agreementsA_list[[u]]) != col(agreementsA_list[[u]])] /  (length(metricsA1) * length(all_metrics))
  diag(agreementsA_list[[u]]) <- diag(agreementsA_list[[u]]) / (length(metricsA1) * (length(metrics)-1) * length(all_metrics))
  }

  values = list()
  
  for (i in 1:(length(metrics))) {
    for (j in 1:(length(metrics))) {
      if(i <= j){#Por aqui
        vector<-c()
        for(u in 1:length(all_metrics)){
          vector<-c(vector,agreementsA_list[[u]][i,j])
        }
        s<-1-sum(vector)
        values[[length(values)+1]]<-c(vector,s)
        
      } else {
        values[[length(values)+1]] <- rep(0,length(all_metrics)+1)
      }
    }
  }
  
  dfValues <- data.frame(matrix(unlist(values), ncol = length(all_metrics)+1, byrow = TRUE))
  colnames(dfValues) <- c(model_names, "FILL")
  
  dfValues_long <- pivot_longer(dfValues, cols=1:length(all_metrics)+1, names_to="algorithm", values_to="values")
  
  graphs <- list()
  
  k <- length(metrics)+3
  
  colours <- c("#1F77B4", "#FFFFFF", "#2CA02C", "#D62728","#D6B127","#D70FFF")
  
  for(i in seq(1, nrow(dfValues_long), by=length(all_metrics)+1)) {
    graph <- ggplot(dfValues_long[i:(i+length(all_metrics)),], aes(x="", y=values, fill=algorithm)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) +
      scale_fill_manual(values = colours) +
      theme_void() +
      guides(fill=guide_legend(title=NULL))
    
    # Agregamos el gráfico a la lista
    graphs[[k]] <- graph
    if(k %% (length(metrics)+1) == 0){
      k <- k + 2
    } else {
      k <- k + 1
    }
  }
  
  for(i in c(1:(length(metrics)))){
    graphs[[i+1]] <- ggplot(data = data.frame(metric = metrics[[i]]), aes(x = 0, y = 0, label = metric)) +
      geom_text(size = 3, vjust = 0.5, hjust = 0.5) +
      theme_void()
    
    graphs[[(length(metrics)+2)+(i-1)*(length(metrics)+2)]] <-  ggplot(data = data.frame(metric = metrics[[i]]), aes(x = 0, y = 0, label = metric)) +
      geom_text(size = 3, vjust = 0.5, hjust = 0.5) +
      theme_void()
  }
  
  
  c_plot <- ggarrange(plotlist = graphs, ncol = (length(metrics) + 1), nrow = (length(metrics) + 1), common.legend = TRUE, legend = 'none')
  
  # Mostrar el gráfico combinado con título
  plot(annotate_figure(c_plot, top = text_grob(paste0("AgreementDisagreement plot"), 
                                             color = "black", face = "bold", size = 8)))
}
