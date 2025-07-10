integrated_agree_disagree <- function(metrics,
                                      model          = c("RF"),
                                      legend_pos     = "bottom") {

  nms         <- names(metrics)
  algorithms     = unique(sapply(strsplit(nms, "_"), `[`, 1))
  fold_number <- sapply(strsplit(names(metrics), "_"), function(x) {
    as.integer(gsub("Fold", "", x[length(x)]))
  })
  folds_per_df<-max(fold_number)
  prefijos    <- sub("_.*", "", nms)        
  listas_por_alg <- split(nms, prefijos)
  
  ## Filtrar por el modelo actual (RF / RLOG)  ───────────────────────────────────
  keep_alg <- intersect(model, unique(unique(sub("^[^_]+_([^_]+)_.*", "\\1", nms))))
  if (length(keep_alg) == 0)
    stop("No metrics found for the selected model/s")
  
  listas_modelo <- lapply(listas_por_alg[keep_alg], \(vec)
                          vec[grepl(model, vec, fixed = TRUE)])
  
  
  dfs <- lapply(names(listas_modelo), function(alg){
    df_raw   <- metrics[names(metrics) %in% listas_modelo[[alg]]]
    df_tag   <- sub(paste0(alg,"_",model,"_(DF[0-9]+)_Fold[0-9]+"), "\\1",
                    names(df_raw))
    split(df_raw, df_tag)
  })
  names(dfs) <- names(listas_modelo)             
  metric_names<-names(dfs[[1]][[1]][[1]])
  
  p            <- length(metric_names)
  n_alg        <- length(dfs)
  
  
  agreements <- replicate(n_alg, matrix(0, p, p), simplify = FALSE)
  names(agreements) <- names(dfs)
  disagreements     <- matrix(0, p, p)
  
  
  for (i_df in seq_along(dfs[[1]])) {        
    
    dfs_df <- lapply(dfs, \(l) {
      as.data.frame(t(as.data.frame(l[[i_df]])))[, metric_names]
    })
    
    for (j in 1:(p-1)) for (k in (j+1):p) for (r in seq_len(folds_per_df)) {

      max_j <- vapply(dfs_df, \(d) d[r, j], numeric(1))
      max_k <- vapply(dfs_df, \(d) d[r, k], numeric(1))
      idx_j <- which.max(max_j)
      idx_k <- which.max(max_k)
      
      agreements[[idx_j]][j, j] <- agreements[[idx_j]][j, j] + 1
      agreements[[idx_k]][k, k] <- agreements[[idx_k]][k, k] + 1
      
      if (idx_j == idx_k) {            
        agreements[[idx_j]][j, k] <- agreements[[idx_j]][j, k] + 1
        agreements[[idx_j]][k, j] <- agreements[[idx_j]][k, j] + 1
      } else {                          
        disagreements[j, k] <- disagreements[j, k] + 1
        disagreements[k, j] <- disagreements[k, j] + 1
      }
    }
  }
  
  
  n_df <- length(dfs[[1]])        
  denom_offdiag <- n_df * folds_per_df
  denom_diag    <- n_df * (p - 1) * folds_per_df
  
  disagreements <- disagreements / denom_offdiag
  agreements    <- lapply(agreements, \(M){
    off <- row(M) != col(M)
    M[off]  <- M[off]  / denom_offdiag
    diag(M) <- diag(M) / denom_diag
    M
  })
  
  
  values <- vector("list", p * p)
  idx    <- 1
  for (i in seq_len(p)) for (j in seq_len(p)) {
    if (i <= j) {
      acs <- vapply(agreements, \(M) M[i, j], numeric(1))
      values[[idx]] <- c(acs, 1 - sum(acs))
    } else  values[[idx]] <- rep(0, n_alg + 1)
    idx <- idx + 1
  }
  df_val <- as.data.frame(do.call(rbind, values))
  colnames(df_val) <- c(names(agreements), "No Match")
  df_long <- tidyr::pivot_longer(df_val, cols = everything(),
                                 names_to = "algorithm",
                                 values_to = "values")
  df_long$algorithm <- factor(df_long$algorithm,
                              levels = c("No Match", names(agreements)))
  
  library(ggplot2); library(ggpubr)
  n_cols <- p + 1
  n_rows <- p + 1
  colours <- c("#FFFFFF", RColorBrewer::brewer.pal(max(3, n_alg), "Set2"))
  
  while (length(colours) < n_alg + 1)
    colours <- c(colours, grDevices::rainbow(n_alg + 1 - length(colours)))
  
  graphs <- vector("list", n_cols * n_rows)
  
  for (i in seq_len(p)) {
    graphs[[i + 1]] <- ggplot(data.frame(metric = metric_names[i]),
                              aes(x = 0, y = 0, label = metric)) +
      geom_text(size = 4) + theme_void()
    graphs[[(n_cols + 1) + (i - 1) * n_cols]] <-
      ggplot(data.frame(metric = metric_names[i]),
             aes(x = 0, y = 0, label = metric)) +
      geom_text(size = 4) + theme_void()
  }
  
  k <- p + 3
  step <- n_cols + 1
  for (i in seq(1, nrow(df_long), by = n_alg + 1)) {
    graphs[[k]] <- ggplot(df_long[i:(i + n_alg),],
                          aes(x = "", y = values, fill = algorithm)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      scale_fill_manual(values = colours) +
      theme_void() +
      guides(fill = guide_legend(title = NULL))
    k <- k + ifelse((k %% n_cols) == 0, 2, 1)
  }
  
  c_plot <- ggarrange(plotlist   = graphs,
                      ncol       = n_cols,
                      nrow       = n_rows,
                      common.legend = TRUE,
                      legend     = legend_pos) +
    theme(plot.background  = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"))
  titulo <- if (model == "RF") "Random Forest models"
  else               "Logistic Regression models"
  plot(annotate_figure(c_plot,
                       top = text_grob(titulo, face = "bold", size = 9)))
}
