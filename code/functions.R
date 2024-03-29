plotSilhouette <- function(matrix, n=NULL, k=NULL) {
  silhouette <- c()
  silhouette = c(silhouette, NA)
  if (is.null(n)) n <- floor(nrow(matrix)/3)
  for (i in 2:n) {
    pam_clusters <- pam(matrix, diss = TRUE, k = i)
    silhouette <- c(silhouette, pam_clusters$silinfo$avg.width)
  }
  plot(1:n, silhouette,
       xlab = "Clusters",
       ylab = "Silhouette Width", cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.2)
  lines(1:n, silhouette)
  if (!is.null(k)) abline(v = k, col = rgb(1, 0, 0, 0.5))
}


plotDistTSNE <- function(dist, clusters, color = NULL, label = NULL, color.legend.title = NULL, seed=1234) {
  set.seed(seed)
  ns <- nrow(as.matrix(dist))
  tsne_object <- Rtsne(dist, 
                       is_distance = TRUE, 
                       perplexity = floor((ns - 1) / 3))
  tsne_df <- tsne_object$Y %>%
    data.frame() %>%
    setNames(c("X", "Y")) %>%
    mutate(cluster = factor(clusters),
           text = label)
  
  if (is.null(color)) color <- tsne_df$cluster else color <- color
  p <- ggplot(tsne_df, aes(X, Y, label = text, color = color)) +
    geom_point() +
    theme_classic(base_size = 14) +
    # theme(legend.position = "top") + 
    labs(x = "tSNE_1", y = "tSNE_2", color = color.legend.title)
  
  if (!is.null(label)) {
    # p <- p + geom_label(show.legend = F) 
    p <- p + ggrepel::geom_label_repel(show.legend = F)
  }
  return(p)
}


plotQuanBoxplot <- function(data, test.method = NULL, pos.x = 0.4, pos.y = 0.95) {
  
  cols <- c(ggsci::pal_jco()(10), ggsci::pal_npg()(10))
  stopifnot(test.method %in% c("t.test", "wilcox.test", "anova", "kruskal.test"))
  data <- data %>%
    gather("key", "value", -clusters) %>%
    mutate(key = factor(key, levels = unique(key)))
  p <- ggplot(data, aes(clusters, value, fill = clusters, color = clusters)) + 
    geom_boxplot(outlier.shape = NA, alpha = 0.6, na.rm = T) +
    geom_jitter(position=position_jitterdodge(), 
                alpha = 0.6, na.rm = T) +
    labs(x = NULL, y = NULL) + 
    facet_wrap(~key, nrow = 3, scales = "free") +
    # ggsci::scale_color_jco(palette = cols) +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    # ggsci::scale_fill_jco(palette = cols) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_blank(),
      strip.background = element_rect(
        color="black", fill="white", size=1.5, linetype="solid"
      ),
      strip.text = element_text(size = 12)
    )
  
  if (!is.null(test.method)) {
    p <- p + ggpubr::stat_compare_means(method = test.method, label.x.npc = pos.x, label.y.npc = pos.y)
  }
  
  return(p)
} 


plotQualBar <- function(data) {
  
  data <- data %>%
    tibble::rownames_to_column("name") %>%
    replace_na("Missing") %>%
    separate_rows(dataTypeTiers) %>%
    separate_rows(sponsorTiers) %>%
    separate_rows(contributorTiers) %>%
    gather("key", "value", -c(name, clusters)) %>%
    distinct() %>%
    mutate(key = factor(key, levels = colnames(data)))
    p3 <- lapply(c("modelContainerization",
                   "monetaryIncentive",
                   "speakingIncentive",
                   "contributorTiers",
                   "sponsorTiers",
                   "dataTypeTiers"),
                   function(feat) {
       p.data <- filter(data, key == feat)
       
       if (feat %in% c("sponsorTiers", "contributorTiers")) {
         p.data$value <- factor(p.data$value, 
                                levels = c("Academic", "Government","Industrial","Foundation",
                                           "Other", "Missing"))
       }
       if(feat == "dataTypeTiers") {
         p.data$value <- factor(p.data$value, 
                                levels = c("clinical", "demographic","genomic","proteomic",
                                           "ehr", "metabolomic", "imaging", 
                                           "other", "Missing"))
       }
       out <- ggplot(p.data,
                     aes(clusters, fill = value)) + 
         geom_bar(stat = "count", position = position_dodge(preserve = "single")) +
         ggsci::scale_fill_jco() +
         labs(y = NULL) + 
         facet_wrap(~key, ncol = 4, scale="free") +
         theme_bw()
       
       # if (feat == "dataTypeTiers") {
       #   out <- out + scale_fill_brewer(palette = "Dark2")
       # }
       return(out)
     })
    
    return(p3)
  # ((p3[[4]] + p3[[5]] + p3[[6]]) + plot_layout(guides = "collect")) /
  #   ((p3[[1]] + p3[[2]] + p3[[3]]) + plot_layout(guides = "collect"))
}

### backup
norm_minmax <- function(x, na.rm = TRUE) {
  
  if (na.rm) {
    out_x <- x
    inx <- which(!is.na(x))
    x <- as.numeric(na.omit(x))
    norm_x <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    out_x[inx] <- norm_x
  } else {
    out_x <- (x - min(x)) / (max(x) - min(x))
  }
  
  return(out_x)
}

transform_log2 <- function(x, na.rm = TRUE) {
  
  if (na.rm) {
    out_x <- x
    inx <- which(!is.na(x))
    x <- as.numeric(na.omit(x))
    log2_x <- log2(x + 0.1)
    out_x[inx] <- log2_x
  } else {
    out_x <- log2(x + 0.1)
  }
  
  return(out_x)
}


showData <- function(data) {
  
  data %>% 
    kable %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
    column_spec(1, bold = T, border_right = T) %>%
    scroll_box(height = "500px")
}


pca_plotly <- function(pca, group_by, title=NULL, legend_title=NULL, includeFeat=FALSE) {
  
  if (is.null(title)) title <- "Individual- PCA"
  
  if (!includeFeat) {
    res <- fviz_pca_ind(pca,
                        habillage = group_by,
                        addEllipses = TRUE,
                        repel = TRUE,
                        geom = c("text","point"),
                        invisible="quali") + 
      labs(title = title)
  } else {
    res <- fviz_pca_biplot(pca,
                           habillage = group_by,
                           addEllipses = TRUE,
                           repel = TRUE,
                           geom = c("text","point"),
                           invisible="quali") + 
      labs(title = title)
  }
  suppressWarnings(
    pca.p <- ggplotly(res, tooltip = c("x","y","colour")) %>% plotly_build()
  )
  grps <- levels(res$data$Groups)
  
  if (length(grps) > 0) {
    
    # prevent ellipse's hover from overlapping with points' hover
    lapply(seq_along(grps) + length(grps), function(i) {
      pca.p$x$data[[i]]$hoverinfo <<- "skip"
    })
    
    lapply(seq_along(grps), function(i) {
      # add marker label
      pca.p$x$data[[i]]$text <<- with(res$data, name)
      # rename legend
      pca.p$x$data[[i]]$name <<- grps[i]
    })
    
    # rename legend title
    pca.p$x$layout$legend$title$text<- legend_title
    
  }
  pca.p
}
