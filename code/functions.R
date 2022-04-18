plotSilhouette <- function(matrix, k=NULL) {
  silhouette <- c()
  silhouette = c(silhouette, NA)
  if (is.null(k)) k <- floor(nrow(matrix)/3)
  for (i in 2:k) {
    pam_clusters <- pam(matrix, diss = TRUE, k = i)
    silhouette <- c(silhouette, pam_clusters$silinfo$avg.width)
  }
  plot(1:k, silhouette,
       xlab = "Clusters",
       ylab = "Silhouette Width")
  lines(1:k, silhouette)
}


plotDistTSNE <- function(dist, clusters, label, seed=1234) {
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
  
  ggplot(tsne_df, aes(X, Y, label = text, color = cluster)) +
    geom_point() +
    ggrepel::geom_label_repel(show.legend = F) +
    theme_classic() +
    labs(x = "tSNE_1", y = "tSNE_2")
}


plotQuanBoxplot <- function(data) {
  
  data <- data %>%
    gather("key", "value", -clusters) %>%
    mutate(key = factor(key, levels = unique(key)))
  ggplot(data, aes(key, value, fill = clusters, color = clusters)) + 
    geom_boxplot(outlier.shape = NA, alpha = 0.6, na.rm = T) +
    geom_jitter(position=position_jitterdodge(), 
                alpha = 0.6, na.rm = T) +
    labs(x = NULL, y = NULL) + 
    facet_wrap(~key, nrow = 4, scale = "free") +
    ggsci::scale_color_jco() +
    ggsci::scale_fill_jco() +
    theme(
      axis.text.x = element_blank(),
      strip.background = element_rect(
        color="black", fill="white", size=1.5, linetype="solid"
      ),
      strip.text = element_text(size = 12)
    )
} 


plotQualBar <- function(data) {
  
  # combine incentive
  # rm_ue_qual_dat3$incentive <- 
  #   case_when(rm_ue_qual_dat3$monetaryIncentive & rm_ue_qual_dat3$speakingIncentive ~ "Speaking, Monetary",
  #             rm_ue_qual_dat3$monetaryIncentive & !rm_ue_qual_dat3$speakingIncentive ~ "Monetary",
  #             !rm_ue_qual_dat3$monetaryIncentive & rm_ue_qual_dat3$speakingIncentive ~ "Speaking",
  #             TRUE ~ "Neither"
  #             )
  data <- data %>%
    # select(-c(monetaryIncentive, speakingIncentive)) %>%
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

