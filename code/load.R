if(!suppressWarnings(require("pacman", character.only = TRUE))) {
  install.packages("pacman", repos = "https://cran.r-project.org/")
}

if (!require("devtools")) install.packages("devtools")
devtools::install_github("talgalili/d3heatmap", quiet = T)

pkg_list <- c("tidyverse", "splitstackshape",
              "mltools", "googlesheets4", "pheatmap", "dendextend",
              "kableExtra", "DT", "patchwork", "gtsummary",
              "cluster", "Rtsne",
              "FactoMineR", "factoextra", "missMDA", "plotly")
pacman::p_load(pkg_list, character.only = TRUE)

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
