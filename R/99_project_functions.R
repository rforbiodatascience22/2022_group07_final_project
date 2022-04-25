library(ggthemes)

# To check for and summarize NA values in a dataframe
na_count <- function(df){
  df %>% 
    summarise_all(~ sum(is.na(.)))
}

# Common theme to be used in all figures in the project
theme_project <- function(){
  theme_gdocs(base_size = 7)
}

# To rotate x-axis labels by 45 deg
rotate_x <- function(){
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))
}

# To rotate y-axis labels by 45 deg
rotate_y <- function(){
  theme(axis.text.y = element_text(angle = 45,
                                   hjust = 1))
}


#In order to be able to create the violin chart divided by sex we used the code taken from:
#https://stackoverflow.com/questions/35717353/split-violin-plot-with-ggplot2

GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}



datadistribution_plot <- function(x, y, df){
  ggplot(df, aes(x = .data[[x]], 
                 y = .data[[y]],
                 fill=Sex)) + 
    geom_split_violin() +
    theme_project() +
  geom_boxplot(aes(x = .data[[x]], 
                   y = .data[[y]]), 
               width=0.3)
}



