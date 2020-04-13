################################################################################
# Plot Missing Values
################################################################################


plotNAs <- function(df) {

  # Missing values code borrowed from: https://jenslaufer.com/data/analysis/visualize_missing_values_with_ggplot.html
  
  missing.values <- df %>% gather(key = "key", value = "val") %>%
    mutate(isna = is.na(val)) %>%   group_by(key) %>%
    mutate(total = n()) %>%   group_by(key, total, isna) %>%
    summarise(num.isna = n()) %>%   mutate(pct = num.isna / total * 100)
  
  
  levels <- (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key
  
  percentage.plot <- missing.values %>% ggplot() + geom_bar(aes(x = reorder(key, desc(pct)), y = pct, fill=isna), 
                                                            stat = 'identity', alpha=0.8) + scale_x_discrete(limits = levels) + 
    scale_fill_manual(name = "", values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) + 
    coord_flip() + labs(title = "Percentage of missing values", x = 'Columns with missing data', y = "Percentage of missing values")
  
  return(percentage.plot)
}

################################################################################
# Correlation Plot Code
  # Corrplot function from: http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
  # Vignette for corrplot is also useful
################################################################################

buildCorrPlot <- function(df){

  data.cor <- cor(df)
  
  # mat : is a matrix of data
  # ... : further arguments to pass to the native R cor.test function
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(df)
  
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  
  # Build corrPlot.png ordered by Angular order of Eigenvectors
  png(height=1100, width=1200, pointsize=15, file="./figures/corrPlot.png")
  corrplot(data.cor, 
           method="circle", 
           order="AOE",
           tl.col="black", 
           type="full", 
           tl.cex = 1, 
           p.mat = p.mat, 
           sig.level = 0.01, 
           insig = "blank")
  dev.off()
}

################################################################################
# Returns a pretty plot with percentages
  # Code from: https://sebastiansauer.github.io/percentage_plot_ggplot2_V2/
################################################################################

percentagePlot <- function(df, x, xlabel) {
  df <- data.frame(df)
  return(ggplot(df, aes(x=x,  group=y)) + 
           geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
           geom_text(aes( label = scales::percent(..prop..),
                          y= ..prop.. ), stat= "count", vjust = -.5) +
           labs(y = "Percent", x=xlabel, fill=xlabel) +
           facet_grid(~y) +
           scale_y_continuous(labels = scales::percent) +
           theme(legend.position="none")
  )
}


######################################################################################################
# Returns a plot for prediction type distribution
# Code from: http://rstudio-pubs-static.s3.amazonaws.com/210269_7873bb711f7b488a92f1195ef1c0e9af.html
######################################################################################################
plot_pred_type_distribution <- function(df, threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$pred >= threshold & df$y == 1, "TP", v)
  v <- ifelse(df$pred >= threshold & df$y == 0, "FP", v)
  v <- ifelse(df$pred < threshold & df$y == 1, "FN", v)
  v <- ifelse(df$pred < threshold & df$y == 0, "TN", v)
  
  df$pred_type <- v
  
  return (ggplot(data=df, aes(x=y, y=pred)) + 
    geom_violin(fill='black', color=NA) + 
    geom_jitter(aes(color=pred_type), alpha=0.3, color='darkgrey') +
    geom_hline(yintercept=threshold, color="red", alpha=0.6) +
    scale_color_discrete(name = "type") +
    labs(title=sprintf("Threshold at %.2f", threshold))
  )
}

#######################################################################################################
# Returns performance metrics for binary class
# Code from: https://rstudio-pubs-static.s3.amazonaws.com/446413_6ac206ffa826466bb3a33be2f338c61f.html
#######################################################################################################
binclass_eval = function (actual, predict) {
  cm = table(as.integer(actual), as.integer(predict), dnn=c('Actual','Predicted'))
  ac = (cm['1','1']+cm['0','0'])/(cm['0','1'] + cm['1','0'] + cm['1','1'] + cm['0','0'])
  pr = cm['1','1']/(cm['0','1'] + cm['1','1'])
  rc = cm['1','1']/(cm['1','0'] + cm['1','1'])
  fs = 2* pr*rc/(pr+rc)
  return (list(cm=cm, recall=rc, precision=pr, fscore=fs, accuracy=ac))
}
