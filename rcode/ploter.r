###############################################################################
# Library to make plots for my results
###############################################################################

# This method makes a plot of performance results of all methods
# input: all_performance - rows -> computation time by number of features,
#   columns -> measures by methods
plot_performance <- function(all_performance)
{
  colors <- c('yellow', 'red', 'green', 'blue')
  plot(x=all_performance[ , 1], y=all_performance[, 2], type='l', col='black',
       xlab='Number of features', ylab='Computation time, seconds')
  legend(x=5, y=80, c('Fusion', 'Fisher', 'Relief', "ADC", "SVM"), lty=1,
         col=c('black', 'yellow', 'red', 'green', 'blue'))
  for (i in 3:6)
  {
    lines(x=all_performance[, 1], y=all_performance[, i], col=colors[i - 2])
  }
}