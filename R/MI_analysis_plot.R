#' lallalalallala
#'
#'@importFrom mpmi mmi
#'@importFrom plotly plot_ly
#'
#' @param MI_threshold_LB  which is the lower bound for the selected MI values
#' @param MI_threshold_UB which is the upper bound for the selected MI values
#' @param MI_threshold_step which is the step to produce MI values
#' @param X_train features in training data (original X_train)
#' @param y_train class variable for training set
#' @param X_test features in test data (original X_test)
#' @param cor_threshold which is a threshold for the linear correlation between the features (X_train)
#'
#' @return a data frame containing selected MI_values with their corresponding results for the mean_MI and the number of remaining features.
#' @return a 3D plot showing the result in data frame
#' @export
MI_analysis_plot = function(MI_threshold_LB,MI_threshold_UB,MI_threshold_step,X_train,y_train,X_test, cor_threshold){

  MI_threshold_values = seq(MI_threshold_LB, MI_threshold_UB, by=MI_threshold_step)

  plot_input = data.frame(matrix(ncol = 3, nrow = ((MI_threshold_UB - MI_threshold_LB)/MI_threshold_step) + 1))

  #provide column names
  colnames(plot_input) <- c('MI_threshold', 'No. features after reduction', 'Mean_MI')

  output = list()

  j = 1

  for (i in MI_threshold_values){

    plot_input [j,1] = i
    output= correlation_based_filtering(X_train, y_train, i, cor_threshold, X_test)
    plot_input [j,2] = length(output$x3)
    mmi_output = mpmi::mmi(cts = output$x1, disc = y_train)
    plot_input [j,3] = mean(as.vector(mmi_output$mi))
    j= j + 1
  }

  fig <- plotly::plot_ly(plot_input, x = plot_input[,1], y = plot_input[,2], z = plot_input[,3], colors = c('#0C4B8E'))
  fig <- fig %>% add_markers()
  fig <- fig %>% layout(xaxis = list(title = 'MI_threshold'),
                        yaxis = list(title = 'No. features'),
                        zaxis = list(title = 'Mean_MI'))

  print (fig)
  return (plot_input)

}
