#' lallalalallala
#'
#' @importFrom plotly plot_ly
#' @importFrom magrittr %>%
#' @importFrom  graphics layout
#' @importFrom plotly add_markers
#'
#' @param MI_threshold_LB  which is the lower bound for the selected MI values
#' @param MI_threshold_UB which is the upper bound for the selected MI values
#' @param MI_threshold_step which is the step to produce MI values
#' @param cor_threshold_LB which is the lower bound for the selected cor values
#' @param cor_threshold_UB which is the upper bound for the selected cor values
#' @param cor_threshold_step which is the step to produce cor values
#' @param X_train features in training data (original X_train)
#' @param y_train class variable for training set
#' @param X_test features in test data (original X_test)
#'
#' @return a data frame containing selected MI_values and cor_values with their corresponding results for the number of remaining features.
#' @return a 3D plot showing the result in data frame
#' @export
MI_cor_plot = function(MI_threshold_LB,MI_threshold_UB,MI_threshold_step,cor_threshold_LB,cor_threshold_UB,cor_threshold_step,X_train,y_train,X_test){

  cor_threshold_values = seq(cor_threshold_LB, cor_threshold_UB, by=cor_threshold_step)
  MI_threshold_values = seq(MI_threshold_LB, MI_threshold_UB, by=MI_threshold_step)

  plot_input = data.frame(matrix(ncol = 3, nrow = (length(cor_threshold_values)*length(MI_threshold_values))))

  #provide column names
  colnames(plot_input) <- c('cor_threshold', 'MI_threshold','No. features after reduction')

  output = list()

  j = 1

  for (i in cor_threshold_values){

    for (f in MI_threshold_values){

      plot_input [j,1] = i
      plot_input [j,2] = f
      output= correlation_based_filtering(X_train, y_train, f, i, X_test)
      plot_input [j,3] = length(output$x3)
      j= j + 1
    }
  }


  fig <- plotly::plot_ly(plot_input, x = plot_input[,1], y = plot_input[,2], z = plot_input[,3], colors = c('#0C4B8E'))
  fig <- fig %>% plotly::add_markers()
  fig <- fig %>% graphics::layout(scene = list(xaxis = list(title = 'cor_threshold'),
                                               yaxis = list(title = 'MI_threshold'),
                                               zaxis = list(title = 'No. features after reduction')))

  print(fig)
  return (plot_input)

}
