#' lallalalallala
#'
#' @importFrom dataPreparation build_scales
#' @importFrom dataPreparation fast_scale
#'
#' @param X_train  features in training data (in this project X_train is the reduced_X_train)
#' @param X_test features in test data (in this project X_test is the reduced_X_test)
#'
#' @return a list which contains the standardized version of X_train and X_test.
#' @export
scaling_train_test = function(X_train, X_test){



  # Output:
  # 1) $y1 = scaled_X_train (in this project scaled_X_train is the standardized reduced_X_train)
  # 2) $y2 = scaled_X_test (in this project scaled_X_train is the standardized reduced_X_train)





  # Prepare the scaling function which will be fitted n the train set
  scaling_fit = dataPreparation::build_scales(data_set = X_train, cols = "auto", verbose = TRUE)
  # Get the new X_train
  scaled_X_train = dataPreparation::fast_scale(data_set = X_train, scales = scaling_fit, verbose = TRUE)
  # Apply the scaling_fit function (which have been fitted to the train set) to the test set
  scaled_X_test = dataPreparation::fast_scale(data_set = X_test, scales = scaling_fit, verbose = TRUE)




  # Make a list by X_train and X_test
  output_standardization_list = list(y1 = scaled_X_train, y2 = scaled_X_test)


  return(output_standardization_list)
}
