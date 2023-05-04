#' lallalalallala
#'
#' @importFrom stats cor
#' @importFrom mpmi mmi
#'
#' @param X_train features in training data (Original X_train)
#' @param y_train class variable for training set
#' @param MI_threshold which is a threshold for the mutual information score between the features (X_train) and the response variable (y_train)
#' @param cor_threshold which is a threshold for the linear correlation between the features (X_train)
#' @param X_test features in test data (Original X_test)
#'
#' @return a list of lists with three outputs which are the reduced_X_train, reduced_X_test and features_list.
#' @export
correlation_based_filtering = function(X_train, y_train, MI_threshold, cor_threshold, X_test){


  # Output:
  # 1) $x1 = reduced_X_train which is the reduced X_train
  # 2) $x2 = reduced_X_test which is the reduced X_test
  # 3) $x3 = features_list which is the list of selected features


  # Get the MI scores between the features (continuous) in X_train and y_train (discrete)
  mmi_output = mpmi::mmi(cts = X_train, disc = y_train)
  mmi_scores_output = mmi_output$mi
  MI_scores = as.vector(mmi_scores_output)


  # Get the indices of the features that they have MI_scores more than the MI_threshold
  True_index = which(MI_scores > MI_threshold)

  # Reduced the X_train by choosing the features in True_index
  reduced_X_train_one = X_train[, True_index]






  # Get the MI scores between the remained features (continuous) and the y_train (discrete)
  new_mmi_output = mpmi::mmi(cts = reduced_X_train_one, disc = y_train)
  new_mmi_scores_output = new_mmi_output$mi
  final_MI_scores = as.vector(new_mmi_scores_output)
  # Sort the final_MI_scores
  inds = order(final_MI_scores, decreasing = FALSE)
  # Order the reduced X_train based on ascending order of MI scores of features with y_train
  reduced_X_train_one = reduced_X_train_one[, inds]






  # Get the names of columns in reduced_X_train_one and make a list by them
  features_list = t(t(colnames(reduced_X_train_one)))
  features_list = as.vector(features_list)
  # Get the number of the features (columns names) in the list
  num_features = length(features_list)





  for ( e in 1:num_features){# e is the index of the features in feature list starting from first feature
    q = e + 1   # q is the index of the features which is next (after) to the feature with index e
    while (q <= num_features){# while loop to check correlation between the pairs
      if (abs(stats::cor(reduced_X_train_one[, e], reduced_X_train_one[, q])) > cor_threshold){
        q = q + 1 # update q
        num_features = num_features - 1 # decrease the num_features if the threshold is passed
        for (i in (q-2):num_features){
          features_list[i] = features_list[(i+1)] # Update the feature list
        }
        features_list = features_list[-(num_features + 1) ] # drop the last element in the feature list

      }else{
        q = q + 1 # Update q

      }

    }
    e = e + 1 # Update e
  }



  # Select the columns in the final feature list from the train data set
  reduced_X_train = reduced_X_train_one[, features_list]


  # Select the columns in the final feature list from the test data set
  reduced_X_test = X_test[, features_list]

  # Make a list by features_list and final train and test data set
  output_filtering_list = list(x1 = reduced_X_train, x2 = reduced_X_test, x3 = features_list)

  return(output_filtering_list)

}
