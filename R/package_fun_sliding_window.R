# This script contain the sliding window function for the package Wind


# Run a sliding window through sequential data

## creating 2 window functions

## find max difference in a window

window_Max_diff <- function(data){
  Max_diff <- max(data) - min(data)
  return(Max_diff)
}

## find minimum value in a window

window_Min_val <- function(data){
  Min_val <- min(data)
  return(Min_val)
}

# Example code to try the function
# window_Max_diff(Test_data_frame[c(1:10), 2])
# window_Min_val(Test_data_frame[c(1:10), 2])

## Sliding_window()
#.. Argument: window_type takes one of 2 inputs: "Max_diff" or "MinVal". This
# ..will make the function run either window_Max_diff or window_Min_val, respectively

#' Title: Sliding window function
#'
#' This function will run through a sequential data input (*data*) with a sliding window.
#' The window size is chosen in the the function argument *window_size*. For each
#' window it will fine either the minimum value (*"Min_val"*) or the maximum
#' difference (*"Max_diff"*) in the window.
#'
#'
#' @param data The sequential input data
#' @param window_size The size of the window, eg. 10.
#' @param window_type The window type. window_type takes one of 2 inputs:
#' *"Max_diff"* or *"MinVal"*. This will make the function run either window_Max_diff
#' or window_Min_val, respectively
#'
#' @return The function will return you the orginal data with an extra column. The
#' extra column will be either Max_diff or Min_val depending on your window_type
#'
#' @export
#'
#' @examples
#'
#' # Test data
#' df <- tibble(x= runif(100), y=x*2)
#' # Window size 10 and a window_type = "Min_val"
#' sliding_window(df, 10, "Min_val")
#' # Window size 10 and a window_type = "Max_diff"
#' sliding_window(df, 10, "Max_diff")

sliding_window <- function(data, window_size, window_type) {

  data[ , c("Max_diff", "Min_val")] <- NA

    # iterate through the time series
    for (i in 1:(nrow(data[, 1]) - window_size - 1)) {

      if (window_type == "Max_diff") {
        # Adding max diff value to 3ed column
        data$Max_diff[i] <-  window_Max_diff(data[i:(i+window_size -1), 2])
      }

      if (window_type == "Min_val") {
        # Add the minimum value of the sliding window to "min" column
        data$Min_val[i] <-  window_Min_val(data[i:(i+window_size-1), 2])
      }

     }
  return(data) # needed to return data - to store the output outside the loops
}


