# This script contain the compare function for the package Wind


# The function compare() will compare the speed difference between different sorting algorithms.

## Sorting algorithms

bubble_sort <- function(arr) {
  n <- length(arr)
  for (i in 1:(n - 1)) {
    for (j in 1:(n - i)) {
      if (arr[j] > arr[j + 1]) {
        # Swap elements arr[j] and arr[j + 1]
        temp <- arr[j]
        arr[j] <- arr[j + 1]
        arr[j + 1] <- temp
      }
    }
  }
  return(arr)
}



quick_sort <- function(arr) {
  if (length(arr) <= 1) {
    return(arr)
  }
  pivot <- arr[length(arr) %/% 2]  # Choose a pivot element (middle of the array)
  left <- arr[arr < pivot] # Elements smaller than the pivot
  middle <- arr[arr == pivot] # Elements equal to the pivot
  right <- arr[arr > pivot] # Elements greater than the pivot

  return(c(quick_sort(left), middle, quick_sort(right)))
}


#' Title:  Function to run algorithm multiple times and check the time it takes
#' Argument algorithm: the package provide the function for a bubble sort (bubble_sort)
#' and a quick sort (quick_sort). Thus, those can be used as inputs. If one wish to
#' compare speed of other functions, write those and use them as input.
#'
#' @param algorithm The sorting algortihms you wish to compare. Present them in a list. As a default it will be list(bubble_sort, quick_sort)
#' @param arr The argument that needs to be sorted. It can take both numeric values and characters
#' @param number The number of time the sorting is compared
#'
#' @return Time it takes to sort the argument
#' @export
#'
#' @examples
#' Function call
#' check_algorithm(algorithm = list(quick_sort, bubble_sort), arr = sample(1:1000), number = 10)
#' check_algorithm(arr = sample(1:1000), number = 10)
#' check_algorithm(arr = LETTERS, number = 10) # it can also take letters
#'  example data
#' df <- c("e","t","p","y")
#' check_algorithm(arr = df, number = 10)

check_algorithm <- function(algorithm = list(bubble_sort, quick_sort), arr, number) {
  time <- list()

  for (a in 1:length(algorithm)) { # here we go through each algorithm in the list
    tmp_result <- c() # empty vector to fill in time info to

    for (b in 1:number) { # here we go through each element in the sample
      tmp_time <- Sys.time() # time upon starting
      algorithm[[a]](arr) # The algorithm is called to sort the argument = arr
      tmp_result <- c(tmp_result, Sys.time() - tmp_time) # the time difference from start to finish, thus the full time it takes to sort
    }

    time[[a]] <- tmp_result # assigns it to an object that exist outside the loop
  }
  # the function output
  name <- deparse(substitute(algorithm)) # for providing the name of the algorithms that are compared
  print(paste("Algorithm:", name))
  return(time)
}


