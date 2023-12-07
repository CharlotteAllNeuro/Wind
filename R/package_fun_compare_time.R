# This script contain the compare function for the package Wind


# The function compare() will compare the speed difference between different sorting algorithms.

## Sorting algorithms

bubble <- function(arr) {
  n <- length(arr)
  for (i in 1:(n - 1)) {
    for (j in 1:(n - i)) {
      if (arr[j] > arr[j + 1]) {
        temp <- arr[j]
        arr[j] <- arr[j + 1]
        arr[j + 1] <- temp
      }
    }
  }
  return(arr)
}



quick <- function(arr) {
  if (length(arr) <= 1) {
    return(arr)
  }
  pivot <- arr[length(arr) %/% 2]  # Choose a pivot element (middle of the array)
  left <- arr[arr < pivot] # Elements smaller than the pivot
  middle <- arr[arr == pivot] # Elements equal to the pivot
  right <- arr[arr > pivot] # Elements greater than the pivot

  return(c(quick(left), middle, quick(right)))
}


#' Title:  Function to run a list of algorithm multiple times and check the time it takes them to sort the same data
#'
#'
#' The *algorithm_speed()* compares the speed of different sorting algorithms.
#' The package *Wind* provide the function for a bubble sort (bubble)
#' and a quick sort (quick). Thus, those can be used as inputs. However, if one wish to
#' compare speed of other sorting algorithms, one can write those and use them as input
#' in *algorithm_speed()*.
#'
#
#' @param argument The argument that needs to be sorted. It can take both numeric values and characters
#' @param repetition The number of time the sorting is performed.
#' @param algorithm The sorting algorithms you wish to compare. Present them in a list. As a default it will be list(bubble_sort, quick_sort)
#'
#' @return The function will return 2 lists of times (or more if you made more sorting algorithms to compare),
#' where the times listed is the time it took to sort the  argument at each repetition. The function also print the list of names
#' of the algorithm input. Now you can compare the time it took to sort the chosen argument.
#' @export
#'
#' @examples
#' # Examples of different function call
#' algorithm_speed(algorithm = list(quick, bubble), argument = sample(1:1000), repetition = 10)
#' algorithm_speed(argument = sample(1:1000), repetition = 10)
#' algorithm_speed(argument = LETTERS, repetition = 10) # it can also take letters


algorithm_speed <- function(algorithm = list(bubble, quick), argument, repetition ) {
  time <- list()

  for (a in 1:length(algorithm)) { # here we go through each algorithm in the list
    tmp_result <- c() # empty vector to fill in time info to

    for (b in 1:repetition) { # here we go through each element in the sample
      tmp_time <- Sys.time() # time upon starting
      algorithm[[a]](argument) # The algorithm is called to sort the argument = arr
      tmp_result <- c(tmp_result, Sys.time() - tmp_time) # the time difference from start to finish, thus the full time it takes to sort
    }

    time[[a]] <- tmp_result # assigns it to an object that exist outside the loop
  }
  # the function output
  name <- deparse(substitute(algorithm)) # for providing the name of the algorithms that are compared
  print(paste("Algorithm:", name))
  return(time)
}

