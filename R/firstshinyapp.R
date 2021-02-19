#' @title firstshinyapp
#'
#'
#'
#' @description basic shiny dashboard that generates two plots and some descriptive text about user uploaded data
#'
#' @details  User has ability to change plot views with dynamic inputs
#' @return  2 plots
#'
#' @section First Plot:
#' The first plot allows the user to generate a scatterplot on two variables and further manipulate the size and coloring of the plot with additional input.
#'
#' Clicking the plot allows the user to calculate and display a drop-one sample correlation.
#'
#' @section Second Plot:
#' The second plot generates the same scatterplot as the first (on the same variables) and allows the user to visualize a rotation of the data by rotated axes.
#'
#' The rotated sample correlation coefficient is calculated and displayed as well as a zeroing value.
#'
#' One limitation is that the scaling of the plot can result in skewed axes rotations. Occurs if range of X and Y variables are significantly different.
#'
#' @export
#'
#' @examples
#' \dontrun{ firsthinyapp()}
firstshinyapp<-function(){
  shiny::runApp(system.file("rotationshinyapp", package="MATH4793MOSER"),launch.browser = TRUE)
}
