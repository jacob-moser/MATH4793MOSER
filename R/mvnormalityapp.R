#' mvnormalityapp
#'
#' @description basic shiny dashboard that generates some output about multivariate nromality checks
#'
#' @details  User has ability to change plot views with dynamic inputs
#' @return  Normal proportion test outcomes, violin plots, standard normal and chi squared QQ plots, a confidence ellipse
#'
#'
#' @export
#'
#' @examples
#' \dontrun{ mvnormalityapp()}
mvnormalityapp <- function(){
  shiny::runApp(system.file("mvnormalityapp", package="MATH4793MOSER"),launch.browser = TRUE)
}
