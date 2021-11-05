#' Weighted Box Plot of One Variable
#'
#' @param df data frame
#' @param x first variable of interest
#' @param weights survey weights that sums to sample size
#'
#' @return
#' @export
#'
#' @examples
ggboxweight = function(df, x, weights){
  return(ggplot(df, aes({{x}}))+geom_boxplot(aes(weight = {{weights}})))}

#' Weighted Box Plot with a categorical variable
#'
#' @param df data frame
#' @param x categorical variable of interest
#' @param y numeric variable of interest
#' @param weights survey weights that sums to sample size
#'
#' @return
#' @export
#'
#' @examples
ggboxweight2d = function(df, x, y, weights){
  return(ggplot(df, aes({{x}}, {{y}}))+geom_boxplot(aes(x = {{x}}, y = {{y}}, weight = {{weights}})))}

#' Weighted Box Plot with a categorical x axis and a faceting variable
#'
#' @param df data frame
#' @param x first categorical variable of interest
#' @param y numeric variable of interest
#' @param z second variable of interest for faceting
#' @param weights survey weights that sums to sample size
#'
#' @return
#' @export
#'
#' @examples
ggboxweight3d = function(df, x, y, z, weights){
  return(ggplot(df, aes({{x}}, {{y}}))+geom_boxplot(aes(x = {{x}}, y = {{y}}, weight = {{weights}}))+facet_grid(rows =vars({{z}})))}
