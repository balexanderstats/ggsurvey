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
#' library(survey)
#' data(api)
#' ggboxweight(apistrat, api00, pw)
#' data(election)
#' ggboxweight(election_pps, Bush, p)
ggboxweight = function(df, x, weights){
  return(ggplot(df, aes({{x}}))+geom_boxplot(aes(weight = {{weights}})))}

#' Weighted Box Plot with a categorical variable
#'
#' @param df data frame
#' @param x numeric variable of interest
#' @param y categorical variable of interest
#' @param weights survey weights that sums to sample size
#'
#' @return
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' ggboxweight2d(apistrat, api00, stype, pw)
ggboxweight2d = function(df, x, y, weights){
  return(ggplot(df, aes({{y}}, {{x}}))+geom_boxplot(aes(x = {{y}}, y = {{x}}, weight = {{weights}})))}

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
#' library(survey)
#' data(api)
#' ggboxweight3d(apistrat, api00, stype,awards, pw)
#' ggboxweight3d(apistrat, api00, stype, awards, pw, binwidth = 10)
ggboxweight3d = function(df, x, y, z, weights){
  return(ggplot(df, aes({{y}}, {{x}}))+geom_boxplot(aes(x = {{y}}, y = {{x}}, weight = {{weights}}))+facet_grid(rows =vars({{z}})))}
