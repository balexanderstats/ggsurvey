#' Weighted Histogram
#'
#' @param df data frame
#' @param x variable of interest
#' @param weights survey weights that sum to sample size
#' @param binwidth desired binwidth, if NULL bins in geom_histogram defaults to 30
#'
#' @return
#' @export
#'
#' @examples
gghistweight = function(df, x, weights, binwidth = NULL){
  if(is.null(binwidth)){
    plotnew = ggplot(df, aes({{x}}))+geom_histogram(aes(weight = {{weights}}))
  }
  else{
    plotnew = ggplot(df, aes({{x}}))+geom_histogram(aes(weight = {{weights}}), binwidth = binwidth)
  }
  return(plotnew)
}
#' Weighted Histogram with One Facet
#'
#' @param df data frame
#' @param x first variable of interest
#' @param y categorical variable for faceting
#' @param weights survey weights that sum to sample size
#' @param binwidth desired binwidth, if NULL bins in geom_histogram defaults to 30
#'
#' @return
#' @export
#'
#' @examples
gghistweight2d = function(df, x, y, weights, binwidth = NULL){
  if(is.null(binwidth)){
    plotnew = ggplot(df, aes({{x}}))+geom_histogram(aes(weight = {{weights}}))+facet_grid(rows = vars({{y}}))
  }
  else{
    plotnew = ggplot(df, aes({{x}}))+geom_histogram(aes(weight = {{weights}}), binwidth = binwidth) +facet_grid(rows = vars({{y}}))
  }
  return(plotnew)
}
#' Weighted Histogram with Two Facets
#'
#' @param df data frame
#' @param x first variable of interest
#' @param y first categorical variable for faceting
#' @param z second categorical variable for faceting
#' @param weights survey weights that sum to sample size
#' @param binwidth desired binwidth, if NULL bins in geom_histogram defaults to 30
#'
#' @return
#' @export
#'
#' @examples
gghistweight3d = function(df, x, y, z, weights, binwidth = NULL){
  if(is.null(binwidth)){
    plotnew = ggplot(df, aes({{x}}))+geom_histogram(aes(weight = {{weights}}))+facet_grid(rows = vars({{y}}), cols = vars({{z}}))
  }
  else{
    plotnew = ggplot(df, aes({{x}}))+geom_histogram(aes(weight = {{weights}}), binwidth = binwidth) +facet_grid(rows = vars({{y}}), cols = vars({{z}}))
  }
  return(plotnew)
}
