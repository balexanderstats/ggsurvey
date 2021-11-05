#' Weighted Hex Plot
#'
#' @param df data frame
#' @param x name of variable for x axis
#' @param y name of variable for y axis
#' @param weights desired binwidth, if NULL bins in geom_histogram defaults to 30
#'
#' @return
#' @export
#'
#' @examples
gghexweight = function(df, x, y, weights){
  return(ggplot(df, aes(x= {{x}}, y= {{y}}))+geom_hex(aes(weight = {{weights}})))
}

#' Weighted Hex Plot with One Facet Variable
#'
#' @param df data frame
#' @param x name of variable for x axis
#' @param y name of variable for y axis
#' @param z faceting variable
#' @param weights desired binwidth, if NULL bins in geom_histogram defaults to 30
#'
#' @return
#' @export
#'
#' @examples
gghexweight2d = function(df, x, y, z, weights){
  return(ggplot(df, aes(x= {{x}}, y= {{y}}))+geom_hex(aes(weight = {{weights}}))+facet_grid(rows = vars({{z}})))
}
#' Weighted Box Plot with Two Facet Variables
#'
#' @param df data frame
#' @param x name of variable for x axis
#' @param y name of variable for y axis
#' @param a first faceting variable
#' @param b second faceting variable
#' @param weights desired binwidth, if NULL bins in geom_histogram defaults to 30
#'
#' @return
#' @export
#'
#' @examples
gghexweight3d = function(df, x, y, a, b, weights){
  return(ggplot(df, aes(x= {{x}}, y= {{y}}))+geom_hex(aes(weight = {{weights}}))+facet_grid(rows = vars({{a}}), cols = vars({{b}})))
}
