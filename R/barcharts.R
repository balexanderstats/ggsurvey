#' Weighted Bar Charts
#'
#' @param df survey dataframe
#' @param x  name of question of interest
#' @param weights survey weights that sums to sample size
#'
#' @return ggplot
#' @export
#'
#' @examples
ggfreqweight = function(df, x, weights){
  return(ggplot(df, aes({{x}}))+geom_bar(aes(weight = {{weights}}, y = (..count..)/sum(..count..))))}
ggfreqweight(crisp, Q21_1, weight1_PID)

#' Crosstabs of Two Variables
#'
#' @param df survey data frame
#' @param x first variable of interest
#' @param y second variable of interest
#' @param weights survey weights that sum to sample size
#'
#' @return
#' @export
#'
#' @examples
ggcrosstabs = function(df, x, y, weights){
  newdf = df %>% group_by({{x}},{{y}})  %>% tally(, wt = {{weights}}) %>% mutate(f = n/sum(n))
  plotnew = ggplot(newdf, aes({{y}}))+geom_bar(aes(weight = f))+facet_grid(cols = vars({{x}}))
  return(plotnew)
}

#' Crosstabs of Three Variable
#'
#' @param df survey data frame
#' @param x first variable of interest
#' @param y second variable of interest
#' @param z third variable of interest
#' @param weights survey weights that sum to sample size
#'
#' @return
#' @export
#'
#' @examples
ggcrosstabs3d = function(df, x, y, z, weights){
  newdf = df %>% group_by({{x}},{{y}}, {{z}})  %>% tally(, wt = {{weights}}) %>% mutate(f = n/sum(n))
  print(newdf)
  plotnew = ggplot(newdf, aes({{z}}))+geom_bar(aes(weight = f))+facet_grid(rows = vars({{x}}), cols = vars({{y}}))
  return(plotnew)

}
