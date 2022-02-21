#' Weighted Box Plot of One Variable
#'
#' In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#' @param df data frame
#' @param x first variable of interest
#' @param weights survey weights that sums to sample size
#'
#' @return ggplot object
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
#' Weighted Box Plot of svy.design object
#'
#' In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#' @param surveyobj svy.design object
#' @param x variable to boxplot
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' ggboxweight_svy(dstrat, api00)
ggboxweight_svy = function(surveyobj, x){
  df = surveyobj$variables
  weights = weights(surveyobj)
  return(ggplot(df, aes({{x}}))+geom_boxplot(aes(weight = {{weights}})))
}



#' Weighted Boxplot with a categorical variable
#'
#' In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#' @param df data frame
#' @param x numeric variable of interest
#' @param y categorical variable of interest
#' @param weights survey weights that sums to sample size
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' ggboxweight2d(apistrat, api00, stype, pw)
ggboxweight2d = function(df, x, y, weights){
  return(ggplot(df, aes({{y}}, {{x}}))+geom_boxplot(aes(x = {{y}}, y = {{x}}, weight = {{weights}})))}

#' Weighted Boxplot of a survey object with a categorical variable
#'
#' In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#' @param surveyobj svy.design object
#' @param x variable to boxplot
#' @param y categorical variable
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' ggboxweight2d_svy(dstrat, api00, stype)
ggboxweight2d_svy = function(surveyobj, x, y){
  df = surveyobj$variables
  weights = weights(surveyobj)
  return(ggplot(df, aes({{y}}, {{x}}))+geom_boxplot(aes(x = {{y}}, y = {{x}}, weight = {{weights}})))
}


#' Weighted Boxplot with a categorical x axis and a faceting variable
#'
#' In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#' @param df data frame
#' @param x first categorical variable of interest
#' @param y numeric variable of interest
#' @param z second variable of interest for faceting
#' @param weights survey weights that sums to sample size
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' ggboxweight3d(apistrat, api00, stype,awards, pw)
ggboxweight3d = function(df, x, y, z, weights){
  return(ggplot(df, aes({{y}}, {{x}}))+geom_boxplot(aes(x = {{y}}, y = {{x}}, weight = {{weights}}))+facet_grid(rows =vars({{z}})))}

#' Weighted Boxplot of svy.design object with two categorical variables
#'
#' In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#' @param surveyobj svy.design
#' @param x variable to boxplot
#' @param y first categorical variable
#' @param z second categorical variable (for faceting)
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' ggboxweight3d_svy(dstrat, api00, stype, awards)
ggboxweight3d_svy = function(surveyobj, x,y,z){
  df = surveyobj$variables
  weights = weights(surveyobj)
  return(ggplot(df, aes({{y}}, {{x}}))+geom_boxplot(aes(x = {{y}}, y = {{x}}, weight = {{weights}}))+facet_grid(rows =vars({{z}})))

}
