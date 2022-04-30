#' Weighted Hex Plot
#'
#' In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#' @param df data frame
#' @param x name of variable for x axis
#' @param y name of variable for y axis
#' @param weights name of weights variable
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' gghexweight(apistrat, api99, api00, pw)
gghexweight = function(df, x, y, weights){
  return(ggplot(df, aes(x= {{x}}, y= {{y}}))+geom_hex(aes(weight = {{weights}})))
}


#' Weighted Hex Plot of Survey Design Object
#'
#' In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#' @param surveyobj svy.design
#' @param x name of variable for x axis
#' @param y name of variable for y axis
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' gghexweight_svy(dstrat, api99, api00)
gghexweight_svy = function(surveyobj, x, y){
  df = surveyobj$variables
  wts = stats::weights(surveyobj)
  return(ggplot(df, aes(x= {{x}}, y= {{y}}))+geom_hex(aes(weight = {{wts}})))
}

#' Weighted Hex Plot with One Facet Variable
#'
#' In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#' @param df data frame
#' @param x name of variable for x axis
#' @param y name of variable for y axis
#' @param z faceting categorical variable
#' @param weights name of weights variable
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' gghexweight2d(apistrat, api99, api00, stype, pw)
gghexweight2d = function(df, x, y, z, weights){
  return(ggplot(df, aes(x= {{x}}, y= {{y}}))+geom_hex(aes(weight = {{weights}}))+facet_grid(rows = vars({{z}})))
}


#' Weighted Hex Plot of svy.design with One Facet Variable
#'
#' In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#' @param surveyobj svy.design
#' @param x variable for x axis
#' @param y variable for y axis
#' @param z faceting variable
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' gghexweight2d_svy(dstrat, api99, api00, stype)
gghexweight2d_svy = function(surveyobj, x,y, z){
  df = surveyobj$variables
  weights = stats::weights(surveyobj)
  return(ggplot(df, aes(x= {{x}}, y= {{y}}))+geom_hex(aes(weight = {{weights}}))+facet_grid(rows = vars({{z}})))
}
#' Weighted Box Plot with Two Facet Variables
#'
#' In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#' @param df data frame
#' @param x name of variable for x axis
#' @param y name of variable for y axis
#' @param a first faceting variable
#' @param b second faceting variable
#' @param weights name of weights variable
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' gghexweight3d(apistrat, api99, api00, stype, awards, pw)
gghexweight3d = function(df, x, y, a, b, weights){
  return(ggplot(df, aes(x= {{x}}, y= {{y}}))+geom_hex(aes(weight = {{weights}}))+facet_grid(rows = vars({{a}}), cols = vars({{b}})))
}


#' Weighted Hex Plot of svy.design with Two Faceting Variables
#'
#' In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#' @param surveyobj svy.design
#' @param x variable for x axis
#' @param y variable for y axis
#' @param a horizontal facetting variable
#' @param b vertical facetting variable
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' gghexweight3d_svy(dstrat, api99, api00, stype, awards)
gghexweight3d_svy = function(surveyobj, x, y, a, b){
  df = surveyobj$variables
  weights = stats::weights(surveyobj)
  return(ggplot(df, aes(x= {{x}}, y= {{y}}))+geom_hex(aes(weight = {{weights}}))+facet_grid(rows = vars({{a}}), cols = vars({{b}})))
}


