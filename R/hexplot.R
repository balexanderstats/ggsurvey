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
#' data(api)
#' gghexweight(apistrat, api99, api00, pw)
gghexweight = function(df, x, y, weights){
  return(ggplot(df, aes(x= {{x}}, y= {{y}}))+geom_hex(aes(weight = {{weights}})))
}


#' Weighted Hex Plot of Survey Design Object
#'
#' @param surveyobj svy.design
#' @param x name of variable for x axis
#' @param y name of variable for y axis
#'
#' @return
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' gghexweight_svy(dstrat, api99, api00)
gghexweight_svy = function(surveyobj, x, y){
  df = surveyobj$variables
  df$weights = weights(surveyobj)
  return(ggplot(df, aes(x= {{x}}, y= {{y}}))+geom_hex(aes(weight = weights)))
}

#' Weighted Hex Plot with One Facet Variable
#'
#' @param df data frame
#' @param x name of variable for x axis
#' @param y name of variable for y axis
#' @param z faceting categorical variable
#' @param weights desired binwidth, if NULL bins in geom_histogram defaults to 30
#'
#' @return
#' @export
#'
#' @examples
#' data(api)
#' gghexweight2d(apistrat, api99, api00, stype, pw)
gghexweight2d = function(df, x, y, z, weights){
  return(ggplot(df, aes(x= {{x}}, y= {{y}}))+geom_hex(aes(weight = {{weights}}))+facet_grid(rows = vars({{z}})))
}


#' Weighted Hex Plot of svy.design with One Facet Variable
#'
#' @param surveyobj svy.design
#' @param x variable for x axis
#' @param y variable for y axis
#' @param z faceting variable
#'
#' @return
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' gghexweight2d_svy(dstrat, api99, api00, stype)
gghexweight2d_svy = function(surveyobj, x,y, z){
  df = surveyobj$variables
  weights = weights(surveyobj)
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
#' data(api)
#' gghexweight3d(apistrat, api99, api00, stype, awards, pw)
gghexweight3d = function(df, x, y, a, b, weights){
  return(ggplot(df, aes(x= {{x}}, y= {{y}}))+geom_hex(aes(weight = {{weights}}))+facet_grid(rows = vars({{a}}), cols = vars({{b}})))
}


#' Weighted Hex Plot of svy.design with Two Faceting Variables
#'
#' @param surveyobj svy.design
#' @param x variable for x axis
#' @param y variable for y axis
#' @param a horizontal facetting variable
#' @param b vertical facetting variable
#'
#' @return
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' gghexweight3d_svy(dstrat, api99, api00, stype, awards)
gghexweight3d_svy = function(surveyobj, x, y, a, b){
  df = surveyobj$variables
  weights = weights(surveyobj)
  return(ggplot(df, aes(x= {{x}}, y= {{y}}))+geom_hex(aes(weight = {{weights}}))+facet_grid(rows = vars({{a}}), cols = vars({{b}})))
}


