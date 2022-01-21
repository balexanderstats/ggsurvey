#' Weighted Histogram
#'
#'In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#' @param df data frame
#' @param x variable of interest
#' @param weights survey weights that sum to sample size
#' @param binwidth desired binwidth, if NULL bins in geom_histogram defaults to 30
#'
#' @return
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' gghistweight(apistrat, api00, pw)
#' gghistweight(apistrat, api00, pw, binwidth = 10)
#' data(election)
#' gghistweight(election_pps, Bush, p)
gghistweight = function(df, x, weights, binwidth = NULL){
  if(is.null(binwidth)){
    plotnew = ggplot(df, aes({{x}}))+geom_histogram(aes(weight = {{weights}}))
  }
  else{
    plotnew = ggplot(df, aes({{x}}))+geom_histogram(aes(weight = {{weights}}), binwidth = binwidth)
  }
  return(plotnew)
}


#' Histogram of svgdesign object
#'
#'In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#' @param surveyobj svy.design object
#' @param x variable to histogram
#' @param binwidth binwidth to pass to geom_hist
#'
#' @return
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' gghistweight_svy(dstrat, api00)
#' gghistweight_svy(dstrat, api00, binwidth = 10)
gghistweight_svy = function(surveyobj, x, binwidth = NULL){
  df = surveyobj$variables
  weights = weights(surveyobj)
  if(is.null(binwidth)){
    plotnew = ggplot(df, aes({{x}}))+geom_histogram(aes(weight = {{weights}}))
  }
  else{
    plotnew = ggplot(df, aes({{x}}))+geom_histogram(aes(weight = {{weights}}), binwidth = binwidth)
  }
  return(plotnew)
}

#' Weighted Histogram with One Facet

#'In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
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
#' library(survey)
#' data(api)
#' gghistweight2d(apistrat, api00, stype, pw)
#' gghistweight2d(apistrat, api00, stype, pw, binwidth = 10)
gghistweight2d = function(df, x, y, weights, binwidth = NULL){
  if(is.null(binwidth)){
    plotnew = ggplot(df, aes({{x}}))+geom_histogram(aes(weight = {{weights}}))+facet_grid(rows = vars({{y}}))
  }
  else{
    plotnew = ggplot(df, aes({{x}}))+geom_histogram(aes(weight = {{weights}}), binwidth = binwidth) +facet_grid(rows = vars({{y}}))
  }
  return(plotnew)
}


#' Histogram of svy.object with One Facet
#'
#'In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#' @param surveyobj svy.design object
#' @param x variable to histogram
#' @param y categorical variable to facet
#' @param binwidth binwidth to pass to geom_hist
#'
#' @return
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' gghistweight2d_svy(dstrat, api00, stype)
#' gghistweight2d_svy(dstrat, api00, stype, binwidth = 10)
gghistweight2d_svy = function(surveyobj, x, y, binwidth = NULL){
  df = surveyobj$variables
  weights = weights(surveyobj)
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
#'In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
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
#' library(survey)
#' data(api)
#' gghistweight3d(apistrat, api00, stype, awards, pw)
#' gghistweight3d(apistrat, api00, stype, awards, pw, binwidth = 10)
gghistweight3d = function(df, x, y, z, weights, binwidth = NULL){
  if(is.null(binwidth)){
    plotnew = ggplot(df, aes({{x}}))+geom_histogram(aes(weight = {{weights}}))+facet_grid(rows = vars({{y}}), cols = vars({{z}}))
  }
  else{
    plotnew = ggplot(df, aes({{x}}))+geom_histogram(aes(weight = {{weights}}), binwidth = binwidth) +facet_grid(rows = vars({{y}}), cols = vars({{z}}))
  }
  return(plotnew)
}
#' Histogram of svy.design object with two facets
#'
#' In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#' @param surveyobj svy.design object
#' @param x variable to histogram
#' @param y horizontal facet
#' @param z vertical facet
#' @param binwidth binwidth to pass to geom_hist
#'
#' @return
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' gghistweight3d_svy(dstrat, api00, stype, awards)
#' gghistweight3d_svy(dstrat, api00, stype, awards, binwidth = 10)
gghistweight3d_svy = function(surveyobj, x, y, z, binwidth = NULL){
  df = surveyobj$variables
  weights = weights(surveyobj)
  if(is.null(binwidth)){
    plotnew = ggplot(df, aes({{x}}))+geom_histogram(aes(weight = {{weights}}))+facet_grid(rows = vars({{y}}), cols = vars({{z}}))
  }
  else{
    plotnew = ggplot(df, aes({{x}}))+geom_histogram(aes(weight = {{weights}}), binwidth = binwidth) +facet_grid(rows = vars({{y}}), cols = vars({{z}}))
  }
  return(plotnew)
}
