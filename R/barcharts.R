#' Weighted Univariate Bar Charts
#'
#' In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#' @param df data frame of survey
#' @param x  name of question of interest
#' @param weights survey weights that sums to sample size
#'
#' @return ggplot object that can be customized using ggplot2 functions including adding titles
#' @export
#'
#' @examples
#' library(survey)
#' #Example with data frame
#' data(api)
#' ggbarweight(apistrat, stype, pw)+ggtitle("Proportion of School Type")+ylab("Proportion")
#' data(nhanes)
#' ggbarweight(nhanes, race, WTMEC2YR)+ylab("Proportion")
ggbarweight = function(df, x, weights){
  return(ggplot(df, aes({{x}}))+geom_bar(aes(weight = {{weights}}, y = (..count..)/sum(..count..))))}



#' Bar Chart from svydesign objects
#'
#'In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#' @param surveyobj svydesign
#' @param x variable to plot
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' ggbarweight_svy(dstrat, stype)+ylab("Proportion")
#' data(nhanes)
#' design <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTMEC2YR, nest=TRUE,data=nhanes)+ylab("Proportion")
#' ggbarweight_svy(design, race)
ggbarweight_svy = function(surveyobj, x){
  df = surveyobj$variables
  weights = weights(surveyobj)
  return(ggplot(df, aes({{x}}))+geom_bar(aes(weight = weights, y = (..count..)/sum(..count..))))
}

#' Crosstabs of Two Variables
#'
#'In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#' @param df data frame of survey
#' @param x variable to bar chart
#' @param y faceting variable
#' @param weights survey weights that sum to sample size
#'
#' @return ggplot object that can be customized using ggplot2 functions including adding titles
#' @export
#'This function creates a crosstab of x by a second variable y
#' @examples
#' library(survey)
#' data(api)
#' ggbarcrosstabs(apistrat, stype, yr.rnd, pw)+ylab("Proportion")+ggtitle("School Type by Year Round Status using api From survey Package")+xlab("Type of School E=Elementary, M=Middle School, H=High School")
#' data(nhanes)
#' ggbarcrosstabs(nhanes, race, agecat, WTMEC2YR)
ggbarcrosstabs = function(df, x, y, weights){
  newdf = df %>% group_by({{y}},{{x}})  %>% tally(, wt = {{weights}}) %>% mutate(f = n/sum(n))
  plotnew = ggplot(newdf, aes({{x}}))+geom_bar(aes(weight = f))+facet_grid(cols = vars({{y}}))
  return(plotnew)
}


#' Crosstabs for svy.design objects
#'
#'In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#' @param svyobj svy.design
#' @param x variable for bar chart
#' @param y faceting variable (comparison factor)
#'
#' @return
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' ggbarcrosstabs_svy(dstrat, stype, yr.rnd)+ylab("Proportion")
#' data(nhanes)
#' design <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTMEC2YR, nest=TRUE,data=nhanes)+ylab("Proportion")
#' ggbarcrosstabs_svy(design, race, agecat)
ggbarcrosstabs_svy = function(surveyobj, x, y){
  df = surveyobj$variables
  df$weights = weights(surveyobj)
  newdf = df %>% group_by({{y}},{{x}})  %>% tally(, wt = {{weights}}) %>% mutate(f = n/sum(n))
  plotnew = ggplot(newdf, aes({{x}}))+geom_bar(aes(weight = f))+facet_grid(cols = vars({{y}}))
  return(plotnew)
}


#' Crosstabs of Three Variables
#'
#'In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#' @param df data frame
#' @param x bar chart variable
#' @param y crosstab variable 1 (horizontal facets)
#' @param z crosstab variable 2 (vertical facets)
#' @param weights survey weights that sum to sample size
#'
#' @return
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' ggbarcrosstabs3d(apistrat, stype, yr.rnd, awards, pw)
#' data(nhanes)
#' ggbarcrosstabs3d(nhanes, race, agecat, RIAGENDR, WTMEC2YR)
ggbarcrosstabs3d = function(df, x, y, z, weights){
  newdf = df %>% group_by({{z}},{{y}}, {{x}})  %>% tally(, wt = {{weights}}) %>% mutate(f = n/sum(n))
  plotnew = ggplot(newdf, aes({{x}}))+geom_bar(aes(weight = f))+facet_grid(rows = vars({{y}}), cols = vars({{z}}))
  return(plotnew)

}

#' Crosstabs of Three Variables Using svy.design object
#'
#'In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#' @param surveyobj svy.design obj
#' @param x bar chart variable
#' @param y crosstab variable 1 (horizontal facets)
#' @param z crosstab variable 2 (vertical facets)
#'
#' @return
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' ggbarcrosstabs3d_svy(dstrat, stype, yr.rnd, awards)
#' data(nhanes)
#' design <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTMEC2YR, nest=TRUE,data=nhanes)+ylab("Proportion")
#' ggbarcrosstabs3d_svy(design, race, agecat, RIAGENDR)
ggbarcrosstabs3d_svy = function(surveyobj, x, y, z){
  df = surveyobj$variables
  df$weights = weights(surveyobj)
  newdf = df %>% group_by({{z}},{{y}}, {{x}})  %>% tally(, wt = weights) %>% mutate(f = n/sum(n))
  plotnew = ggplot(newdf, aes({{x}}))+geom_bar(aes(weight = f))+facet_grid(rows = vars({{y}}), cols = vars({{z}}))
  return(plotnew)

}

