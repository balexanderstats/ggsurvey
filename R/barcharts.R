#' Weighted Univariate Bar Charts
#'
#' In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#'
#' @param df data frame of survey
#' @param x  name of question of interest
#' @param weights survey weights that sums to sample size
#' @param fill if true the fill of each bar will be a different color corresponding to the level of the factor
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(survey)
#' #Example with data frame
#' data(api)
#' ggbarweight(apistrat, stype, pw)
#' ggbarweight(apistrat, stype, pw, fill = TRUE)
#' data(nhanes)
#' ggbarweight(nhanes, race, WTMEC2YR)+ylab("Proportion")
ggbarweight = function(df, x, weights, fill = NULL){
  f =  NULL
  newdf = df %>% group_by({{x}})  %>% tally(wt = {{weights}}) %>% mutate(f = n/sum(n))
  if(is.null(fill)){
    plot = ggplot(newdf, aes({{x}}))+geom_bar(aes(weight =f))
  }
  else if(fill == TRUE){
    plot = ggplot(newdf, aes({{x}}))+geom_bar(aes(weight = f, fill = {{x}}))
  }
  return(plot)}



#' Bar Chart from svydesign objects
#'
#'In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#'
#' @param surveyobj svydesign
#' @param x variable to plot
#' @param fill if true the fill of each bar will be a different color corresponding to the level of the factor
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' ggbarweight_svy(dstrat, stype)+ylab("Proportion")
#' ggbarweight_svy(dstrat, stype, fill = TRUE)
#' data(nhanes)
#' design <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTMEC2YR, nest=TRUE,data=nhanes)
#' ggbarweight_svy(design, agecat)+ylab("Proportion")
#' ggbarweight_svy(design, agecat, fill = TRUE)+ylab("Proportion")
ggbarweight_svy = function(surveyobj, x, fill = NULL){
  f = NULL
  df = surveyobj$variables
  wts = stats::weights(surveyobj)
  df$wts = wts
  newdf = df %>% group_by({{x}})  %>% tally(wt = wts) %>% mutate(f = n/sum(n))
  if(is.null(fill)){
    plot = ggplot(newdf, aes({{x}}))+geom_bar(aes(weight = f))
  }
  else if(fill == TRUE){
    plot = ggplot(newdf, aes({{x}}))+geom_bar(aes(weight = f, fill = {{x}}))
  }
  return(plot)}

#' Crosstabs of Two Variables
#'
#' In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#' This function creates a crosstab of x by a second variable y.
#'
#' @param df data frame of survey
#' @param x variable to bar chart
#' @param y faceting variable
#' @param weights survey weights that sum to sample size
#' @param fill if true the fill of each bar will be a different color corresponding to the level of the factor
#' @param labeller argument to pass onto facet_grid
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' ggbarcrosstabs(apistrat, stype, yr.rnd, pw)+ylab("Proportion")
#' ggbarcrosstabs(apistrat, stype, yr.rnd, pw, fill = TRUE)+ylab("Proportion")
#' data(nhanes)
#' ggbarcrosstabs(nhanes, race, agecat, WTMEC2YR)
ggbarcrosstabs = function(df, x, y, weights, fill = NULL, labeller = NULL){
  f = NULL
  newdf = df %>% group_by({{y}},{{x}})  %>% tally(wt = {{weights}}) %>% mutate(f = n/sum(n))
  if(is.null(fill)){
    if(is.null(labeller)){
      plotnew = ggplot(newdf, aes({{x}}))+geom_bar(aes(weight = f))+facet_grid(cols = vars({{y}}))
    }
    else{
      plotnew = ggplot(newdf, aes({{x}}))+geom_bar(aes(weight = f))+facet_grid(cols = vars({{y}}), labeller = labeller)
    }

  }
  else if(fill == TRUE){
    if(is.null(labeller)){
      plotnew = ggplot(newdf, aes({{x}}))+geom_bar(aes(weight = f, fill = {{x}}))+facet_grid(cols = vars({{y}}))}
    else{
      plotnew = ggplot(newdf, aes({{x}}))+geom_bar(aes(weight = f, fill = {{x}}))+facet_grid(cols = vars({{y}}), labeller = labeller())}
  }
  return(plotnew)
}


#' Crosstabs for svy.design objects
#'
#'In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#'
#' @param x variable for bar chart
#' @param y faceting variable (comparison factor)
#' @param surveyobj svy.design obj
#' @param fill if true the fill of each bar will be a different color corresponding to the level of the factor
#' @param labeller argument to pass onto facet_grid
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(survey)
#' library(ggplot2)
#' data(api)
#' dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' ggbarcrosstabs_svy(dstrat, stype, yr.rnd)+ylab("Proportion")
#' ggbarcrosstabs_svy(dstrat, stype, yr.rnd, TRUE)+ylab("Proportion")
#' data(nhanes)
#' design <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTMEC2YR, nest=TRUE,data=nhanes)
#' ggbarcrosstabs_svy(design, race, agecat)

ggbarcrosstabs_svy = function(surveyobj, x, y, fill = NULL, labeller = NULL){
  f = NULL
  wts = NULL
  df = surveyobj$variables
  df$wts = stats::weights(surveyobj)
  newdf = df %>% group_by({{y}},{{x}})  %>% tally(, wt = wts) %>% mutate(f = n/sum(n))
  if(is.null(fill)){
    if(is.null(labeller)){
      plotnew = ggplot(newdf, aes({{x}}))+geom_bar(aes(weight = f))+facet_grid(cols = vars({{y}}))
    }
    else{
      plotnew = ggplot(newdf, aes({{x}}))+geom_bar(aes(weight = f))+facet_grid(cols = vars({{y}}), labeller = labeller)
    }

  }
  else if(fill == TRUE){
    if(is.null(labeller)){
      plotnew = ggplot(newdf, aes({{x}}))+geom_bar(aes(weight = f, fill = {{x}}))+facet_grid(cols = vars({{y}}))}
    else{
      plotnew = ggplot(newdf, aes({{x}}))+geom_bar(aes(weight = f, fill = {{x}}))+facet_grid(cols = vars({{y}}), labeller = labeller())}
    }

  return(plotnew)
}


#' Crosstabs of Three Variables
#'
#'In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#'
#' @param df data frame
#' @param x bar chart variable
#' @param y crosstab variable 1 (horizontal facets)
#' @param z crosstab variable 2 (vertical facets)
#' @param weights survey weights that sum to sample size
#' @param fill if true the fill of each bar will be a different color corresponding to the level of the factor
#' @param labeller argument to pass onto facet_grid
#'
#' @return ggplot pbject
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' ggbarcrosstabs3d(apistrat, stype, yr.rnd, awards, pw)
#' ggbarcrosstabs3d(apistrat, stype, yr.rnd, awards, pw, TRUE)
#' data(nhanes)
#' ggbarcrosstabs3d(nhanes, race, agecat, RIAGENDR, WTMEC2YR)
ggbarcrosstabs3d = function(df, x, y, z, weights, fill = NULL, labeller = NULL){
  f = NULL
  newdf = df %>% group_by({{z}},{{y}}, {{x}})  %>% tally(, wt = {{weights}}) %>% mutate(f = n/sum(n))
  if(is.null(fill)){
    if(is.null(labeller)){
      plotnew = ggplot(newdf, aes({{x}}))+geom_bar(aes(weight = f))+facet_grid(rows = vars({{y}}), cols = vars({{z}}))
    }
    else{
      plotnew = ggplot(newdf, aes({{x}}))+geom_bar(aes(weight = f))+facet_grid(rows = vars({{y}}), cols = vars({{z}}), labeller = labeller)
    }

  }
  else if(fill == TRUE){
    if(is.null(labeller)){
      plotnew = ggplot(newdf, aes({{x}}))+geom_bar(aes(weight = f, fill = {{x}}))+facet_grid(rows = vars({{y}}), cols = vars({{z}}))

    }
    else{
      plotnew = ggplot(newdf, aes({{x}}))+geom_bar(aes(weight = f, fill = {{x}}))+facet_grid(rows = vars({{y}}), cols = vars({{z}}), labeller = labeller)
    }

  }

  return(plotnew)

}

#' Crosstabs of Three Variables Using svy.design object
#'
#'In ggsurvey you specify both the plotting variables and weights in plain text with no quotes.
#'
#' @param surveyobj svy.design obj
#' @param x bar chart variable
#' @param y crosstab variable 1 (horizontal facets)
#' @param z crosstab variable 2 (vertical facets)
#' @param fill if true the fill of each bar will be a different color corresponding to the level of the factor
#' @param labeller argument to pass onto facet_grid
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(survey)
#' data(api)
#' dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' ggbarcrosstabs3d_svy(dstrat, stype, yr.rnd, awards)
#' ggbarcrosstabs3d_svy(dstrat, stype, yr.rnd, awards, fill = TRUE)
#' data(nhanes)
#' design <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTMEC2YR, nest=TRUE,data=nhanes)
#' ggbarcrosstabs3d_svy(design, race, agecat, RIAGENDR)
ggbarcrosstabs3d_svy = function(surveyobj, x, y, z, fill = NULL, labeller= NULL){
  f = NULL
  wts = NULL
  df = surveyobj$variables
  df$wts = stats::weights(surveyobj)
  newdf = df %>% group_by({{z}},{{y}}, {{x}})  %>% tally(, wt = wts) %>% mutate(f = n/sum(n))
  if(is.null(fill)){
    if(is.null(labeller)){
      plotnew = ggplot(newdf, aes({{x}}))+geom_bar(aes(weight = f))+facet_grid(rows = vars({{y}}), cols = vars({{z}}))
    }
    else{
      plotnew = ggplot(newdf, aes({{x}}))+geom_bar(aes(weight = f))+facet_grid(rows = vars({{y}}), cols = vars({{z}}), labeller = labeller)
    }

  }
  else if(fill == TRUE){
    if(is.null(labeller)){
      plotnew = ggplot(newdf, aes({{x}}))+geom_bar(aes(weight = f, fill = {{x}}))+facet_grid(rows = vars({{y}}), cols = vars({{z}}))

    }
    else{
      plotnew = ggplot(newdf, aes({{x}}))+geom_bar(aes(weight = f, fill = {{x}}))+facet_grid(rows = vars({{y}}), cols = vars({{z}}), labeller = labeller)
    }

  }

  return(plotnew)

}

