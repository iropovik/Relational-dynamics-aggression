#' ---
#' title: "Appendix B for: How Affective Relationships and Classroom Norms Shape Perceptions of Aggressor, Victim, and Defender Roles"
#' author: ""
#' date: "`r Sys.Date()`"
#' output:
#'    html_document:
#'       toc: true
#'       toc_float: true
#'       code_folding: show
#'       fig_retina: 2
#' always_allow_html: yes
#' ---
#+ setup, include = FALSE
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, fig.align = "center", fig.width=12, fig.height=10, out.width="100%", out.height="100%")

# Report the results for the healthy context paradox?
healthyContext <- FALSE

# Install and load required libraries
list.of.packages <- c("tidyverse", "magrittr", "knitr", "metafor")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#+ include = FALSE
lapply(list.of.packages, require, quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)

# Load the workspace from the file
#if(healthyContext == TRUE){
#  load("resultsObjectsHCP.RData")
#} else {
  load("resultsObjects.RData") 
#}

#' The following report presents detailed results for individual models, aggregated results synthesized using meta-analyses, and moderator analyses.
#' 
  
#'# Individual models results
#'
#' List of results for individual models. c#### denotes the class ID, followed by the type of model. 
#' 
#' EstMeans = aggregated (meta-analytic) log odds ratio; SE = standard error; t-stat = convergence parameter
lapply(resultTable, roundDf, 3)

#'# Meta-analysis
#'
#if(healthyContext == FALSE){
#'## Meta-analysis for Baseline Models
#'
#' Note: effectSize = aggregated (meta-analytic) log odds ratio; se = standard error; z = z-value (ratio of effect size to it's standard error); p = p-value; ciLB and ciUB = lower and upper bound of confidence interval, respectively
  roundDf(metaResultsBaseline, digits = 3)
#}
#'## Meta-analysis for Reduced Models
#'
#' Note: effectSize = aggregated (meta-analytic) log odds ratio; se = standard error; z = z-value (ratio of effect size to it's standard error); p = p-value; ciLB and ciUB = lower and upper bound of confidence interval, respectively
roundDf(metaResultsReduced, digits = 3)
#'## Meta-analysis for Full Models
#'
#' Note: effectSize = aggregated (meta-analytic) log odds ratio; se = standard error; z = z-value (ratio of effect size to it's standard error); p = p-value; ciLB and ciUB = lower and upper bound of confidence interval, respectively
roundDf(metaResultsFull, digits = 3)

#'# Moderator analysis
#'
#'## Moderator analysis for Baseline Models
#'
#' Note: intercept = intercept of the meta-regression model; effectSize = aggregated (meta-analytic) log odds ratio; SE = standard error; LRT = likelihood ratio test value; p = p-value; BF10 = default Bayes factor in favor of the H1; posterior = posterior probability of the given parameter
modResultsBaseline
#'## Moderator analysis for Reduced Models
#'
#' Note: intercept = intercept of the meta-regression model; effectSize = aggregated (meta-analytic) log odds ratio; SE = standard error; LRT = likelihood ratio test value; p = p-value; BF10 = default Bayes factor in favor of the H1; posterior = posterior probability of the given parameter
modResultsReduced
#'## Moderator analysis for Full Models
#'
#' Note: intercept = intercept of the meta-regression model; effectSize = aggregated (meta-analytic) log odds ratio; SE = standard error; LRT = likelihood ratio test value; p = p-value; BF10 = default Bayes factor in favor of the H1; posterior = posterior probability of the given parameter
modResultsFull

#'#### Session info
sessionInfo()