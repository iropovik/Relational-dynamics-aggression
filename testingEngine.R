#' ---
#' title: "Incorporating bullying perceptions into relational dynamics of lower-secondary students"
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
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, fig.align = "center")

# packageurl <- "https://cran.r-project.org/src/contrib/Archive/RSiena/RSiena_1.3.14.3.tar.gz"
# install.packages(packageurl, repos = NULL, type = "source")

# rm(list = ls())
startTime <- Sys.time()
# Load the workspace from the file
# load("resultsObjects.RData")

# Settings ----------------------------------------------------------------

# Should Healthy Context paradox be modeled?
healthyContext <- TRUE

# Multicore setting
n.clus <- detectCores() - 1

# Setting for the verbose argument
verboseOn <- FALSE

# Threshold check for estimates that are way out
estimateThreshold <-  100 # Defaults to 20, set to Inf to turn off estimate check

# What is the maximum number of attempts in estimating the Siena model (if there are convergence issues)?
maxAttempt <- 1 # Should take values 5 to 6 if skippingLogic == TRUE. Set to 6 for maximum no of attempts including 2 "overkill" models. Set 4 to not estimate the "overkill" models. Set to 5 for a more optimal tradeoff between convergence and computation time.

# Skipping logic
skippingLogic <- FALSE # Set to FALSE for HCP paper.

# Define fitmeasures
fitMea <- c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr")

# Define the number of bootstrap samples for CFAs
nBoot <- 2000

# Define the number of decimals to round to
decimals <- 3

# Source scripts ----------------------------------------------------------
source("functions.R")

# Creation of files storing the matrices for each class -------------------
# Read wide data, do some data wrangling and get all csv files in the directory
dataWide <- read_sav("dataWide.sav")

# Data wrangling
# Recoding some variables
dataWide <- dataWide %>%
  mutate(
    across(c(aas1, aas2, aas3, bas1, bas2, bas3, bas4), ~ ordered(round(.x, 0))),
    aas2R = ifelse(abs(as.numeric(aas2) - 6) == 1, NA, ordered(abs(as.numeric(aas2) - 6))),
    bas2R = ordered(abs(as.numeric(bas2) - 6))
  )

# Read in the data --------------------------------------------------------
# Define the variable names and corresponding sheet numbers
matrixNames <- c("att", "friend1", "friend2", "excl1", "excl2", "bully1", 
                 "bully2", "victim1", "victim2", "defender1", "defender2")

# Get all .xlsx files in the 'data' directory
files <- list.files(path = "data", pattern = "\\.xlsx$")

# Read each file and store in the list
dat <- list()
for (file in files) {
  # Create a list to store the matrices from the current file
  fileData <- list()
  # Read each sheet in the current file and store in the list
  for (i in seq_along(matrixNames)) {
    if (i == 1) {
      # For the first sheet (attributes), read as a data frame
      fileData[[matrixNames[i]]] <- read.xlsx(paste0("data/", file), sheet = i, colNames = TRUE, rowNames = FALSE)
    } else {
      # For all other sheets, read as a matrix
      fileData[[matrixNames[i]]] <- as.matrix(read.xlsx(paste0("data/", file), sheet = i, colNames = TRUE, rowNames = TRUE))
    }
  }
  # Store the list for the current file in the overall list, using the file name as the name
  dat[[gsub("\\.xlsx$", "", file)]] <- fileData
}
names(dat) <- paste0("c", names(dat))

# Create a data frame to hold classroom and model combinations
if(healthyContext == TRUE){
  modelCombinations <- data.frame(
    classroom = c(reducedModel, fullModel),
    modelType = c(rep("reduced", length(reducedModel)), rep("full", length(fullModel)))
  )
  modelCombinations$classroom <- paste0("c", modelCombinations$classroom)
  availableClasses <- names(dat)
  modelCombinations <- subset(modelCombinations, classroom %in% availableClasses)
} else {
  modelCombinations <- data.frame(
  classroom = c(baselineModel, reducedModel, fullModel),
  modelType = c(rep("baseline", length(baselineModel)), rep("reduced", length(reducedModel)), rep("full", length(fullModel)))
)
  modelCombinations$classroom <- paste0("c", modelCombinations$classroom)
  availableClasses <- names(dat)
  modelCombinations <- subset(modelCombinations, classroom %in% availableClasses)
}

# Generating a list of popularities per each class
idegree_by_class <- lapply(dat, calculate_class_idegree)

mat_victim <- mat_defender <- mat_bully <- list()

for (classroom_name in names(idegree_by_class)) {
  classroom <- idegree_by_class[[classroom_name]]
  victimT1 <- classroom$victim1
  defenderT1 <- classroom$defender1
  bullyT1 <- classroom$bully1
  mat_victim[[classroom_name]] <- victimT1
  mat_defender[[classroom_name]] <- defenderT1
  mat_bully[[classroom_name]] <- bullyT1
}

# Creating covariates using 1SD above mean per each victim, bully, defender matrix
medianSwitch <- TRUE # Select TRUE if you want median instead of mean; FALSE if you want mean value

# Function to create fourth quartile cutoff
create_fourth_quartile_cutoff <- function(mat) {
  combined_data <- unlist(mat)
  cutoff_value <- quantile(combined_data, 0.75, na.rm = TRUE)
  return(cutoff_value)
}

# Existing list of matrices
matrices_cutoffs <- list(mat_victim, mat_defender, mat_bully)

# Apply the function to each matrix in the list
cutoffs_fourth_quartile <- lapply(matrices_cutoffs, create_fourth_quartile_cutoff)
names(cutoffs_fourth_quartile) <- c("victim", "defender", "bully")

# Function to binarize values
binarize_values <- function(vector1, cutoff) {
  ifelse(vector1 >= cutoff, 1, 0)
}

# Now you can use these new cutoffs_fourth_quartile values in your binarization functions
binarize_values_vict <- function(vector1) {
  binarize_values(vector1, cutoffs_fourth_quartile$victim)
}
binarize_values_defe <- function(vector1) {
  binarize_values(vector1, cutoffs_fourth_quartile$defender)
}
binarize_values_bully <- function(vector1) {
  binarize_values(vector1, cutoffs_fourth_quartile$bully)
}
mat_victimB <- lapply(mat_victim,binarize_values_vict)
mat_defenderB <- lapply(mat_defender,binarize_values_defe)
mat_bullyB <- lapply(mat_bully,binarize_values_bully)

# just to optically check numbers per classes
sum_vectors <- function(vector1) {
  sum_vec <- sum(vector1,na.rm = TRUE)
  return(sum_vec)
}

nr_victims <- lapply(mat_victimB,sum_vectors)
nr_defenders <- lapply(mat_defenderB,sum_vectors)
nr_bullies <- lapply(mat_bullyB,sum_vectors)

class_names <- names(mat_victim)

victim_victim <- defender_victim <- bully_victim <- list()

for (class_name in class_names) {
  indegree_victims <- mat_victimB[[class_name]]
  indegree_defender <- mat_defenderB[[class_name]]
  indegree_bully <- mat_bullyB[[class_name]]
  
  num_students <- length(indegree_victims)
  new_adj.mat_victimT1 <- matrix(0, nrow = num_students, ncol = num_students)
  new_adj.mat_defenderT1 <- matrix(0, nrow = num_students, ncol = num_students)
  new_adj.mat_bullyT1 <- matrix(0, nrow = num_students, ncol = num_students)
  
  for (i in 1:num_students) {
    for (j in 1:num_students) {
      if (i != j) {
        new_adj.mat_victimT1[i, j] <- indegree_victims[i] * indegree_victims[j]
        new_adj.mat_defenderT1[i, j] <- indegree_defender[i] * indegree_victims[j]
        new_adj.mat_bullyT1[i, j] <- indegree_bully[i] * indegree_victims[j]
        
        diag(new_adj.mat_victimT1) <- 0
        diag(new_adj.mat_defenderT1) <- 0
        diag(new_adj.mat_bullyT1) <- 0
        
        victim_victim[[class_name]] <- new_adj.mat_victimT1
        defender_victim[[class_name]] <- new_adj.mat_defenderT1
        bully_victim[[class_name]] <- new_adj.mat_bullyT1
      }
    }
  }
}

# Appending the matrices to dat list
toAppend <- list(victim_victim = victim_victim, 
                 defender_victim = defender_victim, 
                 bully_victim = bully_victim,
                 mat_victimB = mat_victimB,
                 mat_defenderB = mat_defenderB,
                 mat_bullyB = mat_bullyB)
for (classroom in names(dat)) { # Loop through each classroom code to append hte adjusted matrices and define student IDs
  idNames <- dat[[classroom]][['att']][, 'id']
  for (listName in names(toAppend)) {
    if (classroom %in% names(toAppend[[listName]])) {
      appendedMatrix <- toAppend[[listName]][[classroom]]
      if(is.matrix(appendedMatrix)){
        rownames(appendedMatrix) <- idNames
        colnames(appendedMatrix) <- idNames
      }
      dat[[classroom]][[listName]] <- appendedMatrix
    }
  }
}

# Initialize a list to hold the sienaData objects
sienaData <- list()

# Iterate through each row in modelCombinations
for (i in 1:nrow(modelCombinations)) {
  classroom <- modelCombinations$classroom[i]
  modelType <- modelCombinations$modelType[i]
  
  # Retrieve the corresponding data
  d <- dat[[classroom]]
  
  # Create the dependent networks
  friendDep <- sienaDependent(array(c(d[["friend1"]], d[["friend2"]]), dim = c(nrow(d[["friend1"]]), ncol(d[["friend1"]]), 2)))
  exclDep <- sienaDependent(array(c(d[["excl1"]], d[["excl2"]]), dim = c(nrow(d[["excl1"]]), ncol(d[["excl1"]]), 2)))
  bullyDep <- sienaDependent(array(c(d[["bully1"]], d[["bully2"]]), dim = c(nrow(d[["bully1"]]), ncol(d[["bully1"]]), 2)))
  victimDep <- sienaDependent(array(c(d[["victim1"]], d[["victim2"]]), dim = c(nrow(d[["victim1"]]), ncol(d[["victim1"]]), 2)))
  defenderDep <- sienaDependent(array(c(d[["defender1"]], d[["defender2"]]), dim = c(nrow(d[["defender1"]]), ncol(d[["defender1"]]), 2)))
  detachment <- sienaDependent(as.matrix(d[["att"]][,c("detachmentT1", "detachmentT2")]) - mean(as.matrix(d[["att"]][,c("detachmentT1", "detachmentT2")]), na.rm = TRUE), type = "continuous")
  gender <- coCovar(d[["att"]]$gender)
  if(healthyContext == TRUE){
    victimVictim <- coDyadCovar(d[["victim_victim"]])
    defenderVictim <- coDyadCovar(d[["defender_victim"]])
    bullyVictim <- coDyadCovar(d[["bully_victim"]])
    vict <- coCovar(d[["mat_victimB"]])
    bul <- coCovar(d[["mat_bullyB"]])
    def <- coCovar(d[["mat_defenderB"]])
  }
  
  # Create the RSiena data object based on the model type
  if(healthyContext == TRUE){
    (if (modelType == "reduced") {
      sienaDat <- sienaDataCreate(friendDep, exclDep, victimDep, gender, detachment, vict, victimVictim)
    } else if (modelType == "full") {
      sienaDat <- sienaDataCreate(friendDep, exclDep, victimDep, bullyDep, defenderDep, gender, detachment, vict, bul, def, victimVictim, defenderVictim, bullyVictim)
    })
  } else {
    if (modelType == "baseline") {
      sienaDat <- sienaDataCreate(friendDep, bullyDep, defenderDep, gender)
    } else if (modelType == "reduced") {
      sienaDat <- sienaDataCreate(friendDep, exclDep, bullyDep, defenderDep, gender)
    } else if (modelType == "full") {
      sienaDat <- sienaDataCreate(friendDep, exclDep, bullyDep, victimDep, defenderDep, gender)
    } 
  }
  
  # Store the sienaData object in the list, naming it according to the classroom and model type
  sienaData[[paste(classroom, ".", modelType, sep = "")]] <- sienaDat
}

# Compute the proportion of victims in classes
# Extract means and classroom codes and save to dataWide
victimClassProp <- sapply(dat, function(x) mean(x$mat_victimB, na.rm = TRUE))
victimClassProp <- data.frame(
  clid = as.double(sprintf("%04d", as.integer(substr(names(dat), 2, 5)))),
  victimClassProp = victimClassProp
)
dataWide <- left_join(dataWide, victimClassProp, by = "clid")

if(healthyContext == TRUE){
  # Define effects for the two models
  handleReducedModel <- function(effects) {
    # structural effects
    effects <- includeEffects(effects, name = "friendDep", gwespFF, inPop, outPop)
    
    # gender effects
    effects <- includeEffects(effects, name = "friendDep", egoX, altX, sameX, interaction1 = "gender")
    
    # higher-level multiplex triadic interactions
    effects <- includeEffects(effects, name = "victimDep", to, interaction1 = "friendDep", include = TRUE)
    
    # Healthy context paradox effects
    effects <- includeEffects(effects, name = "friendDep", altX, interaction1 = "vict")
    effects <- includeEffects(effects, name = "exclDep", altX, interaction1 = "vict")
    
    effects <- includeEffects(effects, name = "friendDep",X, interaction1 = "victimVictim", include = TRUE)
    effects <- includeEffects(effects, name = "exclDep",X, interaction1 = "victimVictim", include = TRUE)
    
    effects <- includeEffects(effects, name = "detachment", recipDeg, interaction1 = "friendDep")
    effects <- includeEffects(effects, name = "detachment", effFrom, interaction1 = "vict")
   
    return(effects)
  }
  
  handleFullModel <- function(effects) {
    # structural effects
    effects <- includeEffects(effects, name = "friendDep", gwespFF, inPop, outPop)
    
    # gender effects
    effects <- includeEffects(effects, name = "friendDep", egoX, altX, sameX, interaction1 = "gender")
    
    # dyadic cross-product effect
    effects <- includeEffects(effects, name = "friendDep", crprod, interaction1 = "defenderDep", include = TRUE)
    effects <- includeEffects(effects, name = "defenderDep",crprod, interaction1 = "friendDep", include = TRUE)
    
    # higher-level multiplex triadic interactions
    effects <- includeEffects(effects, name = "exclDep",to, interaction1 = "friendDep", include = TRUE)
    effects <- includeEffects(effects, name = "bullyDep",to, interaction1 = "friendDep", include = TRUE)
    effects <- includeEffects(effects, name = "victimDep",to, interaction1 = "friendDep", include = TRUE)
    effects <- includeEffects(effects, name = "defenderDep",to, interaction1 = "friendDep", include = TRUE)
    
    # Healthy context paradox effects
    effects <- includeEffects(effects, name = "friendDep", altX, interaction1 = "vict")
    effects <- includeEffects(effects, name = "friendDep", altX, interaction1 = "def")
    effects <- includeEffects(effects, name = "friendDep", altX, interaction1 = "bul")
    
    effects <- includeEffects(effects, name = "exclDep", altX, interaction1 = "vict")
    effects <- includeEffects(effects, name = "exclDep", altX, interaction1 = "def")
    effects <- includeEffects(effects, name = "exclDep", altX, interaction1 = "bul")
    
    effects <- includeEffects(effects, name = "friendDep",X, interaction1 = "victimVictim", include = TRUE)
    effects <- includeEffects(effects, name = "friendDep",X, interaction1 = "defender_victim", include = TRUE)
    effects <- includeEffects(effects, name = "friendDep",X, interaction1 = "bully_victim", include = TRUE)
    
    effects <- includeEffects(effects, name = "exclDep",X, interaction1 = "victimVictim", include = TRUE)
    effects <- includeEffects(effects, name = "exclDep",X, interaction1 = "defenderVictim", include = TRUE)
    effects <- includeEffects(effects, name = "exclDep",X, interaction1 = "bullyVictim", include = TRUE)
    
    effects <- includeEffects(effects, name = "detachment", recipDeg, interaction1 = "friendDep")
    effects <- includeEffects(effects, name = "detachment", effFrom, interaction1 = "vict")
    return(effects)
  } 
} else {
  # Define effects for the three models
  handleBaselineModel <- function(effects) {
    # Structural effects
    effects <- includeEffects(effects, name = "friendDep", gwespFF, balance, inPop, outPop)
    
    # Gender effects
    effects <- includeEffects(effects, name = "friendDep", egoX, altX, sameX, interaction1 = "gender")
    
    # Dyadic cross-product effect
    effects <- includeEffects(effects, name = "friendDep", crprod, interaction1 = "bullyDep", include = TRUE)
    effects <- includeEffects(effects, name = "bullyDep", crprod, interaction1 = "friendDep", include = TRUE)
    effects <- includeEffects(effects, name = "friendDep", crprod, interaction1 = "defenderDep", include = TRUE)
    effects <- includeEffects(effects, name = "defenderDep", crprod, interaction1 = "friendDep", include = TRUE)
    
    return(effects)
  }
  
  handleReducedModel <- function(effects) {
    # Structural effects
    effects <- includeEffects(effects, name = "friendDep", gwespFF, balance, inPop, outPop)
    
    # Gender effects
    effects <- includeEffects(effects, name = "friendDep", egoX, altX, sameX, interaction1 = "gender")
    
    # Dyadic cross-product effect
    effects <- includeEffects(effects, name = "friendDep", crprod, interaction1 = "bullyDep", include = TRUE)
    effects <- includeEffects(effects, name = "bullyDep", crprod, interaction1 = "friendDep", include = TRUE)
    effects <- includeEffects(effects, name = "friendDep", crprod, interaction1 = "defenderDep", include = TRUE)
    effects <- includeEffects(effects, name = "defenderDep", crprod, interaction1 = "friendDep", include = TRUE)
    effects <- includeEffects(effects, name = "exclDep", crprod, interaction1 = "bullyDep", include = TRUE)
    effects <- includeEffects(effects, name = "bullyDep", crprod, interaction1 = "exclDep", include = TRUE)
    effects <- includeEffects(effects, name = "exclDep", crprod, interaction1 = "defenderDep", include = TRUE)
    effects <- includeEffects(effects, name = "defenderDep", crprod, interaction1 = "exclDep", include = TRUE)
    
    # Higher-level multiplex triadic interactions
    effects <- includeEffects(effects, name = "friendDep", from, interaction1 = "exclDep", include = TRUE)
    effects <- includeEffects(effects, name = "friendDep", from, interaction1 = "bullyDep", include = TRUE)
    effects <- includeEffects(effects, name = "friendDep", from, interaction1 = "defenderDep", include = TRUE)
    effects <- includeEffects(effects, name = "exclDep", to, interaction1 = "friendDep", include = TRUE)
    effects <- includeEffects(effects, name = "bullyDep", to, interaction1 = "friendDep", include = TRUE)
    effects <- includeEffects(effects, name = "defenderDep", to, interaction1 = "friendDep", include = TRUE)
    
    return(effects)
  }
  
  handleFullModel <- function(effects) {
    # Structural effects
    effects <- includeEffects(effects, name = "friendDep", gwespFF, balance, inPop, outPop)
    
    # Gender effects
    effects <- includeEffects(effects, name = "friendDep", egoX, altX, sameX, interaction1 = "gender")
    
    # Dyadic cross-product effect
    effects <- includeEffects(effects, name = "friendDep",crprod, interaction1 = "bullyDep", include = TRUE)
    effects <- includeEffects(effects, name = "bullyDep",crprod, interaction1 = "friendDep", include = TRUE)
    effects <- includeEffects(effects, name = "friendDep", crprod, interaction1 = "victimDep", include = TRUE)
    effects <- includeEffects(effects, name = "victimDep",crprod, interaction1 = "friendDep", include = TRUE)
    effects <- includeEffects(effects, name = "friendDep", crprod, interaction1 = "defenderDep", include = TRUE)
    effects <- includeEffects(effects, name = "defenderDep",crprod, interaction1 = "friendDep", include = TRUE)
    
    effects <- includeEffects(effects, name = "exclDep", crprod, interaction1 = "bullyDep", include = TRUE)
    effects <- includeEffects(effects, name = "bullyDep",crprod, interaction1 = "exclDep", include = TRUE)
    effects <- includeEffects(effects, name = "exclDep", crprod, interaction1 = "vicimDep", include = TRUE)
    effects <- includeEffects(effects, name = "vicimDep",crprod, interaction1 = "exclDep", include = TRUE)
    effects <- includeEffects(effects, name = "exclDep", crprod, interaction1 = "defenderDep", include = TRUE)
    effects <- includeEffects(effects, name = "defenderDep",crprod, interaction1 = "exclDep", include = TRUE)
    
    # Higher-level multiplex triadic interactions
    effects <- includeEffects(effects, name = "friendDep",from, interaction1 = "exclDep", include = TRUE)
    effects <- includeEffects(effects, name = "friendDep",from, interaction1 = "bullyDep", include = TRUE)
    effects <- includeEffects(effects, name = "friendDep",from, interaction1 = "victimDep", include = TRUE)
    effects <- includeEffects(effects, name = "friendDep",from, interaction1 = "defenderDep", include = TRUE)
    
    effects <- includeEffects(effects, name = "exclDep",to, interaction1 = "friendDep", include = TRUE)
    effects <- includeEffects(effects, name = "bullyDep",to, interaction1 = "friendDep", include = TRUE)
    effects <- includeEffects(effects, name = "victimDep",to, interaction1 = "friendDep", include = TRUE)
    effects <- includeEffects(effects, name = "defenderDep",to, interaction1 = "friendDep", include = TRUE)
    
    return(effects)
  } 
}

# Initialize an empty list to store the effects for each classroom and model type
sienaEffects <- list()

# To store error messages
errorMessages <- list()

# Loop over each row in 'modelCombinations'
for (i in 1:nrow(modelCombinations)) {
  
  # Extract the classroom and modelType from the current row
  classroom <- modelCombinations[i, 'classroom']
  modelType <- modelCombinations[i, 'modelType']
  
  # Find the corresponding dataset in 'sienaData' based on the classroom and modelType
  d <- sienaData[[paste(classroom, ".", modelType, sep = "")]]
  
  tryCatch({
    # Initialize effects
    effects <- getEffects(d)
    
    # Check the model type and specify effects accordingly
    if (modelType == "baseline") {
      effects <- handleBaselineModel(effects)
    } else if (modelType == "reduced") {
      effects <- handleReducedModel(effects)
    } else if (modelType == "full") {
      effects <- handleFullModel(effects)
    }
    
    # Store the effects object for the current dataset and model type in the overall list
    sienaEffects[[paste(classroom, ".", modelType, sep = "")]] <- effects
  }, error = function(e) {
    # Store the error message
    errorMessages[[paste(classroom, ".", modelType, sep = "")]] <- e$message
    message(paste("Skipping classroom due to error:", classroom, modelType))
  })
}


# Function to check convergence and whether the estimates are reasonable
convergenceOk <- function(res) {
  return(res$tconv.max < 0.25 & max(abs(res$tstat)) < 0.1)
}

estimatesOk <- function(res) {
  # Identify indices of parameters with names containing "basic rate"
  excludeParams <- which(grepl("basic rate", res$requestedEffects$effectName))
  # Remove these parameters from the theta and se vectors
  thetaFiltered <- res$theta[-excludeParams]
  seFiltered <- res$se[-excludeParams]
  # Compute the conditions
  maxThetaCondition <- max(abs(thetaFiltered)) < estimateThreshold
  maxSeCondition <- max(abs(seFiltered)) < estimateThreshold
  return(maxThetaCondition & maxSeCondition)
}

# Initialize an empty data frame to store error messages
errorMessages <- data.frame(modelName = character(), errorMessage = character(), stringsAsFactors = FALSE)

# Function to estimate the model
estimateModel <- function(dataKey, effectsKey, maxAttempts = maxAttempt, estimateThreshold = estimateThreshold, skipLogic = skippingLogic) {
  d <- sienaData[[dataKey]]
  effects <- sienaEffects[[effectsKey]]
  result <- list()
  skipNext <- 0  # Internal counter to keep track of how many iterations to skip
  
  for (i in 1:maxAttempts) {
    if (skipNext > 0 && !skipLogic) {  # Only skip if skipLogic is FALSE
      skipNext <- skipNext - 1
      next  # Skip this iteration
    }
    
    # Estimate the model
    if(healthyContext == TRUE){
      resultTry <- tryCatch({
        if (i == 1) {
          siena07(sienaAlgorithm, data = d, effects = effects,
                  returnDeps = TRUE, useCluster = TRUE, nbrNodes = n.clus, initC = TRUE)
        } else {
          if (i %in% 2:6) {
            siena07(sienaAlgorithmHCP, data = d, effects = effects, prevAns = result[[i - 1]],
                    returnDeps = TRUE, useCluster = TRUE, nbrNodes = n.clus, initC = TRUE)
          }
        }
      }, error = function(e) {
        # Append a row to errorMessages
        errorMsg <- as.character(e)
        errorMessages <<- rbind(errorMessages, data.frame(modelName = effectsKey, errorMessage = errorMsg, stringsAsFactors = FALSE))
        e
      })
    } else {
      resultTry <- tryCatch({
        if (i == 1) {
          siena07(sienaAlgorithm, data = d, effects = effects,
                  returnDeps = TRUE, useCluster = TRUE, nbrNodes = n.clus, initC = TRUE)
        } else {
          if (i %in% c(2, 3, 4)) {
            siena07(sienaAlgorithm, data = d, effects = effects, prevAns = result[[i - 1]],
                    returnDeps = TRUE, useCluster = TRUE, nbrNodes = n.clus, initC = TRUE)
          } else {
            if (i == 5) {
              sienaAlgorithm2 <- sienaAlgorithmCreate(projname = "multiplex_algo", firstg = 0.01, n2start = 1000, n3 = 5000)
              siena07(sienaAlgorithm2, data = d, effects = effects,
                      returnDeps = TRUE, useCluster = TRUE, nbrNodes = n.clus, initC = TRUE)
            } else {
              if (i == 6) {
                sienaAlgorithm2 <- sienaAlgorithmCreate(projname = "multiplex_algo", firstg = 0.005, n2start = 2000, n3 = 10000)
                siena07(sienaAlgorithm2, data = d, effects = effects,
                        returnDeps = TRUE, useCluster = TRUE, nbrNodes = n.clus, initC = TRUE)
              }
            }
          }
        }
      }, error = function(e) {
        # Append a row to errorMessages
        errorMsg <- as.character(e)
        errorMessages <<- rbind(errorMessages, data.frame(modelName = effectsKey, errorMessage = errorMsg, stringsAsFactors = FALSE))
        e
      })
    }
    
    # Check for errors and convergence
    if (inherits(resultTry, "error")) {
      cat("Error in estimating the model for", effectsKey, "\n")
    } else {
      result[[i]] <- resultTry
      converged <- convergenceOk(result[[i]])
      estimated <- estimatesOk(result[[i]])
      
      # Check for NA and set to FALSE if NA
      if (is.na(converged)) {
        converged <- FALSE
      }
      if (is.na(estimated)) {
        estimated <- FALSE
      }
      if (converged && estimated) {
        return(result[[i]])
      } else {
        cat("Convergence issues in estimating the model for", effectsKey, "\n")
        if (!estimated && i == 1 && maxAttempts > 4 && !skipLogic) {
          skipNext <- 3  # Skip the next three iterations
        }
      }
    }
  }
  if (i >= maxAttempts) {
    return(result[[maxAttempts]])
  } else {
    cat("Unable to retrieve the result for attempt", maxAttempts, "for", effectsKey, "\n")
    return(NULL)
  }
}

# Initialize an empty list to store the results
result <- list()

# Loop through each data set in sienaData
set.seed(1)
for (dataKey in names(sienaData)) {
  # Find matching effects keys from sienaEffects
  matchingEffectsKeys <- grep(dataKey, names(sienaEffects), value = TRUE)
  for (effectsKey in matchingEffectsKeys) {
    cat("Estimating model for:", effectsKey, "\n")
    # Call the estimateModel function and store the result
    resultEntry <- tryCatch({
      estimateModel(dataKey, effectsKey)
    }, error = function(e) {
      # Append a row to errorMessages
      errorMsg <- as.character(e)
      errorMessages <<- rbind(errorMessages, data.frame(modelName = effectsKey, errorMessage = errorMsg, stringsAsFactors = FALSE))
      cat("Error encountered:", errorMsg, "\n")
      NULL
    })
    # Store the result
    result[[effectsKey]] <- resultEntry
  }
}
