#' ---
#' title: "Relational dynamics in bullying, victimization, and defending perceptions"
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

# Install and load required libraries
list.of.packages <- c("tidyverse", "magrittr", "psych", "openxlsx", "RSiena", "metafor", "lavaan", "parallel", "purrr", "haven", "readr", "writexl", "sna")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#+ include = FALSE
lapply(list.of.packages, require, quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)

# rm(list = ls())
startTime <- Sys.time()
# Load the workspace from the file
# load("resultsObjects.RData")

# Test run involving only n = nClassBeta classes
betaTesting <- FALSE
nClassBeta <- 3

# Should Healthy Context paradox be modeled?
healthyContext <- FALSE

# Define moderators for the meta-regression analysis
if(healthyContext == TRUE){
  mods <- c("victimClassProp")  
} else {
  mods <- c("bCMD_Latent", "bnc1_Latent")  
}

# Define eligible models for each classroom
if(healthyContext == TRUE){
  reducedModel <- as.character(c("0401", "0402", "0501", "0601", "0602", "0603", "0701", "0801", "0901", "0902", "1002", "1003", "1004", "1101", "1201", "1202", "1301", "1302", "1401", "1501", "1502", "1601", "1701", "1702", "1703", "1801", "1901", "1902", "2002"))
  fullModel <- as.character(c("0401", "0402", "0501", "0601", "0602", "0603", "0701", "0901", "1002", "1003", "1201", "1202", "1302", "1401", "1501", "1502", "1601", "1701", "1703", "1901", "2002"))
} else {
  baselineModel <- as.character(c("0101", "0201", "0401", "0402", "0501", "0602", "0603", "0701", "0702", "0901", "1002", "1003", "1101", "1201", "1202", "1302", "1401", "1502", "1602", "1702", "1703", "1801", "1901", "1902", "1903", "2001", "2002"))
  reducedModel <- as.character(c("0401", "0402", "0501", "0602", "0603", "0701", "0702", "1101", "1201", "1202", "1302", "1502", "1602", "1703", "1901", "2002"))
  fullModel <- as.character(c("0401", "0402", "0501", "0602", "1101", "1302", "1502", "1901"))
}

# Settings ----------------------------------------------------------------
# Should models with with problematic convergence or excessive estimates be filtered out before the synthesis?
filterModelsSetting <- TRUE

# Multicore setting
n.clus <- detectCores() - 1

# Setting for the verbose and batch arguments
verboseOn <- FALSE
batchOn <- TRUE

# Number of iterations and firstg parameter setting
if(healthyContext == TRUE){
  firstgSetting <- .01
  iterN2 <- 500 #1000
  iterN3 <- 5000 #10000
} else {
  firstgSetting <- .05
  iterN2 <- 500 #1000
  iterN3 <- 5000 #10000
}

# Threshold check for estimates that are way out
estimateThreshold <-  50 # Defaults to 20, set to Inf to turn off estimate check

# What is the maximum number of attempts in estimating the Siena model (if there are convergence issues)?
maxAttempt <- 12 # Should take values 5 to 6 if skippingLogic == TRUE. Set to 6 for maximum no of attempts including 2 "overkill" models. Set 4 to not estimate the "overkill" models. Set to 5 for a more optimal tradeoff between convergence and computation time.

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

# Define the list of variables for each factor
variableList <- list(
  cmd = c("bCMD_1", "bCMD_2", "bCMD_3", "bCMD_4", "bCMD_5", "bCMD_6", "bCMD_7", "bCMD_8", "bCMD_9", "bCMD_10", "bCMD_11", "bCMD_12", "bCMD_13", "bCMD_14", "bCMD_15", "bCMD_16", "bCMD_17"),
  bnc = c("bnc1", "bnc2", "bnc3"),
  detachmentA = c("aas1", "aas2R", "aas3", "aas4"),
  detachmentB = c("bas1", "bas2R", "bas3", "bas4")
)

# For scales within the dataWide dataset, compute factor or principal component score
cfaResults <- cfaFunction(data = dataWide, variableList = variableList, boot = nBoot, fitMeasures = fitMea)
dataWide <<- cfaResults$data

# Get all csv files in the directory
folder_path <- "matrices/"
all_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

# Unique classroom ids
classroom_ids <- unique(substr(basename(all_files), 1, 4))

# Match the sheet names and corresponding files
sheet_map <- list(
  "friendshipT1" = "01-output-an1", 
  "friendshipT2" = "02-output-bn1", 
  "exclusionT1" = "01-output-an3", 
  "exclusionT2" = "02-output-bn3", 
  "bullyT1" = "01-output-ar1", 
  "bullyT2" = "02-output-br1", 
  "victimT1" = "01-output-ar4", 
  "victimT2" = "02-output-br4", 
  "defenderT1" = "01-output-ar7", 
  "defenderT2" = "02-output-br7"
)

# List of sheet names for recoding
recode_sheets <- c("bullyT1", "bullyT2", "victimT1", "victimT2", "defenderT1", "defenderT2")

# Loop over each unique classroom id
for (classroom_id in classroom_ids) {
  
  # Loop over each element in the sheet map
  df_list <- list()
  ref_column <- NULL
  for (sheet_name in names(sheet_map)) {
    
    # Construct the file name
    file_name <- paste0(classroom_id, sheet_map[[sheet_name]], ".csv")
    
    # Find the file in the directory
    file_path <- Sys.glob(file.path(folder_path, file_name))
    
    # If file exists, read the csv and store in the list
    if (length(file_path) > 0) {
      
      # Read the csv file
      df <- as.matrix(read_csv(file_path[1]))
      
      # For the first sheet, store the first column as reference
      if (is.null(ref_column)) {
        ref_column <- df[,1]
        colnames(df)[1] <- "id"
      } else {
        # Replace the first column with the reference column in other sheets
        df[,1] <- ref_column
        colnames(df) <- c("id", ref_column)
      }
      
      # Replace -1, 10, -9 with NAs
      df <- as.data.frame(df)
      df <- df %>%
        mutate(across(everything(), ~na_if(., -1))) %>%
        mutate(across(everything(), ~na_if(., 10))) %>%
        mutate(across(everything(), ~na_if(., -9)))
      
      # Do the recoding for sheets that need it
      if (sheet_name %in% recode_sheets) {
        df <- df %>%
          mutate(across(everything(), ~ ifelse(. == 1, 0, ifelse(. %in% c(2, 3), 1, .))))
      }
      
      # Store the data frame in the list
      df_list[[sheet_name]] <- df
    }
  }
  
  # Create the attributes sheet
  if ("friendshipT1" %in% names(df_list)) {
    attributes_df <- data.frame(id = as.numeric(unlist(df_list[["friendshipT1"]][,1])))
    attributes_df <- left_join(attributes_df, dataWide, by = c("id" = "aid")) %>%
      select(id, gender = sex, detachmentT1 = aas1_Latent, detachmentT2 = bas1_Latent)
    # Add the attributes sheet to the beginning of the list
    df_list <- append(list("attributes" = attributes_df), df_list)
  }
  
  # Write the data frames to an xlsx file
  if (length(df_list) > 0) {
    write_xlsx(df_list, path = file.path("data/", paste0(classroom_id, ".xlsx")))
  }
}

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

# Reduce the list length if betaTesting is TRUE
if (betaTesting == TRUE) {
  fullDat <- dat
  if(healthyContext == TRUE){
    definedClasses <- unique(c(reducedModel, fullModel))
  } else {
    definedClasses <- unique(c(baselineModel, reducedModel, fullModel))
  }
  dat <- dat[paste0("c", definedClasses, sep = "")]
  
  if (length(dat) >= nClassBeta) {
    dat <- dat[1:nClassBeta]
  }
}

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

# Create RSiena algorithms
sienaAlgorithm <- sienaAlgorithmCreate(projname = "multiplex_algo", firstg = firstgSetting, n2start = iterN2, n3 = iterN3)
sienaAlgorithmHCP <- sienaAlgorithmCreate(projname = "multiplex_algo", firstg = 0.001, n2start = 1000, n3 = 5000)

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
    if (skipNext > 0 && skipLogic) {  # Only skip if skipLogic is TRUE
      skipNext <- skipNext - 1
      next  # Skip this iteration
    }
    
    # Estimate the model
    if(healthyContext == TRUE){
      resultTry <- tryCatch({
        if (i == 1) {
          siena07(sienaAlgorithm, data = d, effects = effects,
                  returnDeps = TRUE, useCluster = TRUE, nbrNodes = n.clus, initC = TRUE, batch = batchOn, verbose = verboseOn, )
        } else {
          if (i %in% 2:5) {
            siena07(sienaAlgorithm, data = d, effects = effects, prevAns = result[[i - 1]],
                    returnDeps = TRUE, useCluster = TRUE, nbrNodes = n.clus, initC = TRUE, batch = batchOn, verbose = verboseOn)
          } else { 
            if(i == 6){
              siena07(sienaAlgorithmHCP, data = d, effects = effects,
                      returnDeps = TRUE, useCluster = TRUE, nbrNodes = n.clus, initC = TRUE, batch = batchOn, verbose = verboseOn)
            } else { 
              if(i %in% 7:10){
                siena07(sienaAlgorithmHCP, data = d, effects = effects, prevAns = result[[i - 1]],
                        returnDeps = TRUE, useCluster = TRUE, nbrNodes = n.clus, initC = TRUE, batch = batchOn, verbose = verboseOn)
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
    } else {
      resultTry <- tryCatch({
        if (i == 1) {
          siena07(sienaAlgorithm, data = d, effects = effects,
                  returnDeps = TRUE, useCluster = TRUE, nbrNodes = n.clus, initC = TRUE, batch = batchOn, verbose = verboseOn)
        } else {
          if (i %in% c(2, 3, 4)) {
            siena07(sienaAlgorithm, data = d, effects = effects, prevAns = result[[i - 1]],
                    returnDeps = TRUE, useCluster = TRUE, nbrNodes = n.clus, initC = TRUE, batch = batchOn, verbose = verboseOn)
          } else {
            if (i == 5) {
              sienaAlgorithm2 <- sienaAlgorithmCreate(projname = "multiplex_algo", firstg = 0.01, n2start = 1000, n3 = 5000)
              siena07(sienaAlgorithm2, data = d, effects = effects,
                      returnDeps = TRUE, useCluster = TRUE, nbrNodes = n.clus, initC = TRUE, batch = batchOn, verbose = verboseOn)
            } else {
              if (i == 6) {
                sienaAlgorithm2 <- sienaAlgorithmCreate(projname = "multiplex_algo", firstg = 0.005, n2start = 2000, n3 = 10000)
                siena07(sienaAlgorithm2, data = d, effects = effects,
                        returnDeps = TRUE, useCluster = TRUE, nbrNodes = n.clus, initC = TRUE, batch = batchOn, verbose = verboseOn)
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

# List of networks
networks <- c("friendDep", "exclDep")

# List of GoF measures to calculate
auxilliaryFuns <- list("IndegreeDist" = IndegreeDistribution, 
                       "OutdegreeDist" = OutdegreeDistribution, 
                       "GeodesicDist" = GeodesicDistribution, 
                       "TriadCensus" = TriadCensus)

# Calculate GoF for each dataset and each network
gofList <- lapply(names(result), function(name) {
  res <- result[[name]]
  if (is.null(res)) return(NULL)
  
  sapply(networks, function(net) {
    netGof <- lapply(names(auxilliaryFuns), function(gofName) {
      tryCatch({
        setNames(list(sienaGOF(res, verbose = TRUE, varName = net, auxilliaryFuns[[gofName]])), gofName)
      }, error = function(e) {
        message(paste("Error in sienaGOF for", gofName, ":", e$message))
        return(NULL)  # Return NULL or some other value to indicate failure
      })
    })
    unlist(netGof, recursive = FALSE)
  }, simplify = FALSE)
})
names(gofList) <- names(result)

# Filter out result objects with convergence issues or excessive estimates (or SEs)
if(filterModelsSetting == TRUE){
  resultsFull <- result
  result <- filterModels(result) 
}

# Create the result table for a set of filtered and unfiltered network objects
createResultTable <- function(res) {
  if (is.null(res)) return(data.frame(effectName = NA, estMeans = NA, se = NA, tstat = NA))
  data.frame(effectName = res$effects$effectName, estMeans = res$theta, se = res$se, tstat = res$tstat)
}
resultTable <- lapply(result, createResultTable)
resultTableAll <- lapply(resultsFull, createResultTable)
names(resultTable) <- names(result)
names(resultTableAll) <- names(resultsFull)

# Combine the data frames in resultTable into one data frame
allResults <- do.call(rbind, lapply(seq_along(resultTable), function(i) {
  cbind(class = names(resultTable)[i], resultTable[[i]])
}))

# Split the combined data frame by effectName
effectsList <- split(allResults, allResults$effectName)

# Create a list of plots for each GoF measure of each network of each dataset
plotList <- lapply(names(gofList), function(dataset) {
  datasetPlots <- lapply(names(gofList[[dataset]]), function(network) {
    netGofPlots <- lapply(names(gofList[[dataset]][[network]]), function(auxFun) {
      measureGof <- gofList[[dataset]][[network]][[auxFun]]
      plot(measureGof, scale = TRUE, center = TRUE, 
           main = paste("Dataset:", dataset, "Network:", network, "Function:", auxFun))
    })
    # Name the elements in netGofPlots
    names(netGofPlots) <- names(gofList[[dataset]][[network]])
    netGofPlots
  })
  # Name the elements in datasetPlots
  names(datasetPlots) <- names(gofList[[dataset]])
  datasetPlots
})
# Name the elements in plotList
names(plotList) <- names(gofList)
plotList

# Meta-analysis -----------------------------------------------------------
# Filter the names of result and resultTable for different models
baselineNames <- grep("baseline$", names(result), value = TRUE)
reducedNames <- grep("reduced$", names(result), value = TRUE)
fullNames <- grep("full$", names(result), value = TRUE)

# Subset the result and resultTable lists
resultTableBaseline <- resultTable[baselineNames]
resultTableReduced <- resultTable[reducedNames]
resultTableFull <- resultTable[fullNames]

# Define a function to run the meta-analysis on each effect
runMetaAnalysis <- function(df, method = "REML", slab = df$class) {
  rmaObject <- rma(yi = df$estMeans, sei = df$se, data = df, method = method, slab = slab)
  return(rmaObject)
}

# Function to perform meta-analysis
metaAnalysisFunction <- function(resultTableSubset) {
  # Combine the data frames in resultTableSubset into one data frame
  allResults <- do.call(rbind, lapply(seq_along(resultTableSubset), function(i) {
    cbind(class = names(resultTableSubset)[i], resultTableSubset[[i]])
  }))
  
  # Split the combined data frame by effectName
  effectsList <- split(allResults, allResults$effectName)
  
  # Apply the function to each effect
  metaResults <- lapply(effectsList, runMetaAnalysis)
  names(metaResults) <- names(effectsList)
  
  # Extract the primary results for each effectName
  primaryResults <- data.frame(
    effect = names(metaResults),
    effectSize = sapply(metaResults, function(x) x$b),
    se = sapply(metaResults, function(x) sqrt(diag(x$vb))),
    z = sapply(metaResults, function(x) x$zval),
    p = sapply(metaResults, function(x) x$pval),
    ciLB = sapply(metaResults, function(x) x$ci.lb),
    ciUB = sapply(metaResults, function(x) x$ci.ub),
    stringsAsFactors = FALSE)
  
  # Return the primary results table
  return(roundDf(primaryResults, decimals))
}

# Perform meta-analyses for each subset and print the results
if(healthyContext == FALSE){
  print("Meta-analysis for Baseline Models")
  (metaResultsBaseline <- metaAnalysisFunction(resultTableBaseline))
}
print("Meta-analysis for Reduced Models")
(metaResultsReduced <- metaAnalysisFunction(resultTableReduced))
print("Meta-analysis for Full Models")
(metaResultsFull <- metaAnalysisFunction(resultTableFull))

# Moderator analyses ------------------------------------------------------
# Define a function to filter data frames within each element of effectsList
filterEffectsList <- function(effectsList, modelType) {
  filteredList <- lapply(effectsList, function(df) {
    df[grep(modelType, df$class), ]
  })
  # Remove elements with zero rows
  filteredList <- filteredList[sapply(filteredList, function(df) nrow(df) > 0)]
  return(filteredList)
}

# Define a wrapper function for the moderator analysis
fitModObjects <- function(effectsList) {
  # Split the dataWide dataset by the variable clid
  splitData <- split(dataWide, dataWide$clid)
  
  # Compute average for each moderator within each split and bind them into a single data frame
  averageList <- lapply(splitData, function(df) {
    sapply(mods, function(mod) mean(df[[mod]], na.rm = TRUE), simplify = "data.frame")
  })
  
  # Convert the list into a data frame and set the clid column
  averageDF <- do.call(rbind, averageList)
  averageDF <- as.data.frame(averageDF) %>% rownames_to_column(var = "clid")
  
  # Pad clid with leading zeros to make it 4 digits long
  averageDF$clid <- sprintf("%04d", as.integer(averageDF$clid))
  
  # Transform and join
  effectsList <- lapply(effectsList, function(df) {
    # Remove 'c' prefix and additional information like '.baseline', '.reduced', '.full'
    df$class <- gsub("c([0-9]+)\\..*", "\\1", df$class)
    
    # Left join to add moderator variables
    df <- left_join(df, select(averageDF, all_of(c("clid", mods))), by = c("class" = "clid"))
    return(df)
  })
  
  # Define a function to run meta-regression for each moderator
  runMetaRegression <- function(df, mod, slab = df$class) {
    result <- tryCatch({
      rma(yi = df[,3], sei = df[,4], mods = df[,mod], data = df, method = "ML", slab = slab)
    }, error = function(e) {
      NULL
    })
    return(result)
  }
  
  # Create an empty list to store the meta-regression models
  modResultObjects <- vector("list", length(mods))
  
  for(i in seq_along(mods)) {
    mod_name <- mods[i]
    modObjects <- lapply(effectsList, function(df) {
      col_index <- match(mod_name, colnames(df))
      if(col_index) {
        modModel <- runMetaRegression(df, col_index)
        return(modModel)
      } else {
        message(paste(mod_name, "not found in dataframe"))
        return(NULL)
      }
    })
    modResultObjects[[i]] <- modObjects
  }
  
  names(modResultObjects) <- mods
  return(modResultObjects)
}

modResultsExtract <- function(effectsList, modObjects) {
  mods <- names(modObjects)  
  splitData <- split(dataWide, dataWide$clid)
  averageList <- lapply(splitData, function(df) {
    sapply(mods, function(mod) mean(df[[mod]], na.rm = TRUE), simplify = "data.frame")
  })
  
  averageDF <- do.call(rbind, averageList)
  averageDF <- as.data.frame(averageDF) %>% rownames_to_column(var = "clid")
  
  averageDF$clid <- sprintf("%04d", as.integer(averageDF$clid))
  
  effectsList <- lapply(effectsList, function(df) {
    df$class <- gsub("c([0-9]+)\\..*", "\\1", df$class)
    df <- left_join(df, select(averageDF, all_of(c("clid", mods))), by = c("class" = "clid"))
    return(df)
  })
  
  moderatorResults <- lapply(seq_along(mods), function(modIdx) {
    do.call(rbind, mapply(function(modModel, df) {
      if (is.null(modModel)) {
        return(c(
          effectSize = NA, SE = NA, LRT = NA, pValue = NA, BF10 = NA, posterior = NA)
        )
      }
      
      # Use the data from df to create nullModel
      nullModel = rma(yi = df[,3], sei = df[,4], method = "ML")  
      fullModel = modModel
      
      c(intercept = fullModel$beta[1],
        effectSize = fullModel$beta[2],
        SE = fullModel$se[2],
        LRT = tryCatch({anova(nullModel, fullModel)$LRT}, error = function(e) { NA }),
        pValue = tryCatch({anova(nullModel, fullModel)$pval}, error = function(e) { NA }),
        BF10 = 1/exp((BIC(fullModel) - BIC(nullModel))/2),
        posterior = 1/exp((BIC(fullModel) - BIC(nullModel))/2)/(1+1/exp((BIC(fullModel) - BIC(nullModel))/2)))
    }, modObjects[[modIdx]], effectsList, SIMPLIFY = FALSE))  # pass effectsList to mapply
  })
  
  names(mods) <- mods
  names(moderatorResults) <- names(mods)
  result <- lapply(moderatorResults, function(x) roundDf(as.data.frame(x), decimals))
  
  return(result)
}

# Filter data frames within each element of effectsList for baseline, reduced, and full
effectsListBaseline <- filterEffectsList(effectsList, "baseline")
effectsListReduced <- filterEffectsList(effectsList, "reduced")
effectsListFull <- filterEffectsList(effectsList, "full")

# Run the moderator analyses for each subset and export meta-regression objects and output tables
modModelsBaseline <- fitModObjects(effectsListBaseline)
modResultsBaseline <- modResultsExtract(effectsListBaseline, modModelsBaseline)
modModelsReduced <- fitModObjects(effectsListReduced)
modResultsReduced <- modResultsExtract(effectsListReduced, modModelsReduced)
modModelsFull <- fitModObjects(effectsListFull)
modResultsFull <- modResultsExtract(effectsListFull, modModelsFull)

# Print the results
cat("Moderator Analysis for Baseline Models\n")
print(modResultsBaseline)

cat("Moderator Analysis for Reduced Models\n")
print(modResultsReduced)

cat("Moderator Analysis for Full Models\n")
print(modResultsFull)

sessionInfoCurrent <- sessionInfo()

# Save the workspace
if(healthyContext == TRUE){
  save.image("resultsObjectsHCP.RData")
} else {
  save.image("resultsObjects.RData")
}
# Store the end time
endTime <- Sys.time()
# Processing time
endTime - startTime

########################################################
# # Generating a list of popularities per each class
# idegree_by_class <- lapply(dat, calculate_class_idegree)
# 
# mat_victim <- list()
# mat_defender <- list()
# mat_bully <- list()
# 
# for (classroom_name in names(idegree_by_class)) {
#   classroom <- idegree_by_class[[classroom_name]]
#   victimT1 <- classroom$victim1
#   defenderT1 <- classroom$defender1
#   bullyT1 <- classroom$bully1
#   mat_victim[[classroom_name]] <- victimT1
#   mat_defender[[classroom_name]] <- defenderT1
#   mat_bully[[classroom_name]] <- bullyT1
# }
# 
# # Creating covariates using 1SD above mean per each victim, bully, defender matrix
# 
# medianSwitch <- TRUE # Select TRUE if you want median instead of mean; FALSE if you want mean value
# 
# # Function to create fourth quartile cutoff
# create_fourth_quartile_cutoff <- function(mat) {
#   combined_data <- unlist(mat)
#   cutoff_value <- quantile(combined_data, 0.75, na.rm = TRUE)
#   return(cutoff_value)
# }
# 
# # Existing list of matrices
# matrices_cutoffs <- list(mat_victim, mat_defender, mat_bully)
# 
# # Apply the function to each matrix in the list
# cutoffs_fourth_quartile <- lapply(matrices_cutoffs, create_fourth_quartile_cutoff)
# names(cutoffs_fourth_quartile) <- c("victim", "defender", "bully")
# 
# # Function to binarize values
# binarize_values <- function(vector1, cutoff) {
#   ifelse(vector1 >= cutoff, 1, 0)
# }
# 
# # Now you can use these new cutoffs_fourth_quartile values in your binarization functions
# binarize_values_vict <- function(vector1) {
#   binarize_values(vector1, cutoffs_fourth_quartile$victim)
# }
# 
# binarize_values_defe <- function(vector1) {
#   binarize_values(vector1, cutoffs_fourth_quartile$defender)
# }
# 
# binarize_values_bully <- function(vector1) {
#   binarize_values(vector1, cutoffs_fourth_quartile$bully)
# }
# 
# mat_victimB <- lapply(mat_victim,binarize_values_vict)
# mat_defenderB <- lapply(mat_defender,binarize_values_defe)
# mat_bullyB <- lapply(mat_bully,binarize_values_bully)
# 
# # just to optically check numbers per classes
# sum_vectors <- function(vector1) {
#   sum_vec <- sum(vector1,na.rm = TRUE)
#   return(sum_vec)
# }
# 
# nr_victims <- lapply(mat_victimB,sum_vectors)
# nr_defenders <- lapply(mat_defenderB,sum_vectors)
# nr_bullies <- lapply(mat_bullyB,sum_vectors)
# 
# class_names <- names(mat_victim)
# 
# victim_victim <- list()
# defender_victim <- list()
# bully_victim <- list()
# 
# for (class_name in class_names) {
#   indegree_victims <- mat_victimB[[class_name]]
#   indegree_defender <- mat_defenderB[[class_name]]
#   indegree_bully <- mat_bullyB[[class_name]]
#   
#   num_students <- length(indegree_victims)
#   new_adj.mat_victimT1 <- matrix(0, nrow = num_students, ncol = num_students)
#   new_adj.mat_defenderT1 <- matrix(0, nrow = num_students, ncol = num_students)
#   new_adj.mat_bullyT1 <- matrix(0, nrow = num_students, ncol = num_students)
#   
#   for (i in 1:num_students) {
#     for (j in 1:num_students) {
#       if (i != j) {
#         new_adj.mat_victimT1[i, j] <- indegree_victims[i] * indegree_victims[j]
#         new_adj.mat_defenderT1[i, j] <- indegree_defender[i] * indegree_victims[j]
#         new_adj.mat_bullyT1[i, j] <- indegree_bully[i] * indegree_victims[j]
#         
#         diag(new_adj.mat_victimT1) <- 0
#         diag(new_adj.mat_defenderT1) <- 0
#         diag(new_adj.mat_bullyT1) <- 0
#         
#         victim_victim[[class_name]] <- new_adj.mat_victimT1
#         defender_victim[[class_name]] <- new_adj.mat_defenderT1
#         bully_victim[[class_name]] <- new_adj.mat_bullyT1
#       }
#     }
#   }
# }
