# goodness of fit
# GoF for geodesics distribution requires a function to get GDs from simulations
GeodesicDistribution <- function(i, data, sims, period, groupName,
                                 varName, levls = c(1:5,Inf), cumulative = TRUE, ...) {
  x <- networkExtraction(i, data, sims, period, groupName, varName)
  require(sna)
  a <- sna::geodist(symmetrize(x))$gdist
  if (cumulative)
  {
    gdi <- sapply(levls, function(i){ sum(a <= i) })
  }
  else
  {
    gdi <- sapply(levls, function(i){ sum(a == i) })
  }
  names(gdi) <- as.character(levls)
  gdi
}

# GOF for triad census requires a function to get triads from simulations
TriadCensus <- function(i, data, sims, wave, groupName, varName, levls = 1:16){
  
  unloadNamespace("igraph") # to avoid package clashes
  
  require(sna)
  
  require(network)
  
  x <- networkExtraction(i, data, sims, wave, groupName, varName)
  
  if (network.edgecount(x) <= 0) {x <- symmetrize(x)}
  
  # because else triad.census(x) will lead to an error
  
  tc <- sna::triad.census(x, mode = "digraph")
  
  # triad names are transferred automatically
  
  tc
}

# Round data.frame function
roundDf <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[,nums] <- round(df[,nums], digits = digits)
  (df)
}

# Compute factor scores
cfaFunction <- function(data, variableList, boot, fitMeasures) {
  modelResults <- list()
  factorScoresData <- data.frame(aid = data$aid)
  
  for (variable in variableList) {
    # Extract the base name of the variable (remove the "_1")
    variableName <- strsplit(variable[1], "_1")[[1]][1]
    
    # Check if the column already exists in the factorScoresData data frame
    if (paste0(variableName, "_Latent") %in% names(factorScoresData)) {
      next
    }
    
    if (length(variable) >= 4) {
      # If the variable is comprised of 4 items or more, perform CFA
      modelFormula <- paste0(variable[1], "F =~ ", paste(variable, collapse = " + "))
      
      fit <- cfa(modelFormula, data,
                 std.lv = TRUE, mimic = "Mplus", estimator = "WLSMV", test = "Satterthwaite", se = "robust", ordered = variable, bootstrap = boot)
      
      # Check for missing data and add NAs for these cases when calculating the factor scores
      factorScores <- rep(NA, nrow(data))
      complete_cases <- complete.cases(data[variable])
      factorScores[complete_cases] <- as.numeric(lavPredict(fit))
      
      factorScoresData <- cbind(factorScoresData, setNames(data.frame(factorScores), paste0(variableName, "_Latent")))
      
      modelResults[[variableName]] <- list(
        summary = summary(fit, standardized = TRUE),
        fitMeasures = fitmeasures(fit, fitMea),
        factorScores = factorScores
      )
    } else {
      # If the variable is comprised of less than 4 items, compute PCA score
      pcaResult <- principal(data[variable], nfactors = 1, scores = TRUE)
      pcaScore <- as.numeric(pcaResult$scores)
      factorScoresData <- cbind(factorScoresData, setNames(data.frame(pcaScore), paste0(variableName, "_Latent")))
      
      modelResults[[variableName]] <- list(
        summary = NA,
        fitMeasures = NA,
        factorScores = pcaScore
      )
    }
  }
  
  # Join the original data with the factor scores data
  data <- left_join(data, factorScoresData, by = "aid")
  return(list(data = data, modelResults = modelResults))
}

# Functions for the calculation of popularity
calculate_idegree <- function(adjacency_matrix) {
  idegree <- sna::degree(adjacency_matrix, gmode = "digraph", cmode = "indegree")
  return(idegree)
}

calculate_class_idegree <- function(classroom) {
  classroom_idegree_list <- lapply(classroom, calculate_idegree)
  return(classroom_idegree_list)
}

filterModels <- function(result) {
  # Identify the indices of models that meet the criteria for convergence and estimates
  validIndices <- sapply(1:length(result), function(i) {
    res <- result[[i]]
    if (is.null(res)) {
      return(FALSE)  # Skip NULL results
    }
    convOk <- convergenceOk(res)
    estOk <- estimatesOk(res)
    return(convOk & estOk)
  })
  
  # Subset the result list to include only models that meet the criteria
  resultFiltered <- result[validIndices]
  
  return(resultFiltered)
}
