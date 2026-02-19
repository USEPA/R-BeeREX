## This file provides the functions that support calculations within the R-BeeREX Shiny Application

#### EEC Calculations ####
## All functions are created to take only the exposure_chem_data data.frame() as an input
## Any function parameters, (Kenega, Briggs) are provided as defaults to the functions that are use, but can be optionally modified

## The format of exposure_chem_data is here:
# > str(exposure_chem.out)
# tibble [33 × 9] (S3: tbl_df/tbl/data.frame)
# $ index               : num [1:33] 1 2 3 4 5 6 7 8 9 10 ...
# $ use_site            : chr [1:33] "Artichoke, Globe" "Artichoke, Globe" "Chickpea" "Blackberry" ...
# $ max_application_rate: num [1:33] 1.5 1.5 0.25 1 1 0.5 0.25 0.5 0.5 0.5 ...
# $ application_type    : chr [1:33] "Spray, Granular" "Spray" "Spray" "Seed" ...
# $ soil_or_foliar      : chr [1:33] "Foliar and Soil" "Soil" NA NA ...
# $ seed_treatment_rate : chr [1:33] NA NA NA "1" ...
# $ udl_type            : chr [1:33] "Vegetable fresh or processing market" "Vegetable fresh or processing market" "Vegetable commodity" "Small fruit trellised" ...
# $ koc                 : num [1:33] NA NA NA NA NA NA NA NA NA NA ...
# $ log_kow             : num [1:33] NA NA NA NA NA NA NA NA NA NA ...
# > 


foliarToEEC<-function(exposure_chem_data,fs_a=110,fs_b=98){
  fsEECOut<-fs_a*exposure_chem_data$max_application_rate
  return(fsEECOut)
}
soilToEEC<-function(exposure_chem_data,sa_a=0.95,sa_b=2.05,sa_c=0.82,sa_d=0.0648,sa_e=0.2431,sa_f=0.5822,sa_g=1.5,sa_h=0.2,sa_i=1.5,sa_j=0.01,sa_k=0.5,sa_l=0.45){
  if(exposure_chem_data$log_kow > 5){
    return(NA)
  } else {
    sAEECPreUnits<-(10^(sa_a*exposure_chem_data$log_kow-sa_b)+sa_c)*(-sa_d*(exposure_chem_data$log_kow^2)+sa_e*exposure_chem_data$log_kow+sa_f)*(sa_g/(sa_h+sa_i*exposure_chem_data$koc*sa_j))
    sAEECOut<-sAEECPreUnits*sa_k*exposure_chem_data$max_application_rate
    return(sAEECOut)}
}
soilSprayToEEC<-function(exposure_chem_data){
  sAEECOut<-soilToEEC(exposure_chem_data)
  fsEECOut<-foliarToEEC(exposure_chem_data)
  return(c(fsEECOut,sAEECOut))
}
seedTreatmentToEEC<-function(exposure_chem_data,pollen_or_nectar,st_pollen=1.8,st_nectar=4.5){
  if(pollen_or_nectar=="nectar") {sTEECout<-as.numeric(exposure_chem_data$seed_treatment_rate)*0.1*st_nectar}
  else if(pollen_or_nectar=="pollen"){sTEECout<-as.numeric(exposure_chem_data$seed_treatment_rate)*0.1*st_pollen}
  return(sTEECout)
}



#exposure_chem_test<-data.frame(seed_treatment_rate<-c(NA,4),other_data<-c(1,NA))
#seedTreatmentToEEC(exposure_chem_test[1,],pollen_or_nectar = "nectar")


EECs<-function(exposure_chem_data,units="ug a.i./mg"){
  #append empty EEC Rows
  eec_cols<-data.frame("foliar_eec"=NA,"foliar_popcomm_eec"=NA,"soil_eec"=NA,"seed_nectar_eec"=NA,"seed_pollen_eec"=NA)
  exposure_chem_eecs<-cbind(exposure_chem_data,eec_cols)
  for(i in 1:nrow(exposure_chem_eecs)){
    unitfactor<-ifelse(units=="ug a.i./mg",1/1000,1)
    exposure_chem_eecs[i,]$foliar_eec<-foliarToEEC(exposure_chem_eecs[i,])*unitfactor
    exposure_chem_eecs[i,]$foliar_popcomm_eec<-foliarToEEC(exposure_chem_eecs[i,],fs_a=36)*unitfactor
    exposure_chem_eecs[i,]$soil_eec<-soilToEEC(exposure_chem_eecs[i,])*unitfactor
    
    exposure_chem_eecs[i,]$seed_nectar_eec<-seedTreatmentToEEC(exposure_chem_eecs[i,],pollen_or_nectar="nectar")*unitfactor
    exposure_chem_eecs[i,]$seed_pollen_eec<-seedTreatmentToEEC(exposure_chem_eecs[i,],pollen_or_nectar="pollen")*unitfactor
  }
  return(exposure_chem_eecs)
}


#### Total Dose, RQs, MODs ####
## This function takes the species toxicity data and the exposure_chem_eec data and calculates Total Dose, RQs, and MODs

## The structure of the exposure_chem_eecs input data.frame is here:
# > str(exposure_chem_eecs.out)
# 'data.frame':	33 obs. of  12 variables:
# $ index               : num  1 2 3 4 5 6 7 8 9 10 ...
# $ use_site            : chr  "Artichoke, Globe" "Artichoke, Globe" "Chickpea" "Blackberry" ...
# $ max_application_rate: num  1.5 1.5 0.25 1 1 0.5 0.25 0.5 0.5 0.5 ...
# $ application_type    : chr  "Spray, Granular" "Spray" "Spray" "Seed" ...
# $ soil_or_foliar      : chr  "Foliar and Soil" "Soil" NA NA ...
# $ seed_treatment_rate : chr  NA NA NA "1" ...
# $ udl_type            : chr  "Vegetable fresh or processing market" "Vegetable fresh or processing market" "Vegetable commodity" "Small fruit trellised" ...
# $ koc                 : num  NA NA NA NA NA NA NA NA NA NA ...
# $ log_kow             : num  NA NA NA NA NA NA NA NA NA NA ...
# $ foliar_eec          : num  0.165 0.165 0.0275 0.11 0.11 0.055 0.0275 0.055 0.055 0.055 ...
# $ soil_eec            : num  NA NA NA NA NA NA NA NA NA NA ...
# $ seed_eec            : num  NA NA NA 0.001 NA NA NA NA NA NA ...
# > 

## The structure of the species_toxicity input data.frame is here:
# > str(species_toxicity.out)
# Classes ‘data.table’ and 'data.frame':	41 obs. of  13 variables:
# $ common_name           : chr  "Bumble bee" "Bumble bee" "Bumble bee" "Bumble bee" ...
# $ life_stage            : chr  "Adult" "Adult" "Adult" "Adult" ...
# $ caste_or_task         : chr  "Worker" "Worker" "Worker" "Worker" ...
# $ age                   : chr  "0-10" "0-10" "0-10" "0-10" ...
# $ jelly                 : num  0 0 0 0 0 0 0 0 0 0 ...
# $ nectar                : num  372 372 372 372 372 372 372 372 60 140 ...
# $ pollen                : num  30 30 30 30 30 30 30 30 23 9.6 ...
# $ level_of_organization : chr  "non-listed" "non-listed" "non-listed" "listed individual" ...
# $ acute_subacute_chronic: chr  "acute" "chronic" "chronic" "acute" ...
# $ exposure_route        : chr  "oral" "oral" "oral" "oral" ...
# $ dose_based            : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
# $ endpoint_type         : chr  "lc50" "noaec" "loaec" "lc50" ...
# $ value                 : num  20 5 10 20 5 ...
# - attr(*, ".internal.selfref")=<externalptr> 
#   - attr(*, "sorted")= chr [1:2] "common_name" "life_stage"
# >




## The function but with a progress indicator 
totalDoseRQsMODs <- function(species_toxicity_data, exposure_chem_eecs_data, progress = NULL) {
  # Build empty df for storing calculated values
  speciesOutList <- list()
  use_names <- c()
  td_rq_mod <- data.frame("td_foliar" = numeric(),"td_soil"=numeric(),"td_seed"=numeric(), "rq_mod_foliar" = numeric(),"rq_mod_soil" = numeric(),"rq_mod_seed" = numeric())
  species_toxicity_td_rq <- cbind(species_toxicity_data, td_rq_mod)
  
  # Calculate total steps for progress
  total_steps <- nrow(exposure_chem_eecs_data) * nrow(species_toxicity_td_rq)
  current_step <- 0
  
  for (i in 1:nrow(exposure_chem_eecs_data)) {
    use_names[i] <- paste(exposure_chem_eecs_data$index[i], exposure_chem_eecs_data$use_site[i])
    
    for (j in 1:nrow(species_toxicity_td_rq)) {
      current_step <- current_step + 1
      if (!is.null(progress)) {
        progress(current_step / total_steps)
      }
      
      species_toxicity_td_rq[j,]$td_foliar<-
        if(is.na(species_toxicity_td_rq[j,]$exposure_route)){
          NA
        } else if(species_toxicity_td_rq[j,]$exposure_route=="contact"){
          2.7*exposure_chem_eecs_data[i,]$max_application_rate
        } else {
          switch(species_toxicity_td_rq[j,]$level_of_organization,
                 "non-listed"=exposure_chem_eecs_data[i,]$foliar_eec*(species_toxicity_td_rq[j,]$pollen+species_toxicity_td_rq[j,]$nectar+(species_toxicity_td_rq[j,]$jelly/100)),
                 "listed individual"=exposure_chem_eecs_data[i,]$foliar_eec*(species_toxicity_td_rq[j,]$pollen+species_toxicity_td_rq[j,]$nectar+(species_toxicity_td_rq[j,]$jelly/100)),
                 "listed population"=exposure_chem_eecs_data[i,]$foliar_popcomm_eec*(species_toxicity_td_rq[j,]$pollen+species_toxicity_td_rq[j,]$nectar+(species_toxicity_td_rq[j,]$jelly/100)),
                 "listed community"=exposure_chem_eecs_data[i,]$foliar_popcomm_eec*(species_toxicity_td_rq[j,]$pollen+species_toxicity_td_rq[j,]$nectar+(species_toxicity_td_rq[j,]$jelly/100)))
        }
      species_toxicity_td_rq[j,]$td_soil<-exposure_chem_eecs_data[i,]$soil_eec*(species_toxicity_td_rq[j,]$pollen+species_toxicity_td_rq[j,]$nectar+(species_toxicity_td_rq[j,]$jelly/100))
      species_toxicity_td_rq[j,]$td_seed<-(exposure_chem_eecs_data[i,]$seed_pollen_eec*species_toxicity_td_rq[j,]$pollen)+(exposure_chem_eecs_data[i,]$seed_nectar_eec*species_toxicity_td_rq[j,]$nectar)
      
      species_toxicity_td_rq[j,]$rq_mod_foliar<-
        if(any(is.na(c(species_toxicity_td_rq[j,]$td_foliar,species_toxicity_td_rq[j,]$value)))){
          NA} else {species_toxicity_td_rq[j,]$td_foliar/species_toxicity_td_rq[j,]$value
          }
      species_toxicity_td_rq[j,]$rq_mod_soil<-if(any(is.na(c(species_toxicity_td_rq[j,]$td_soil,species_toxicity_td_rq[j,]$value)))){
        NA} else {species_toxicity_td_rq[j,]$td_soil/species_toxicity_td_rq[j,]$value
        }
      species_toxicity_td_rq[j,]$rq_mod_seed<-if(any(is.na(c(species_toxicity_td_rq[j,]$td_seed,species_toxicity_td_rq[j,]$value)))){
        NA} else {species_toxicity_td_rq[j,]$td_seed/species_toxicity_td_rq[j,]$value
        }
    }
    
    speciesOutList[[use_names[i]]] <- species_toxicity_td_rq
  }
  
  return(speciesOutList)
}



## SIMPLE SUMMARIZE FUNCTION FOR species_exposure data
## Produces, for each use scenario, a list with values indexed by species and by level of organization

library(data.table)

# Define the function
summarize_data <- function(data) {
  # Convert to data.table if not already
  if (!is.data.table(data)) {
    data <- as.data.table(data)
  }
  
  # Summarize the data
  summary <- data[!is.na(rq_mod_foliar) | !is.na(rq_mod_soil) | !is.na(rq_mod_seed),
                  .(
                    rq_mod_foliar_max = max(rq_mod_foliar, na.rm = TRUE),
                    rq_mod_soil_max = max(rq_mod_soil, na.rm = TRUE),
                    rq_mod_seed_max = max(rq_mod_seed, na.rm = TRUE)
                  ),
                  by = .(level_of_organization, common_name, life_stage, acute_subacute_chronic, exposure_route)]
  
  # Create a nested list to store tables
  nested_list <- list()
  
  # Unique common names
  unique_common_names <- unique(summary$common_name)
  
  # Loop over each unique common name
  for (name in unique_common_names) {
    # Filter summary for this common name
    common_name_summary <- summary[common_name == name]
    
    # Create a list for each level of organization within this common name
    common_name_list <- list()
    
    # Unique levels of organization for this common name
    unique_levels <- unique(common_name_summary$level_of_organization)
    
    # Loop over each unique level of organization
    for (org in unique_levels) {
      # Subset the summary table for each level of organization
      subset_table <- common_name_summary[level_of_organization == org]
      
      # Store in the list with the level of organization as the key
      common_name_list[[org]] <- subset_table
    }
    
    # Store the list for this common name
    nested_list[[name]] <- common_name_list
  }
  
  return(nested_list)
}

summarizeDataAll <- function(species_exposure_data) {
  use_names <- names(species_exposure_data)
  resultsAll <- list()
  withProgress(message = 'Summarizing Results...', value = 0, {
    for (i in 1:length(species_exposure_data)) {
      name <- use_names[i]
      resultsAll[[name]] <- summarize_data(species_exposure_data[[i]])
      incProgress(1 / length(species_exposure_data))
    }
  })
  return(resultsAll)
}


flattenResultsDF <- function(df) {
  # Check if the data frame has the expected columns
  required_columns <- c("level_of_organization", "common_name", "life_stage",
                        "acute_subacute_chronic", "exposure_route",
                        "rq_mod_foliar_max", "rq_mod_soil_max", "rq_mod_seed_max")
  
  if (!all(required_columns %in% names(df))) {
    stop("Data frame does not have the required columns.")
  }
  
  # Initialize an empty named vector
  result <- c()
  
  # Iterate over each row of the data frame
  for (i in 1:nrow(df)) {
    # Create the label based on the current row
    label_base <- paste(df$common_name[i], df$level_of_organization[i], df$life_stage[i],
                        df$acute_subacute_chronic[i], df$exposure_route[i],
                        sep = ".")
    
    # Add the foliar_max value with the appropriate label
    foliar_label <- paste(label_base, "foliar_max", sep = ".")
    result[foliar_label] <- df$rq_mod_foliar_max[i]
    
    # Add the soil_max value with the appropriate label
    soil_label <- paste(label_base, "soil_max", sep = ".")
    result[soil_label] <- df$rq_mod_soil_max[i]
    
    # Add the seed_max value with the appropriate label
    seed_label <- paste(label_base, "seed_max", sep = ".")
    result[seed_label] <- df$rq_mod_seed_max[i]
  }
  
  return(result)
}

flattenNamedVectors <- function(list_of_vectors) {
  # Gather all names, treating NA as empty strings first
  all_names_raw <- unlist(lapply(list_of_vectors, function(x) {
    nm <- names(x)
    nm[is.na(nm)] <- ""
    nm
  }))
  all_names_unique <- unique(all_names_raw)
  
  # Repair names: replace empty with 'unknown', sanitize, ensure uniqueness
  repair_names <- function(nm) {
    nm <- as.character(nm)
    nm[nm == "" | is.na(nm)] <- "unknown"
    nm <- gsub("[^A-Za-z0-9._]+", "_", nm)
    nm <- gsub("^_+|_+$", "", nm)
    make.unique(nm, sep = "..dup")
  }
  repaired_all <- repair_names(all_names_unique)
  
  # Map original -> repaired
  name_map <- setNames(repaired_all, all_names_unique)
  
  # Function to convert vectors to named lists with all repaired columns
  convert_to_named_list <- function(vec, all_repaired, name_map) {
    vnm <- names(vec)
    vnm[is.na(vnm)] <- ""
    vnm <- name_map[vnm]
    names(vec) <- vnm
    # Build list with all repaired names
    named_list <- setNames(as.list(rep(NA_real_, length(all_repaired))), all_repaired)
    # Fill in values (coerce to list to preserve types)
    named_list[names(vec)] <- as.list(vec)
    named_list
  }
  
  named_list <- lapply(list_of_vectors, convert_to_named_list, all_repaired = repaired_all, name_map = name_map)
  # Build the data.frame; keep names as-is to avoid any repair messages
  flattened_df <- do.call(rbind, lapply(named_list, function(x) as.data.frame(x, check.names = FALSE)))
  
  return(flattened_df)
}

flattenResultsList<-function(results_by_scenario_data){
  use_names<-names(results_by_scenario_data)
  useList<-list()
  for(use in 1:length(results_by_scenario_data)){
    name<-use_names[use]
    flatVEC<-c()
    for(species in 1:length(results_by_scenario_data[[use]])){
      for(levels in 1:length(results_by_scenario_data[[use]][[species]])){
        flatDF<-flattenResultsDF(results_by_scenario_data[[use]][[species]][[levels]])
        flatVEC<-c(flatVEC,flatDF)
      }
    }
    useList[[name]]<-flatVEC
  }
  useListOut<-flattenNamedVectors(useList)
  return(useListOut)
}

#test<-flattenResultsList(results_by_scenario.out)
#View(test)


removeExtraApplicationResults <- function(df) {
  # Identify columns to remove based on names containing both "contact" and "seed_max"
  columns_to_remove <- grepl("contact", names(df), ignore.case = TRUE) & grepl("seed_max", names(df), ignore.case = TRUE)
  
  # Remove the identified columns
  df <- df[, !columns_to_remove]
  
  # Iterate over each row in the data frame
  for (i in seq_len(nrow(df))) {
    soil_or_foliar_value <- df[i, "soil_or_foliar"]
    
    if (soil_or_foliar_value == "Foliar Spray") {
      # Set entries ending in "seed_max" to NA - leaving soil_max for indirect effects
      df[i, grepl("seed_max$", names(df))] <- NA
    } else if (soil_or_foliar_value == "Soil Spray") {
      # Set entries ending in "seed_max" to NA
      df[i, grepl("seed_max$", names(df))] <- NA
    } else if (soil_or_foliar_value == "seed") {
      # Set entries ending in "soil_max", "foliar_max", or containing "contact" to NA
      df[i, grepl("soil_max$", names(df)) | grepl("foliar_max$", names(df)) | grepl("contact", names(df), ignore.case = TRUE)] <- NA
    } else if (soil_or_foliar_value == "soil/granular") {
      # Set entries ending in "foliar_max", "seed_max", or containing "contact" to NA
      df[i, grepl("foliar_max$", names(df)) | grepl("seed_max$", names(df)) | grepl("contact", names(df), ignore.case = TRUE)] <- NA
    }
  }
  return(df)
}

cleanResults<-function(results_by_scenario_flat_data,exposure_chem_eecs_data){
  
  appended_flat<-cbind(exposure_chem_eecs_data,results_by_scenario_flat_data)
  clean_results<-removeExtraApplicationResults(appended_flat)
  
  return(clean_results)
}


## Helper function, called within formatResultsExport()
## This function takes species_exposure_data and exposure_chem_eecs_data and converts it into a list of, indexed by use site index + name, that contains all of the rq_mod_*_max values, filtered by the application type, and the exposure route
## The result is a list of data.frames with rq_mod_*_max values for all classes of bees and endpoints included in the model
summarize_rq_list <- function(species_exposure_data, exposure_chem_eecs_data, eecs_index_col = "index") {
  # Validate species_exposure_data
  if (!is.list(species_exposure_data) || is.null(names(species_exposure_data)) || any(names(species_exposure_data) == "")) {
    stop("species_exposure_data must be a named list of data.frames.")
  }
  # Validate exposure_chem_eecs_data and required columns
  if (!is.data.frame(exposure_chem_eecs_data) || !"soil_or_foliar" %in% names(exposure_chem_eecs_data)) {
    stop("exposure_chem_eecs_data must be a data.frame with a 'soil_or_foliar' column.")
  }
  if (!eecs_index_col %in% names(exposure_chem_eecs_data)) {
    stop("exposure_chem_eecs_data must contain the index column '", eecs_index_col, "'.")
  }
  
  # Helper: extract the first integer from a string, or NA if none
  extract_first_int <- function(x) {
    x <- as.character(x)
    m <- regexpr("\\d+", x)
    if (m[1] == -1) return(NA_integer_)
    val <- substr(x, m[1], m[1] + attr(m, "match.length") - 1)
    suppressWarnings(as.integer(val))
  }
  
  # Helper to get numeric index for each row in exposure_chem_eecs_data
  eecs_index_raw <- exposure_chem_eecs_data[[eecs_index_col]]
  eecs_index_num <- if (is.numeric(eecs_index_raw)) {
    as.integer(eecs_index_raw)
  } else {
    vapply(eecs_index_raw, extract_first_int, integer(1))
  }
  
  # Normalize soil_or_foliar to a scenario (case-insensitive, robust)
  normalize_spray_type <- function(x) {
    sx <- tolower(trimws(as.character(x)))
    dplyr::case_when(
      grepl("foliar", sx, fixed = TRUE) ~ "foliar spray",
      grepl("seed",   sx, fixed = TRUE) ~ "seed",
      grepl("soil",   sx, fixed = TRUE) & grepl("granular|/", sx) ~ "soil/granular",
      grepl("soil",   sx, fixed = TRUE) ~ "soil spray",
      TRUE ~ NA_character_
    )
  }
  
  # Define grouping variables and rq column pattern inside the function
  group_vars <- c(
    "common_name",
    "level_of_organization",
    "life_stage",
    "acute_subacute_chronic",
    "exposure_route",
    "endpoint_type"
  )
  rq_pattern <- "^rq_"
  
  # NA-safe max
  fmax <- function(x) if (all(is.na(x))) NA_real_ else suppressWarnings(max(as.numeric(x), na.rm = TRUE))
  
  # Summarize one df using the matched soil_or_foliar via leading number from list element name
  summarize_one_df <- function(df, nm) {
    if (!is.data.frame(df)) stop("All elements of species_exposure_data must be data.frames.")
    
    # Identify rq_* columns and coerce to numeric
    rq_cols <- grep(rq_pattern, names(df), value = TRUE)
    if (length(rq_cols)) {
      df <- df %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(rq_cols), function(x) suppressWarnings(as.numeric(x))))
    }
    
    # Use only grouping vars present in df
    group_vars_present <- intersect(group_vars, names(df))
    
    # Group if any group vars exist
    grouped <- if (length(group_vars_present)) {
      dplyr::group_by(df, dplyr::across(dplyr::all_of(group_vars_present)))
    } else df
    
    # If no rq_* columns, return distinct groups (no rq summaries)
    if (!length(rq_cols)) {
      return(dplyr::distinct(grouped, dplyr::across(dplyr::all_of(group_vars_present))))
    }
    
    # Summarise: max for each rq_* column, wide format
    summarised <- grouped %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(rq_cols),
          list(max = ~ fmax(.)),
          .names = "{.col}__{.fn}"
        ),
        .groups = "drop"
      )
    
    # Extract the first integer from the list element name
    nm_index <- extract_first_int(nm)
    if (is.na(nm_index)) {
      stop("List element name '", nm, "' does not contain a number to match against '", eecs_index_col, "'.")
    }
    
    # Find matching row(s) in exposure_chem_eecs_data by index
    match_idx <- which(eecs_index_num == nm_index)
    if (length(match_idx) == 0) {
      stop("No matching row in exposure_chem_eecs_data where ", eecs_index_col, " == ", nm_index,
           " for list element name '", nm, "'.")
    }
    if (length(match_idx) > 1) {
      warning("Multiple matching rows in exposure_chem_eecs_data for index ", nm_index, ". Using the first match.")
      match_idx <- match_idx[1]
    }
    
    soil_value <- exposure_chem_eecs_data$soil_or_foliar[match_idx]
    soil_missing <- is.na(soil_value) || !nzchar(trimws(as.character(soil_value)))
    
    # If soil_or_foliar is missing/unrecognized, set all rq_mod_*__max columns to NA and return
    if (soil_missing) {
      mod_cols <- grep("^rq_mod.*__max$", names(summarised), value = TRUE)
      if (length(mod_cols)) {
        summarised <- summarised %>%
          dplyr::mutate(dplyr::across(dplyr::all_of(mod_cols), ~ NA_real_))
      }
      return(summarised)
    }
    
    spray_type <- normalize_spray_type(soil_value)
    if (is.na(spray_type)) {
      mod_cols <- grep("^rq_mod.*__max$", names(summarised), value = TRUE)
      if (length(mod_cols)) {
        summarised <- summarised %>%
          dplyr::mutate(dplyr::across(dplyr::all_of(mod_cols), ~ NA_real_))
      }
      return(summarised)
    }
    
    # Operate only on summarised rq_mod*__max columns
    mod_cols <- grep("^rq_mod.*__max$", names(summarised), value = TRUE)
    if (length(mod_cols)) {
      # Determine which mod columns to keep based on spray_type
      keep_regex <- switch(
        spray_type,
        "foliar spray"   = c("^rq_mod_foliar.*__max$","^rq_mod_soil.*__max$"),
        "soil spray"     = c("^rq_mod_foliar.*__max$", "^rq_mod_soil.*__max$"),
        "soil/granular"  = "^rq_mod_soil.*__max$",
        "seed"           = "^rq_mod_seed.*__max$"
      )
      keep_cols <- unique(unlist(lapply(keep_regex, function(re) grep(re, mod_cols, value = TRUE))))
      drop_cols <- setdiff(mod_cols, keep_cols)
      
      # Set non-kept mod columns to NA
      if (length(drop_cols)) {
        summarised <- summarised %>%
          dplyr::mutate(dplyr::across(dplyr::all_of(drop_cols), ~ NA_real_))
      }
    }
    
    # ALWAYS remove contact-based RQs for seed and soil mod columns, regardless of soil_or_foliar
    if ("exposure_route" %in% names(summarised)) {
      contact_rows <- tolower(trimws(as.character(summarised$exposure_route))) == "contact"
      if (any(contact_rows)) {
        seed_cols <- unique(c(
          grep("^rq_mod_seed.*__max$", names(summarised), value = TRUE),
          grep("^rq_mod_seed_max$",   names(summarised), value = TRUE)
        ))
        soil_cols <- unique(c(
          grep("^rq_mod_soil.*__max$", names(summarised), value = TRUE),
          grep("^rq_mod_soil_max$",    names(summarised), value = TRUE)
        ))
        target_cols <- unique(c(seed_cols, soil_cols))
        if (length(target_cols)) {
          for (tc in target_cols) {
            summarised[[tc]] <- ifelse(contact_rows, NA_real_, summarised[[tc]])
          }
        }
      }
    }
    
    summarised
  }
  
  # Apply to each element, using its name's first number to select the right exposure_chem_eecs_data row
  out <- setNames(
    mapply(function(nm, df) summarize_one_df(df, nm),
           names(species_exposure_data),
           species_exposure_data,
           SIMPLIFY = FALSE),
    names(species_exposure_data)
  )
  return(out)
}

## Helper function, called within formatResultsExport()
## This helper function takes data.frames supplied by the summarize_rq_list() function and flattens them into a single vector and 
## it also replaced values with NAs for scenarios that are not appropriate, based on the rq_type specified as the argument
flatten_rq_mod_vector <- function(df, rq_type = c("all", "foliar", "soil", "seed", "all NA")) {
  # Normalize rq_type (case-insensitive), allow "all NA" with space
  rq_type_in <- tolower(trimws(if (length(rq_type) > 0) rq_type[1] else "all"))
  
  # Map synonyms to canonical values
  if (rq_type_in %in% c("foliar spray", "foliar-spray")) rq_type_in <- "foliar"
  
  valid_types <- c("all", "foliar", "soil", "seed", "all na")
  if (!rq_type_in %in% valid_types) {
    stop("rq_type must be one of: 'all', 'foliar' (or 'foliar spray'), 'soil', 'seed', 'all NA'")
  }
  
  # Basic checks
  if (!is.data.frame(df)) stop("df must be a data.frame")
  required_fields <- c(
    "common_name",
    "level_of_organization",
    "life_stage",
    "acute_subacute_chronic",
    "exposure_route",
    "endpoint_type"
  )
  missing <- setdiff(required_fields, names(df))
  if (length(missing)) {
    stop("df is missing required columns: ", paste(missing, collapse = ", "))
  }
  
  n <- nrow(df)
  if (n == 0) {
    # Return empty 0-column data.frame
    return(data.frame(check.names = FALSE))
  }
  
  # Helper: choose primary or alt column, else NA
  get_col <- function(primary, alt) {
    if (primary %in% names(df)) df[[primary]]
    else if (alt %in% names(df)) df[[alt]]
    else rep(NA_real_, n)
  }
  
  # Pull rq columns (support both __max and _max)
  foliar <- suppressWarnings(as.numeric(get_col("rq_mod_foliar__max", "rq_mod_foliar_max")))
  soil   <- suppressWarnings(as.numeric(get_col("rq_mod_soil__max",   "rq_mod_soil_max")))
  seed   <- suppressWarnings(as.numeric(get_col("rq_mod_seed__max",   "rq_mod_seed_max")))
  
  # Sanitizer for name parts: lowercase, trim, non-alnum -> underscore, collapse runs
  # Ensure NA/empty tokens become 'unknown'
  sanitize <- function(x) {
    x <- tolower(trimws(as.character(x)))
    x <- gsub("[^a-z0-9]+", "_", x)
    x <- gsub("^_+|_+$", "", x)
    x[is.na(x) | x == ""] <- "unknown"
    x
  }
  
  # Build base stems for each row
  parts_mat <- cbind(
    sanitize(df$common_name),
    sanitize(df$level_of_organization),
    sanitize(df$life_stage),
    sanitize(df$acute_subacute_chronic),
    sanitize(df$exposure_route),
    sanitize(df$endpoint_type)
  )
  stems <- apply(parts_mat, 1, function(r) paste(r, collapse = "."))
  
  # Column names for each block
  nm_foliar <- paste0(stems, ".rq_mod_foliar_max")
  nm_soil   <- paste0(stems, ".rq_mod_soil_max")
  nm_seed   <- paste0(stems, ".rq_mod_seed_max")
  
  # Assemble full set (always include all three blocks)
  out_names <- c(nm_foliar, nm_soil, nm_seed)
  out_vals  <- c(foliar,    soil,    seed)
  
  # Apply rq_type masking while preserving all columns
  foliar_idx <- seq_len(n)
  soil_idx   <- n + seq_len(n)
  seed_idx   <- 2*n + seq_len(n)
  
  if (rq_type_in == "foliar") {
    # When foliar (or foliar spray) is chosen, include foliar + soil, exclude seed
    out_vals[seed_idx] <- NA_real_
  } else if (rq_type_in == "soil") {
    out_vals[foliar_idx] <- NA_real_
    out_vals[seed_idx]   <- NA_real_
  } else if (rq_type_in == "seed") {
    out_vals[foliar_idx] <- NA_real_
    out_vals[soil_idx]   <- NA_real_
  } else if (rq_type_in == "all na") {
    out_vals[] <- NA_real_
  }
  
  # Return as a one-row data.frame with these columns
  df_out <- as.data.frame(as.list(stats::setNames(as.list(out_vals), out_names)),
                          check.names = FALSE, optional = TRUE)
  df_out
}

collapse_results<- function(df, prefer = c("first", "last"),
                            warn_conflicts = FALSE, return_map = FALSE) {
  stopifnot(is.data.frame(df))
  prefer <- match.arg(prefer)
  
  # Compute base names by stripping the final ".suffix" from each column name
  old_names  <- names(df)
  base_names <- sub("\\.[^.]*$", "", old_names)
  
  # Group column indices by base name (preserve original order of first occurrence)
  idx_groups    <- split(seq_along(base_names), base_names)
  unique_bases  <- unique(base_names)
  
  # Internal helpers
  unify_group_types <- function(cols) {
    cols <- lapply(cols, function(v) if (is.factor(v)) as.character(v) else v)
    all_num <- all(vapply(cols, function(v) is.numeric(v) || is.integer(v) || is.logical(v), logical(1)))
    if (all_num) {
      lapply(cols, function(v) as.numeric(v))
    } else {
      lapply(cols, as.character)
    }
  }
  coalesce_list <- function(cols, prefer) {
    cols <- unify_group_types(cols)
    if (prefer == "last") cols <- rev(cols)
    Reduce(function(a, b) ifelse(!is.na(a), a, b), cols)
  }
  warn_if_conflicts <- function(cols, base_name) {
    as_char <- lapply(unify_group_types(cols), as.character)
    mat <- do.call(cbind, as_char)
    conflict_rows <- apply(mat, 1, function(x) {
      ux <- unique(x[!is.na(x)])
      length(ux) > 1
    })
    n_conf <- sum(conflict_rows, na.rm = TRUE)
    if (n_conf > 0) {
      warning(sprintf("Column group '%s' has %d rows with conflicting non-NA values; using '%s' non-NA per row.",
                      base_name, n_conf, prefer), call. = FALSE)
    }
  }
  
  # Build output columns in order of first appearance of each base
  out_list <- vector("list", length(unique_bases))
  names(out_list) <- unique_bases
  
  # Optional mapping (base name -> original column names)
  map <- lapply(unique_bases, function(b) old_names[idx_groups[[b]]])
  names(map) <- unique_bases
  
  for (i in seq_along(unique_bases)) {
    b   <- unique_bases[i]
    idx <- idx_groups[[b]]
    if (length(idx) == 1) {
      out_list[[i]] <- df[[idx]]
    } else {
      cols <- lapply(idx, function(j) df[[j]])
      if (warn_conflicts) warn_if_conflicts(cols, base_name = b)
      out_list[[i]] <- coalesce_list(cols, prefer = prefer)
    }
  }
  
  res <- as.data.frame(out_list, stringsAsFactors = FALSE, optional = TRUE)
  if (!is.null(row.names(df))) row.names(res) <- row.names(df)
  
  if (return_map) {
    return(list(data = res, map = map))
  } else {
    return(res)
  }
}



## This function creates a list of two data.frames, on_site and off_site. These data.frames contain the organized, summarized data
## from each of the exposure use sites.  It uses the two helper functions summarize_rq_list() and flatten_rq_mod_vector(). 
## The output of this function is then named c('on_site','off_site') and exported to excel. 

formatResultsExport<-function(species_exposure_data,exposure_chem_eecs_data, collapsed=FALSE){
  rq_summary<<-summarize_rq_list(species_exposure_data,exposure_chem_eecs_data)
  flat.rq.na<<-flatten_rq_mod_vector(rq_summary[[1]],rq_type="all NA")
  on.site<<-off.site<-cbind(exposure_chem_eecs_data,flat.rq.na)
  newcols<<-intersect(names(on.site),names(flat.rq.na))
  
  # Helper: normalize soil_or_foliar to scenario labels (consistent with summarize_rq_list)
  normalize_spray_type_fmt <- function(x) {
    sx <- tolower(trimws(as.character(x)))
    if (!nzchar(sx)) return(NA_character_)
    if (grepl("foliar", sx, fixed = TRUE)) return("foliar spray")
    if (grepl("seed",   sx, fixed = TRUE)) return("seed")
    if (grepl("soil",   sx, fixed = TRUE) && grepl("granular|/", sx)) return("soil/granular")
    if (grepl("soil",   sx, fixed = TRUE)) return("soil spray")
    return(NA_character_)
  }
  
  for(i in 1:length(rq_summary)){
    type_raw <- exposure_chem_eecs_data[i,'soil_or_foliar']
    spray_type <- normalize_spray_type_fmt(type_raw)
    
    if (is.na(spray_type)) {
      # Missing or unrecognized: set both on-site and off-site to all NA for this scenario
      flat.rq.all.na <- flatten_rq_mod_vector(rq_summary[[i]], rq_type = "all NA")
      on.site[i,newcols]  <- flat.rq.all.na[1,newcols,drop=FALSE]
      off.site[i,newcols] <- flat.rq.all.na[1,newcols,drop=FALSE]
    } else if (spray_type == "foliar spray") {
      flat.rq<-flatten_rq_mod_vector(rq_summary[[i]],rq_type="foliar")
      on.site[i,newcols]<-flat.rq[1,newcols,drop=FALSE]
      off.site[i,newcols]<-flat.rq[1,newcols,drop=FALSE]
    } else if (spray_type == "soil spray") {
      flat.rq.on<-flatten_rq_mod_vector(rq_summary[[i]],rq_type="soil")
      on.site[i,newcols]<-flat.rq.on[1,newcols,drop=FALSE]
      flat.rq.off<-flatten_rq_mod_vector(rq_summary[[i]],rq_type="foliar")
      off.site[i,newcols]<-flat.rq.off[1,newcols,drop=FALSE]
    } else if (spray_type == "soil/granular") {
      flat.rq.on<-flatten_rq_mod_vector(rq_summary[[i]],rq_type="soil")
      on.site[i,newcols]<-flat.rq.on[1,newcols,drop=FALSE]
      flat.rq.off<-flatten_rq_mod_vector(rq_summary[[i]],rq_type="all NA")
      off.site[i,newcols]<-flat.rq.off[1,newcols,drop=FALSE]
    } else if (spray_type == "seed") {
      flat.rq.on<-flatten_rq_mod_vector(rq_summary[[i]],rq_type="seed")
      on.site[i,newcols]<-flat.rq.on[1,newcols,drop=FALSE]
      flat.rq.off<-flatten_rq_mod_vector(rq_summary[[i]],rq_type="all NA")
      off.site[i,newcols]<-flat.rq.off[1,newcols,drop=FALSE]
    }
  }
  
  if(collapsed==TRUE){
    on.site.collapsed<-collapse_results(on.site)
    off.site.collapsed<-collapse_results(off.site)
    out.list<-list(on.site.collapsed,off.site.collapsed)
  } else {
    out.list<-list(on.site,off.site)
  }
  return(out.list)
  
}


# test<-formatResultsExport(species_exposure.out,exposure_chem_eecs.out)
# names(test[[1]])
# write_xlsx(list(on_site=test[[1]], off_site=test[[2]]), "R-BeeREX-Results.xlsx")