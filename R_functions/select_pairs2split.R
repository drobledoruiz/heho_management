################################################################################
##          Function to select the pairs to split per neighbourhood           ##
##                                                                            ##
##  Author: Diana Robledo-Ruiz. PhD student, Monash University                ##
##  Date: 2020-04-03                                                          ##
##                                                                            ##
## This function requires:                                                    ##
##   - Input:                                                                 ##
##       - actual  = dataframe of actual pairs of a breeding season           ##
##       - singles = list with two dataframes of single females and males     ##
##       - MSI_df  = MSI dataframe (vertical matrix) for the season           ##
##       - pivot   = pivot table of number of pairs per neighbourhood         ##
##   - User specified parameters:                                             ##
##       - MSI.split = vector (or value) of MSI scores to split               ##
##       - prop.split = proportion of pairs to split in a neighbourhood       ##
##   - Two custom made functions (included at the beginning of this script):  ##
##       - find.potential.pairs.MSI                                           ##
##       - calculate.averageMSI                                               ##
##                                                                            ##
## Index:                                                                     ##
##   Line 28: Function find.potential.pairs.MSI                               ##
##   Line 74: Function calculate.averageMSI                                   ##
##   Line 97: Main function select.pairs2split                                ##
##   Line 175: Example of use for select.pairs2split                          ##
################################################################################


################## Defining function FIND POTENTIAL PAIRS MSI ##################
## This function finds the MSI of all potential candidate-single pairs
find.potential.pairs.MSI <- function(candidates, singles, MSI_df) {
  
  # Make a list of two (potential pairs for females and males)
  pot_pairs <- list()
  # Column of pair individuals
  pot_pairs[[1]] <- data.frame(pair_female = rep(candidates$female_band, 
                                                 nrow(singles[[2]])))
  pot_pairs[[2]] <- data.frame(pair_male = rep(candidates$male_band, 
                                               nrow(singles[[1]])))
  # Add column of singles
  pot_pairs[[1]]$single_male <- rep(singles[[2]]$Band, 
                                    each = nrow(candidates))
  pot_pairs[[2]]$single_female <- rep(singles[[1]]$Band,
                                      each = nrow(candidates))
  # Make all character
  pot_pairs[[1]][] <- lapply(pot_pairs[[1]], as.character)
  pot_pairs[[2]][] <- lapply(pot_pairs[[2]], as.character)
  
  # Assign MSI scores
  pot_pairs[[1]] <- merge(pot_pairs[[1]], 
                          MSI_df, 
                          by.x = c("pair_female",
                                   "single_male"),
                          by.y = c("female_band", 
                                   "male_band"),
                          sort = FALSE)
  pot_pairs[[2]] <- merge(pot_pairs[[2]], 
                          MSI_df, 
                          by.x = c("pair_male",
                                   "single_female"),
                          by.y = c("male_band", 
                                   "female_band"),
                          sort = FALSE)
  # Make MSI column numeric
  pot_pairs[[1]]$MSI_df <- as.numeric(pot_pairs[[1]]$MSI_df)
  pot_pairs[[2]]$MSI_df <- as.numeric(pot_pairs[[2]]$MSI_df)
  
  # Final list with potential pairs for female and male
  return(pot_pairs)
}
################################################################################



################### Defining function CALCULATE AVERAGE MSI ####################
## This function calculates average MSI of male and female candidates
calculate.averageMSI <- function(candidates, pot_pairs) {
  # Make average MSI columns (empty)
  candidates$female_avMSI <- rep("-", nrow(candidates))
  candidates$male_avMSI <- rep("-", nrow(candidates))
  # Only for non-empty candidates:
  if (nrow(candidates) > 0)
    # Fill in average MSI values
    for (i in 1:nrow(candidates)) {
      # Females average
      tmp1 <- pot_pairs[[1]][pot_pairs[[1]]$pair_female == candidates$female_band[i], ]
      candidates$female_avMSI[i] <- mean(tmp1$MSI_df)
      # Males average
      tmp2 <- pot_pairs[[2]][pot_pairs[[2]]$pair_male == candidates$male_band[i], ]
      candidates$male_avMSI[i] <- mean(tmp2$MSI_df)
    }
  return(candidates)
}
################################################################################



################ Defining MAIN function SELECT PAIRS TO SPLIT ##################
select.pairs2split <- function(actual, MSI.split, singles, MSI_df, pivot, prop.split) {
  
  
  ####### 1. Select only socially observed pairs
  actual <- actual[order(actual$MSI, decreasing = TRUE), ]  # Sort by MSI, highest first
  social <- actual[actual$pair_type != "genetic", ]  # Keep only observable pairs
  social <- droplevels(social)                       # Drop unused levels
  rownames(social) <- 1:(nrow(social))               # Assign row names from 1
  
  
  ####### 2. Choose candidates to split PER NEIGHBOURHOOD
  candidates <- list()
  for (i in levels(social$site))
    # Choose candidates per site, equal or above MSI limit
    candidates[[i]] <- social[social$site == i &
                                social$MSI >= min(MSI.split), ]
  
  
  ####### 3. Order candidate pairs by preference of spliting (top-down)
  # Build possible pairs (candidate-single) and assign their MSI
  pot_pairs <- list()
  for (i in 1:length(candidates)) 
    pot_pairs[[i]] <- find.potential.pairs.MSI(candidates = candidates[[i]],
                                               singles = singles[[i]],
                                               MSI_df = MSI_df)
  
  
  # Calculate average potential MSI for candidates female and male
  for (i in 1:length(candidates))
    candidates[[i]] <- calculate.averageMSI(candidates = candidates[[i]],
                                            pot_pairs = pot_pairs[[i]])
  
  
  # Drop candidate pairs for which there are no single males and females (NaN)
  for (i in 1:length(candidates)) {
    index <- which(candidates[[i]]$female_avMSI == "NaN" &
                     candidates[[i]]$male_avMSI == "NaN")
    if (length(index) > 0)  
      candidates[[i]] <- candidates[[i]][-index, ]
  }
  
  
  # Order candidate pairs by priority
  for (i in 1:length(candidates)) {
    # Check minimum average MSI for the pair
    candidates[[i]]$min_avMSI <- apply(candidates[[i]][, c(7,   # female_avMSI
                                                           8)], # male_avMSI
                                       1,   # by row
                                       min) # obtain minimum value
    # Order decreasingly by MSI, increasingly by min_avMSI
    candidates[[i]] <- candidates[[i]][order(-candidates[[i]]$MSI, 
                                             candidates[[i]]$min_avMSI), ]
  }
  
  
  # Drop candidate pairs that do not improve MSI
  for (i in 1:length(candidates)) {
    index <- which(candidates[[i]]$min_avMSI >= candidates[[i]]$MSI)
    # If there are such pairs, drop them from candidates
    if (length(index) > 0)  
      candidates[[i]] <- candidates[[i]][-index, ]
  }
  
  ####### 4. Calculate max. number of pairs to split
  limits <- list()
  
  for (i in levels(social$site)) {
    # Count singles present in site = max. n of pairs to split (1st LIMIT)
    limits[[i]] <- data.frame(n_singles = sum(pivot[pivot$site == i, "singles"]))
    # Calculate proportion of social pairs = max. n of pairs to split (2nd LIMIT)
    limits[[i]]$n_prop <- floor(nrow(social[social$site == i, ])*(prop.split))
    # Find the lowest limit = FINAL MAX. NUMBER OF PAIRS TO SPLIT
    limits[[i]]$n_split <- apply(limits[[i]],
                                 1,   # by row
                                 min) # obtain minimum value
  }
  
  
  ####### 5. Shorten the list of candidates per season if necessary  
  pairs_to_split <- list()
  for (i in 1:length(candidates))
    # Shorten if too many candidates
    if (nrow(candidates[[i]]) > limits[[i]]$n_split) {
      pairs_to_split[[i]] <- candidates[[i]][1:limits[[i]]$n_split, ]
      # Otherwise, keep all candidates
    } else {
      pairs_to_split[[i]] <- candidates[[i]]
    }
  return(pairs_to_split)
}
################################################################################



################################ Example of use ################################
## select.pairs2split(actual = actual_all[actual_all$season == "14_15", ],    ##
##                    MSI.split = c(5,6),                                     ##
##                    singles = singles_all[[4]],                             ##
##                    MSI_df = MSI_df,                                        ##
##                    pivot = droplevels(pivot_all[pivot_all$season == "2014/2015", ]),
##                    prop.split = 1/4)                                       ##
################################################################################
