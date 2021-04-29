################################################################################
##       Function to re-pair the pair member with the lowest average MSI      ##
##                                                                            ##
##  Author: Diana Robledo-Ruiz. PhD student, Monash University                ##
##  Date:   2020-04-07                                                        ##
##                                                                            ##
## This function chooses which member of a pair to "re-pair": the one with    ##
## the lowest average MSI score. If both members have the same average, it    ##
## re-pairs the one that has more available singles. If available singles are ##
## the same for both, it keeps the female. Then it re-calculates the average  ##
## MSI for the chosen individuals after adding them in the pool of singles.   ##
##                                                                            ##
## *NOTE: This function correctly deals with members that have NaN as average ##
##        MSI (because there are no available singles). NaN is considered     ##
##        "greater" than any number, so it keeps the individual of the other  ##
##        sex.                                                                ##
##                                                                            ##
## This function requires:                                                    ##
##   - Input:                                                                 ##
##       - drop    = dataframe output of pairs2split function                 ##
##       - singles = list with two dataframes of single females and males     ##
##       - MSI_df  = MSI dataframe (vertical matrix) for the season           ##
##   - Two custom made functions (included at the beginning of this script):  ##
##       - find.potential.pairs.MSI                                           ##
##       - calculate.averageMSI                                               ##
##                                                                            ##
## Index:                                                                     ##
##   Line 36:  Function find.potential.pairs.MSI                              ##
##   Line 81:  Function calculate.averageMSI                                  ##
##   Line 103: Main function re.pair.lowest.avMSI                             ##
##   Line 178: Example of use for find.potential.pairs.MSI                    ##
################################################################################



################## Defining function FIND POTENTIAL PAIRS MSI ##################
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



############## Defining MAIN function RE-PAIR LOWEST AVERAGE MSI ###############
re.pair.lowest.avMSI <- function(drop, singles, MSI_df) {
  
  # If neighbourhood has no pairs to split, it is omitted
  if (nrow(drop) > 0) {
    # Start dataframe of new pairs
    re.pair <- drop[, 1:4]                           # same season and site
    re.pair$pair_type <- rep("re-pair", nrow(drop))  # add pair_type
    re.pair$MSI <- rep("NA", nrow(drop))             # add empty MSI
    
    # Select member of pair to keep:
    for (j in 1:nrow(drop)) {
      # Keep female if its avMSI is lowest
      if (drop$female_avMSI[j] < drop$male_avMSI[j]) 
        re.pair$male_band[j] <- "NA"

      # Keep male if its avMSI is lowest
      if (drop$female_avMSI[j] > drop$male_avMSI[j])
        re.pair$female_band[j] <- "NA"

      # If male and female avMSI are equal, keep the one with more available singles
      if (drop$female_avMSI[j] == drop$male_avMSI[j]) {
        # If more female singles, keep male
        if (nrow(singles[[1]]) > nrow(singles[[2]]))
          re.pair$female_band[j] <- "NA"

        # If more male singles, keep female
        if (nrow(singles[[1]]) < nrow(singles[[2]]))
          re.pair$male_band[j] <- "NA"

        # If equal number of singles, keep female
        if (nrow(singles[[1]]) == nrow(singles[[2]]))
          re.pair$male_band[j] <- "NA"
      }
    }
    
    # Add re-pair individuals as new singles
    new_singles <- singles
    for (j in 1:nrow(re.pair)) {
      if (re.pair$female_band[j] == "NA") {
        new_singles[[2]] <- rbind(new_singles[[2]], 
                                  new_singles[[2]][nrow(new_singles[[2]]), ])
        new_singles[[2]]$Band[nrow(new_singles[[2]])] <- re.pair$male_band[j]
      }
      if (re.pair$male_band[j] == "NA") {
        new_singles[[1]] <- rbind(new_singles[[1]], 
                                  new_singles[[1]][nrow(new_singles[[1]]), ])
        new_singles[[1]]$Band[nrow(new_singles[[1]])] <- re.pair$female_band[j]
      }
    }
    
    # Find all potential pairs with their MSI
    pot_pairs <- find.potential.pairs.MSI(candidates = drop,
                                          singles = new_singles,
                                          MSI_df = MSI_df)
    
    # Re-calculate average MSI of all pairs
    new_avMSI <- calculate.averageMSI(candidates = drop, pot_pairs = pot_pairs)
    
    # Assign new avMSI to re.pair
    for (j in 1:nrow(re.pair)) {
      if (re.pair$female_band[j] == "NA") {
        re.pair$MSI[j] <- new_avMSI$male_avMSI[j]
      }
      if (re.pair$male_band[j] == "NA") {
        re.pair$MSI[j] <- new_avMSI$female_avMSI[j]
      }
    }
    return(re.pair)
  }
}
################################################################################



################################ Example of use ################################
## try <- list()                                                              ##
## for (i in 1:length(drop))                                                  ##
##   try[[i]] <- re.pair.lowest.avMSI(drop = drop[[i]],                       ##
##                                    singles = singles[[i]],                 ##
##                                    MSI_df = MSI_df[[i]])                   ##
################################################################################
