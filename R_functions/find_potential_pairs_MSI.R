################################################################################
##         Function find the MSI of potential candidate-single pairs          ##
##                                                                            ##
##  Author: Diana Robledo-Ruiz. PhD student, Monash University                ##
##  Date: 2020-04-03                                                          ##
##                                                                            ##
## This function builds all possible pairs per individual (candidate-pair     ##
## member with available singles) and assigns their respective MSI score. If  ##
## there are no candidate pairs, or not available singles, it just returns a  ##
## dataframe with 0 rows.                                                     ##
##                                                                            ##
## This function requires as input:                                           ##
##   - candidates = dataframe of the pairs to split                           ##
##   - singles    = list with two dataframes of single females and males      ##
##   - MSI_df     = MSI dataframe (vertical matrix) for the season            ##
##                                                                            ##
## Index:                                                                     ##
##   Line 24: Function find.potential.pairs.MSI                               ##
##   Line 69: Example of use for find.potential.pairs.MSI                     ##
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



################################ Example of use ################################
## pot_pairs <- list()                                                        ##
## for (i in 1:length(candidates))                                            ##
##   pot_pairs[[i]] <- potential.pairs(candidates = candidates[[i]],          ##
##                                     singles = singles[[i]],                ##
##                                     MSI_df = MSI_df)                       ##
################################################################################
