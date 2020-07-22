################################################################################
##       Function calculate average MSI of male and female candidates         ##
##                                                                            ##
##  Author: Diana Robledo-Ruiz. PhD student, Monash University                ##
##  Date: 2020-04-03                                                          ##
##                                                                            ##
## This function uses a list of potential pairs to calculate:                 ##
##   - female_avMSI with all available male singles.                          ##
##   - male_avMSI with all available female singles.                          ##
## If there are no candidate pairs (0 rows), it leaves the dataframe with 0   ##
## rows. If there are no available singles for a pair member, it assigns NaN  ##
## as average.                                                                ##
##                                                                            ##
## This function requires as input:                                           ##
##   - candidates = dataframe of the pairs to split                           ##
##   - pot_pairs  = list with two dataframes of potential pairs for females   ##
##                  and males.                                                ##
##                                                                            ##
## Index:                                                                     ##
##   Line 26: Function calculate.averageMSI                                   ##
##   Line 49: Example of use for find.potential.pairs.MSI                     ##
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



################################ Example of use ################################
## for (i in 1:length(candidates))                                            ##
##   candidates[[i]] <- calculate.averageMSI(candidates = candidates[[i]],    ##
##                                           pot_pairs = pot_pairs[[i]])      ##
################################################################################
