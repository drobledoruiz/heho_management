################################################################################
##          Function identify individuals "removed" from population           ##
##                                                                            ##
##  Author: Diana Robledo-Ruiz. PhD student, Monash University                ##
##  Date:   2020-04-07                                                        ##
##                                                                            ##
## This function identifies per neighbourhood, the ID of the individuals that ##
## were "removed" from the population (not re-paired) in management1.         ##
##                                                                            ##
## This function requires as input:                                           ##
##   - re.pair = list of dataframes of the new formed pairs ("re-pair")       ##
##   - drop    = list of the pairs that were split with the original IDs      ##
##                                                                            ##
## Index:                                                                     ##
##   Line 21: Function find.potential.pairs.MSI                               ##
##   Line 42: Example of use for find.potential.pairs.MSI                     ##
################################################################################



############### Defining function IDENTIFY REMOVED INDIVIDUALS #################
identify.removed.ind <- function(re.pair, drop) {
  removed <- list()
  for (i in 1:length(re.pair)) {
    removed[[i]] <- vector()
    # If the re.pair list is not empty (NULL)
    if (is.null(re.pair[[i]]) != TRUE) {
      for (j in 1:nrow(re.pair[[i]])) {
        if (re.pair[[i]]$female_band[j] == "NA")
          removed[[i]][j] <- drop[[i]]$female_band[j]
        if (re.pair[[i]]$male_band[j] == "NA")
          removed[[i]][j] <- drop[[i]]$male_band[j]
      }
    }
  }
  return(removed)
}
################################################################################



################################ Example of use ################################
## removed <- list()                                                          ##
## for (i in 1:length(re.pair))                                               ##
##   removed[[i]] <- identify.removed.ind(re.pair = re.pair[[i]],             ##
##                                        drop = drop[[i]])                   ##
################################################################################
