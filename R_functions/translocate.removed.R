################################################################################
##             Function to translocate the removed individuals to             ##
##                 the neighbourhood with lowest average MSI                  ##
##                                                                            ##
##  Author: Diana Robledo-Ruiz. PhD student, Monash University                ##
##  Date: 2021-05-04                                                          ##
##                                                                            ##
## This function requires:                                                    ##
##   - Input:                                                                 ##
##       - re.pair = list output of re.pair.lowest.avMSI function             ##
##       - drop    = dataframe output of pairs2split function                 ##
##       - singles = list with two dataframes of single females and males     ##
##       - MSI_df  = MSI dataframe (vertical matrix) for the season           ##
##       - kin_df  = Kinship dataframe (vertical matrix) for the season       ##
##   - Custom made function called "identify.removed.ind" that I include at   ##
##     the beginning of this script                                           ##
##                                                                            ##
## Index:                                                                     ##
##   Line 26: Function identify.removed.ind                                   ##
##   Line 47: Main function translocate.removed                               ##
##   Line 432: Example of use for translocate.removed                         ##
################################################################################



################ Defining function IDENTIFY REMOVED INDIVIDUALS ################
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



################## Defining main function TRANSLOCATE REMOVED ##################
translocate.removed <- function(re.pair, drop, singles, MSI_df, kin_df) {
  
  # Identify removed individuals per neighbourhood
  removed <- list()
  for (i in 1:length(re.pair)) {
    removed <- identify.removed.ind(re.pair = re.pair,
                                    drop = drop)
  }
  
  # Make df of male and female singles
  singles_males <- singles[[1]][[2]]
  if (length(singles) > 1) {
    for (i in 2:length(singles)) {
      singles_males <- rbind(singles_males, singles[[i]][[2]])
    }
  }
  singles_males <- droplevels(singles_males)
  
  singles_females <- singles[[1]][[1]]
  if (length(singles) > 1) {
    for (i in 2:length(singles)) {
      singles_females <- rbind(singles_females, singles[[i]][[1]])
    }
  }
  singles_females <- droplevels(singles_females)
  
  # Obtain list of neighbourhoods with singles
  sites <- levels(rbind(singles_females, singles_males)$Site)
  
  # Create list where we will put output
  trans <- list()
  
  # LOOP STARTS!!!
  for (i in 1:length(removed)) { # per neighbourhood
    
    # If the list is not empty (there are removed inds to translocate)
    if (is.character(removed[[i]])){
      
      trans[[i]] <- list()
      
      female <- vector()
      male <- vector()
      
      # Grab males and females
      for (j in 1:length(removed[[i]])) { #individual (line)
        # Females
        if (removed[[i]][j] %in% drop[[i]]$female_band)
          female[j] <- removed[[i]][j]
        # Males
        if (removed[[i]][j] %in% drop[[i]]$male_band)
          male[j] <- removed[[i]][j]
      }
      
      # Drop NAs
      female <- female[!is.na(female)]
      male <- male[!is.na(male)]
      
      # Build table
      female <- as.data.frame(female)
      male <- as.data.frame(male)
      
      names(female)[names(female) == "female"] <- "female_band"
      names(male)[names(male) == "male"] <- "male_band"
      
      if (nrow(female) > 0)
        female$site <- drop[[i]]$site[1]
      
      if (nrow(male) > 0)
        male$site <- drop[[i]]$site[1]
      
      female <- droplevels(female)
      male <- droplevels(male)
      
      # Check if there are neighbourhoods to which to translocate
      sites_trans <- sites[!sites %in% unique(c(levels(female$site),
                                                levels(male$site)))]
      
      # NO OTHER neighbourhoods:
      if (length(sites_trans) < 1) {
        print("There is not another neighbourhood to translocate to")
        stop("Function is terminated")
      }
      
      # ONE neighbourhood to which to translocate:
      if (length(sites_trans) == 1) {
        
        # FEMALES with singles from the other neighbourhood
        if (nrow(female) > 0) {
          tmp <- female[rep(seq_len(nrow(female)), 
                            each = nrow(singles_males[singles_males$Site == sites_trans, ])), ]
          
          tmp2 <- singles_males[singles_males$Site == sites_trans, 
                                c("Band", "Site")]
          
          if (length(levels(tmp$female_band)) > 1) {
            for (x in 2:length(levels(tmp$female_band))) {
              tmp2 <- rbind(tmp2, 
                            singles_males[singles_males$Site == sites_trans, 
                                          c("Band", "Site")])
            }
          }
          
          female <- cbind(tmp, tmp2)
          
          female <- merge(female, MSI_df,
                          by.x = c("female_band", "Band"),
                          by.y = c("female_band", "male_band"))
          
          female$MSI_df <- as.numeric(female$MSI_df)
          
          female <- merge(female, kin_df,
                          by.x = c("female_band", "Band"),
                          by.y = c("female_band", "male_band"))
          
          female <- droplevels(female)
          
          # Build final table
          trans_female <- data.frame(female_band = rep(NA, 
                                                       length(levels(female$female_band))),
                                     Site = rep(NA, 
                                                length(levels(female$female_band))), # new site
                                     avMSI = rep(NA, 
                                                 length(levels(female$female_band))),
                                     avKIN = rep(NA, 
                                                 length(levels(female$female_band))))
          
          # Fill-in final table
          for (x in 1:length(levels(female$female_band))) {
            trans_female$female_band[x] <- levels(female$female_band)[x]
            trans_female$Site[x] <- levels(female$Site)
            trans_female$avMSI[x] <- mean(female[female$female_band == levels(female$female_band)[x], 
                                                 "MSI_df"])
            trans_female$avKIN[x] <- mean(female[female$female_band == levels(female$female_band)[x], 
                                                 "kinship"])
          }
          
          trans[[i]][[1]] <- trans_female
        }
        
        # MALES with singles from the other neighbourhood
        if (nrow(male) > 0) {
          tmp <- male[rep(seq_len(nrow(male)), 
                          each = nrow(singles_females[singles_females$Site == sites_trans, ])), ]
          
          tmp2 <- singles_females[singles_females$Site == sites_trans, 
                                  c("Band", "Site")]
          
          if (length(levels(tmp$male_band)) > 1) {
            for (x in 2:length(levels(tmp$male_band))) {
              tmp2 <- rbind(tmp2, 
                            singles_females[singles_females$Site == sites_trans, 
                                            c("Band", "Site")])
            }
          }
          
          male <- cbind(tmp, tmp2)
          
          male <- merge(male, MSI_df,
                        by.x = c("male_band", "Band"),
                        by.y = c("male_band", "female_band"))
          
          male$MSI_df <- as.numeric(male$MSI_df)
          
          male <- merge(male, kin_df,
                        by.x = c("male_band", "Band"),
                        by.y = c("male_band", "female_band"))
          
          male <- droplevels(male)
          
          trans_male <- data.frame(male_band = rep(NA, 
                                                   length(levels(male$male_band))),
                                   Site = rep(NA, 
                                              length(levels(male$male_band))), # new site
                                   avMSI = rep(NA, 
                                               length(levels(male$male_band))),
                                   avKIN = rep(NA, 
                                               length(levels(male$male_band))))
          
          for (x in 1:length(levels(male$male_band))) {
            trans_male$male_band[x] <- levels(male$male_band)[x]
            trans_male$Site[x] <- levels(male$Site)
            trans_male$avMSI[x] <- mean(male[male$male_band == levels(male$male_band)[x], 
                                             "MSI_df"])
            trans_male$avKIN[x] <- mean(male[male$male_band == levels(male$male_band)[x], 
                                             "kinship"])
          }
          
          trans[[i]][[2]] <- trans_male
        }
        
      }
      
      
      # MORE THAN ONE neighbourhood to which to translocate:
      if (length(sites_trans) > 1) {
        
        # FEMALES with singles from other neighbourhoods
        if (nrow(female) > 0) {
          trans_female_all <- data.frame()
          
          for (k in 1:length(sites_trans)) {
            if (sites_trans[k] %in% levels(singles_males$Site)) {
              tmp <- female[rep(seq_len(nrow(female)), 
                                each = nrow(singles_males[singles_males$Site == sites_trans[k], ])), ]
              
              tmp2 <- singles_males[singles_males$Site == sites_trans[k], 
                                    c("Band", "Site")]
              
              if (length(levels(tmp$female_band)) > 1) {
                for (x in 2:length(levels(tmp$female_band))) {
                  tmp2 <- rbind(tmp2, 
                                singles_males[singles_males$Site == sites_trans[k], 
                                              c("Band", "Site")])
                }
              }
              
              tmp_female <- cbind(tmp, tmp2)
              
              tmp_female <- merge(tmp_female, MSI_df,
                                  by.x = c("female_band", "Band"),
                                  by.y = c("female_band", "male_band"))
              
              tmp_female$MSI_df <- as.numeric(tmp_female$MSI_df)
              
              tmp_female <- merge(tmp_female, kin_df,
                                  by.x = c("female_band", "Band"),
                                  by.y = c("female_band", "male_band"))
              
              tmp_female <- droplevels(tmp_female)
              
              trans_female <- data.frame(female_band = rep(NA, 
                                                           length(levels(female$female_band))),
                                         Site = rep(NA, 
                                                    length(levels(female$female_band))), # new site
                                         avMSI = rep(NA, 
                                                     length(levels(female$female_band))),
                                         avKIN = rep(NA, 
                                                     length(levels(female$female_band))))
              
              for (x in 1:length(levels(tmp_female$female_band))) {
                trans_female$female_band[x] <- levels(tmp_female$female_band)[x]
                trans_female$Site[x] <- levels(tmp_female$Site)
                trans_female$avMSI[x] <- mean(tmp_female[tmp_female$female_band == levels(tmp_female$female_band)[x], 
                                                         "MSI_df"])
                trans_female$avKIN[x] <- mean(tmp_female[tmp_female$female_band == levels(tmp_female$female_band)[x], 
                                                         "kinship"])
              }
              
              trans_female_all <- rbind(trans_female_all, trans_female)
            }  
          }
          
          ## Select only lowest avMSI
          trans_female_all$female_band <- as.factor(trans_female$female_band)
          
          trans_female_final <- data.frame()
          
          for (l in 1:length(levels(trans_female_all$female_band))){
            tmp_trans <- trans_female_all[trans_female_all$female_band == levels(trans_female_all$female_band)[l], ]
            
            # order by avMSI
            tmp_trans <- tmp_trans[order(tmp_trans$avMSI), ]
            
            # Keep only the lowest
            trans_female_final <- rbind(trans_female_final, tmp_trans[1, ])
          }
          
          trans[[i]][[1]] <- trans_female_final   
        }
        
        # MALES with singles from other neighbourhoods
        if (nrow(male) > 0) {
          trans_male_all <- data.frame()
          
          for (k in 1:length(sites_trans)) {
            if (sites_trans[k] %in% levels(singles_females$Site)) {
              tmp <- male[rep(seq_len(nrow(male)), 
                              each = nrow(singles_females[singles_females$Site == sites_trans[k], ])), ]
              
              tmp2 <- singles_females[singles_females$Site == sites_trans[k], 
                                      c("Band", "Site")]
              
              if (length(levels(tmp$male_band)) > 1) {
                for (x in 2:length(levels(tmp$male_band))) {
                  tmp2 <- rbind(tmp2, 
                                singles_females[singles_females$Site == sites_trans[k], 
                                                c("Band", "Site")])
                }
              }
              
              tmp_male <- cbind(tmp, tmp2)
              
              tmp_male <- merge(tmp_male, MSI_df,
                                by.x = c("male_band", "Band"),
                                by.y = c("male_band", "female_band"))
              
              tmp_male$MSI_df <- as.numeric(tmp_male$MSI_df)
              
              tmp_male <- merge(tmp_male, kin_df,
                                by.x = c("male_band", "Band"),
                                by.y = c("male_band", "female_band"))
              
              tmp_male <- droplevels(tmp_male)
              
              trans_male <- data.frame(male_band = rep(NA, 
                                                       length(levels(male$male_band))),
                                       Site = rep(NA, 
                                                  length(levels(male$male_band))), # new site
                                       avMSI = rep(NA, 
                                                   length(levels(male$male_band))),
                                       avKIN = rep(NA, 
                                                   length(levels(male$male_band))))
              
              for (x in 1:length(levels(tmp_male$male_band))) {
                trans_male$male_band[x] <- levels(tmp_male$male_band)[x]
                trans_male$Site[x] <- levels(tmp_male$Site)
                trans_male$avMSI[x] <- mean(tmp_male[tmp_male$male_band == levels(tmp_male$male_band)[x], 
                                                     "MSI_df"])
                trans_male$avKIN[x] <- mean(tmp_male[tmp_male$male_band == levels(tmp_male$male_band)[x], 
                                                     "kinship"])
              }
              
              trans_male_all <- rbind(trans_male_all, trans_male)
              
            }
          }
          
          ## Select only lowest avMSI
          trans_male_all$male_band <- as.factor(trans_male$male_band)
          
          trans_male_final <- data.frame()
          
          for (l in 1:length(levels(trans_male_all$male_band))){
            
            tmp_trans <- trans_male_all[trans_male_all$male_band == levels(trans_male_all$male_band)[l], ]
            
            # order by avMSI
            tmp_trans <- tmp_trans[order(tmp_trans$avMSI), ]
            
            # Keep only the lowest
            trans_male_final <- rbind(trans_male_final, tmp_trans[1, ])
          }
          
          trans[[i]][[2]] <- trans_male_final   
        }
        
      }
      
    }
    
  }
  
  # Fuse list to df
  df <- data.frame()
  
  for (y in 1:length(trans)) {
    for (z in 1:length(trans[[y]])) {
      if (! is.null(trans[[y]][[z]])) {
        
        # Change names of columns
        names(trans[[y]][[z]])[names(trans[[y]][[z]]) == "Site"] <- "site"
        
        if ("female_band" %in% colnames(trans[[y]][[z]])){
          trans[[y]][[z]]$sex <- rep("F", nrow(trans[[y]][[z]]))
          names(trans[[y]][[z]])[names(trans[[y]][[z]]) == "female_band"] <- "band"
        } else {
          trans[[y]][[z]]$sex <- rep("M", nrow(trans[[y]][[z]]))
          names(trans[[y]][[z]])[names(trans[[y]][[z]]) == "male_band"] <- "band"
        }
        
        # Add pair-type column
        trans[[y]][[z]]$pair_type <- rep("translocated", nrow(trans[[y]][[z]]))
        
        df <- rbind(df, trans[[y]][[z]])
      }
    }
  }
  
  ##### Final dataframe with translocated individuals
  return(df)
}
################################################################################


        
################################ Example of use ################################
## translocate.removed(re.pair = re.pair,                                     ##
##                     drop = drop,                                           ##
##                     singles = singles,                                     ##
##                     MSI_df = MSI_df,                                       ##
##                     kin_df = kind_df)                                      ##
################################################################################

