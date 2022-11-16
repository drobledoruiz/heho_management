# Functions to evaluate the potential benefits of two breeding management strategies for wild populations

In this repository you find the functions and code to run simulations as presented in Robledo-Ruiz et al. (2021) "A novel framework for evaluating _in-situ_ breeding management strategies in endangered populations" _Molecular Ecology Resources_. With these functions you can:
1. Asses the presence and degree of inbreeding avoidance: _sample.random.pairs_ and _find.best.pairs_
2. Estimate the potential benefits of implementing the two breeding management strategies proposed by Robledo-Ruiz et al. (2021): _select.pairs2split_, _re.pair.lowest.avMSI_ and _translocate.removed_.

Contact: Diana Robledo-Ruiz, diana.robledoruiz1@monash.edu

--------------------------------------------------------------
The functions _sample.random.pairs_ and _find.best.pairs_ are used per site in a population (a population may be divided in multiple social subgroups: "sites"). They require as input an MSI matrix (for the site) in vertical format (dataframe) with each row pertaining to one male-female pair and their MSI score. The columns should be named: 
  - First column "female_band": ID of female
  - Second column "male_band": ID of male
  - Third column "MSI": MSI score of that pair

ATTENTION! The functions only work well with dataframes (NOT with tibbles!).

If you have a matrix with females as columns and males as rows (as the one obtained from PMx), and want to make it vertical:
  - Make sure the first column (that should contain male IDs) is named "UniqueID"
  - Follow the next script:

```
  # Import the file directly obtained from PMx
  matrix <- read.table(file,
                       sep = "\t",
                       header = FALSE)
  
  names(matrix) <- lapply(matrix[1, ], as.character)
  
  matrix <- matrix[-c(1, 2), -2]  # Drop empty column and row
  
  # Make MSI matrix a vertical dataframe with appropriate format
  library(tidyr)
  df <- gather(matrix, 
               key   = "male_band", 
               value = "MSI", 
               -UniqueID)  # Exclude the 1st column ("UniqueID") = males
  
  names(df)[names(df) == "UniqueID"] <- "female_band"  # Change name of 1st column
  
  df[] <- lapply(df, as.character)  # Make all columns character
  
  rownames(df) <- 1:(nrow(df))  # Assign row names from 1 to nrow
```


---------------------------------------------------------------------------------------
The function _select.pairs2split_ is used for an entire population that is divided in social subgroups (sites). It requires as input 6 items:
  1. actual= a dataframe of pairs occuring in the population with the next columns:
      - "female_band": female ID as character
      - "male_band": male ID as character
      - "season": bredding season as factor with one level. All pairs should have the same season.
      - "site": factor with at least two levels (two sites)
      - "pair_type": factor that describes the type of pair. A pair can be "social" (observed in the field), "genetic" (discovered by parentage analyses and NOT observed in the field), or "both" (observed in the field and confirmed by parentage analyses). If no parentage analyses were done, make all pairs "social".
      - "MSI": the MSI score of each pair

  2. MSI.split= a vector with the MSI scores that would make a pair a candidate to be split. Examples: 5 or c(5, 6)
  
  3. singles= the data of the individuals that are present in the population but not engaged in a pair (aka singles). This should be a list that contains as many sublists as sites there are in actual. Each sublist should have two dataframes (first dataframe for single females, second dataframe for single males). This means that if there are two sites, A and B, there should be four dataframes (singles[[1]] is the sublist for site A, with singles[[1]][[1]] being the dataframe for single females, and singles[[1]][[2]] the dataframe for single males; singles[[2]] is the sublist for site B, with singles[[2]][[1]] being the dataframe for single females, and singles[[2]][[2]] the dataframe for single males). Each dataframe should have the columns:
      - "Site": the site of the pairs. All individuals in a dataframe should have the same site.
      - "Band": the ID of the individual.
      - "Sex": the sex of the individual. All individuals in a dataframe should have the same sex.

  4. MSI_df= the MSI matrix (for the whole population) in vertical format (dataframe) with each row pertaining to one male-female pair and their MSI score. The columns should be named: 
      - First column "female_band": ID of female
      - Second column "male_band": ID of male
      - Third column "MSI_df": MSI score of that pair

  5. pivot= a dataframe with the count of the number of singles per site, per sex. It should have the columns:
      - "site": in the example of two sites, values from top to bottom would be "A" "A" "B" "B"
      - "sex": first females and then males. In the example of two sites, values form top to bottom would be "F" "M" "F" "M"
      - "singles": the count of singles per site and sex

  6. prop.split= the maximum proportion of pairs that can be split per site. . Examples: 0.25 or 1/3

It outputs a list with as many dataframes as sites (used as input in the next function _re.pair.lowest.avMSI_). Each dataframe contains the pairs that should be split per site.



--------------------------------------------------
The function _re.pair.lowest.avMSI_ is used for an entire population that is divided in social subgroups (sites). It is the next step after using _select.pairs2split_ and it tells you which individual to keep in the population (not remove) from the pairs that were selected to split in the previous step (it also tells you the potential average MSI that the individual that you keep will have). It requires as input 3 items:
  1. drop= the output of the previous step _select.pairs2split_ which is a list with as many dataframes as sites. Each dataframe contains the pairs that were selected to be split per site.
 
  2. singles= the same item that was used by _select.pairs2split_. It is the data of the individuals that are present in the population but not engaged in a pair (aka singles). This should be a list that contains as many sublists as sites there are in actual. Each sublist should have two dataframes (first dataframe for single females, second dataframe for single males). This means that if there are two sites, A and B, there should be four dataframes (singles[[1]] is the sublist for site A, with singles[[1]][[1]] being the dataframe for single females, and singles[[1]][[2]] the dataframe for single males; singles[[2]] is the sublist for site B, with singles[[2]][[1]] being the dataframe for single females, and singles[[2]][[2]] the dataframe for single males). Each dataframe should have the columns:
      - "Site": the site of the pairs. All individuals in a dataframe should have the same site.
      - "Band": the ID of the individual.
      - "Sex": the sex of the individual. All individuals in a dataframe should have the same sex.

  3. MSI_df= the same item that was used by _select.pairs2split_. It is the MSI matrix (for the whole population) in vertical format (dataframe) with each row pertaining to one male-female pair and their MSI score. The columns should be named: 
      - First column "female_band": ID of female
      - Second column "male_band": ID of male
      - Third column "MSI_df": MSI score of that pair

The output is a a list with as many dataframes as sites. Each dataframe contains the individuals to keep in the population (not remove).



--------------------------------------------------
The function _translocate.removed_ is used for an entire population that is divided in social subgroups (sites). It is the next step after using _re.pair.lowest.avMSI_ and it tells you to which site to translocate the individuals you removed (from the pairs that were selected to split). It chooses the site for which the individual has the lowest average MSI with the singles in the site (it tells you the potential average MSI and kinship). It requires as input 5 items:
  1. re.pair= the output of the previous step _re.pair.lowest.avMSI_. It is a list of vectors, each vector with the ID of the individuals to keep in the population (not remove).
 
  2. drop= the output of the step _select.pairs2split_ which is a list with as many dataframes as sites. Each dataframe contains the pairs that were selected to be split per site.
 
  3. singles= the same item that was used by _select.pairs2split_ and _re.pair.lowest.avMSI_. It is the data of the individuals that are present in the population but not engaged in a pair (aka singles). This should be a list that contains as many sublists as sites there are in actual. Each sublist should have two dataframes (first dataframe for single females, second dataframe for single males). This means that if there are two sites, A and B, there should be four dataframes (singles[[1]] is the sublist for site A, with singles[[1]][[1]] being the dataframe for single females, and singles[[1]][[2]] the dataframe for single males; singles[[2]] is the sublist for site B, with singles[[2]][[1]] being the dataframe for single females, and singles[[2]][[2]] the dataframe for single males). Each dataframe should have the columns:
      - "Site": the site of the pairs. All individuals in a dataframe should have the same site.
      - "Band": the ID of the individual.
      - "Sex": the sex of the individual. All individuals in a dataframe should have the same sex.

  4. MSI_df= the same item that was used by _select.pairs2split_ and _re.pair.lowest.avMSI_. It is the MSI matrix (for the whole population) in vertical format (dataframe) with each row pertaining to one male-female pair and their MSI score. The columns should be named: 
      - First column "female_band": ID of female
      - Second column "male_band": ID of male
      - Third column "MSI_df": MSI score of that pair

  5.  MSI_df= the pair-kinship matrix (for the whole population) in vertical format (dataframe) with each row pertaining to one male-female pair and their pair-kinship value. The columns should be named: 
      - First column "female_band": ID of female
      - Second column "male_band": ID of male
      - Third column "kinship": MSI score of that pair
