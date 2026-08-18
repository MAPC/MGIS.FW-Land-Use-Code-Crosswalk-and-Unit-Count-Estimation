################################################################################
# Functions for use in MAPC's Land Parcel Database Research, Cleaning, and Spatial Operations
# Author: Brandon Stanaway 
# Contact: bstanaway@mapc.org, bstanaway.econ@gmail.com
# Github: 
# Date Created: 6/6/2025
# 
# Purpose: Create single file to be used for several different LPDB scripts
#
################################################################################

# 1. NA Function

na.finder_f <- function(df){
  print(sapply(X = df, FUN =  function(x) sum(is.na(x))))
}

# 2. Deprecated Use Code Assignment

depr_assign <- function(df){
  
  updated_df <- df |> mutate(
    # Fix depricated use codes for "PUB-SCHOOL" instance first.
    STD.USE_DESC = case_when(
      STD.USE_CODE == "903" & str_detect(MUNI.USE_DESC, regex(paste(c("PUB-SCHOOL", "Pub-School", "pub-school"), collapse = "|"), ignore_case = T)) == TRUE ~ "Improved, Education (Municipal or County)",
      STD.USE_CODE == "9030" & str_detect(MUNI.USE_DESC, regex(paste(c("PUB-SCHOOL", "Pub-School", "pub-school"), collapse = "|"), ignore_case = T)) == TRUE ~ "Improved, Education (Municipal or County)",
      .default = STD.USE_DESC
    ),
    STD.USE_CODE = case_when(
      STD.USE_CODE == "903" & str_detect(MUNI.USE_DESC, regex(paste(c("PUB-SCHOOL", "Pub-School", "pub-school"), collapse = "|"), ignore_case = T)) == TRUE ~ "934",
      STD.USE_CODE == "9030" & str_detect(MUNI.USE_DESC, regex(paste(c("PUB-SCHOOL", "Pub-School", "pub-school"), collapse = "|"), ignore_case = T)) == TRUE ~ "934",
      .default = STD.USE_CODE
    ),
    # Fix for deprecated use codes and decsriptions.
    STD.USE_DESC = case_when(
      STD.USE_CODE == "901" ~ "Comm. Of Mass. (Other)",
      STD.USE_CODE == "9010" ~ "Comm. Of Mass. (Other)",
      STD.USE_CODE == "902" ~ "Improved County or Regional Association Commission",
      STD.USE_CODE == "9020" ~ "Improved County or Regional Association Commission",
      STD.USE_CODE == "903" ~ "Improved, Selectmen or City Council (Municipal)",
      STD.USE_CODE == "9030" ~ "Improved, Selectmen or City Council (Municipal)",
      STD.USE_CODE == "904" ~ "College or University (Educational Private)",
      STD.USE_CODE == "9040" ~ "College or University (Educational Private)",
      STD.USE_CODE == "905" ~ "Other (Charitable Org.)",
      STD.USE_CODE == "9050" ~ "Other (Charitable Org.)",
      STD.USE_CODE == "906" ~ "Other (Religious Org.)",
      STD.USE_CODE == "9060" ~ "Other (Religious Org.)",
      STD.USE_CODE == "907" ~ "121A Corporations",
      STD.USE_CODE == "9070" ~ "121A Corporations",
      STD.USE_CODE == "908" ~ "Housing Authority",
      STD.USE_CODE == "9080" ~ "Housing Authority",
      .default = STD.USE_DESC
    ),
    STD.USE_CODE = case_when(
      STD.USE_CODE == "901" ~ "919",
      STD.USE_CODE == "9010" ~ "919",
      STD.USE_CODE == "902" ~ "994",
      STD.USE_CODE == "9020" ~ "994",
      STD.USE_CODE == "903" ~ "931",
      STD.USE_CODE == "9030" ~ "931",
      STD.USE_CODE == "904" ~ "942",
      STD.USE_CODE == "9040" ~ "942",
      STD.USE_CODE == "905" ~ "951",
      STD.USE_CODE == "9050" ~ "951",
      STD.USE_CODE == "906" ~ "962",
      STD.USE_CODE == "9060" ~ "962",
      STD.USE_CODE == "907" ~ "990",
      STD.USE_CODE == "9070" ~ "990",
      STD.USE_CODE == "908" ~ "970",
      STD.USE_CODE == "9080" ~ "970",
      .default = STD.USE_CODE
    )
  )
  
  return(updated_df)
}

# 3. In-Law Use Code Assignment

inlaw_assign <- function(df){
  df <- df |> 
    mutate(
      # Fix for in-law flag.
      STD.USE_DESC = case_when(
        flag.109 == TRUE ~ "Multiple Houses on one parcel",
        .default = STD.USE_DESC
      ),
      STD.USE_CODE = case_when(
        flag.109 == TRUE ~ "109",
        .default = STD.USE_CODE
      )
    )
  
  return(df)
}
