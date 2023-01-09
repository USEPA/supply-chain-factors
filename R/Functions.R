#' Generate emission factor table that includes Supply-chain Emission Factor (SEF)
#' and Marginal Emission Factor (MEF) of GHGs
#' for US commodities/industries at the BEA Detail/Summary level of categorization.
#' @param model A complete USEEIO model.
#' @param price_adjusted_model A list of price-adjusted matrices from the USEEIO model.
#' @param margin A logical value indicating whether to produce Marginal Emission Factor.
#' @return A emission factor table.
generateEmissionFactorTable <- function(model, price_adjusted_model, margin = FALSE) {
  dollaryear <- unique(sub(".*\\_", "", names(price_adjusted_model)))
  # Generate supply chain emission factors table
  if (margin) {
    #M_matrix <- paste0("M_margin_pur_", dollaryear)
    N_matrix <- paste0("N_margin_pur_", dollaryear)
  } else {
    #M_matrix <- paste0("M_pur_", dollaryear)
    N_matrix <- paste0("N_pur_", dollaryear)
  }
  #df <- rbind(as.data.frame(price_adjusted_model[[M_matrix]],
  #                          row.names = gsub("/.*", "", rownames(price_adjusted_model[[M_matrix]]))),
  #            as.data.frame(price_adjusted_model[[N_matrix]])["Other Greenhouse Gases", ])
  df <- as.data.frame(price_adjusted_model[[N_matrix]])["GWP-AR4-100", ]
  df$Unit <- paste0(c(rep(paste0("kg/", dollaryear, " USD"), nrow(df)-1),
                      paste0("kg CO2e/", dollaryear, " USD")),
                    ", purchaser price")
  #df$Flowable <- rownames(df)
  df$Flowable <- "CO2e"
  
  # Reshape df into long table
  df_long <- reshape2::melt(df, id.vars = c("Flowable", "Unit"),
                            variable.name = "Sector", value.name = "FlowAmount")
  df_long$Sector <- gsub("/.*", "", as.character(df_long$Sector))
  df_long <- merge(df_long,
                   model[[sub("y", "ies", model$specs$CommodityorIndustryType)]][, c("Code", "Name")],
                   by.x = "Sector", by.y = "Code")
  # Re-order columns
  df_long <- df_long[, c("Sector", "Name", "Flowable", "Unit", "FlowAmount")]
  return(df_long)
}


#' Maps a table of SEFs/MEFs from model sectors to NAICS 2017.
#' flexible for different field names for codes and names
#' @param table A df of SEFs/MEFs in the form of the table in the calculateEmissionsFactors script
#' @param modelcrosswalk The crosswalk object from the model
#' @param useeiocodefield Name of the field with the model code
#' @param useeionamefield Name of the field with the model name
#' @return A emission factor table.
mapto2017NAICS <- function(table,modelcrosswalk,useeiocodefield,useeionamefield) {
  library(devtools)
  # subset the model original crosswalk to just get model sectors and NAICS_6, which is 2012 NAICS
  xwalk <- modelcrosswalk[,c("USEEIO","NAICS")]
  #Just get NAICS 6
  xwalk <- xwalk[nchar(xwalk$NAICS) == 6, ]
  # leaves some NAs, drop those rows
  xwalk <- na.omit(xwalk)
  
  table <- merge(table,xwalk, by.x = c(useeiocodefield), by.y = "USEEIO")
  
  # Load in 2012 to 2017 NAICS crosswalk
  naics_12_to_17 <- getNAICS2012to2017Concordances()
  naics_12_to_17 <- naics_12_to_17[,c("2012 NAICS Code","2017 NAICS Code","2017 NAICS Title")]
  
  # Merge this in with the table
  table <- merge(table,naics_12_to_17, by.x = c("NAICS"), by.y = "2012 NAICS Code")
  
  # Remove the additional columns up the fields 
  #table <- table[,-c("NAICS",useeiocodefield,useeionamefield)]
  return(table)
}

#' Loads the Census NAICS 2012 to 2017 crosswalk if not already present
#' Repurposed from useeior package 
#' Original at https://github.com/USEPA/useeior/blob/6e6b2a6c73efce8a077af76857da71cae8b4bdbf/R/CrosswalkFunctions.R#L217
getNAICS2012to2017Concordances <- function() {
  filename <- "2012_to_2017_NAICS.xlsx"
  if(!file.exists(filename)) {
    utils::download.file("https://www.census.gov/naics/concordances/2012_to_2017_NAICS.xlsx",
                         filename, mode = "wb")
  }
  df <- as.data.frame(readxl::read_excel(filename, sheet = 1, col_names = TRUE, skip = 2))
  df <- df[, startsWith(colnames(df), "20")]
  return(df)
}

