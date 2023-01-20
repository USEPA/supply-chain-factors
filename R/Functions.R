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


#' Maps a table of SEFs/MEFs from model sectors to NAICS 2017
#' Implements output-weighted averaging for SEFs where multiple model sectors exist for a NAICS 2017 sector
#' @param table A df of SEFs/MEFs in the form of the table in the calculateEmissionsFactors script
#' @param modelcrosswalk The crosswalk object from the model
#' @param useeiocodefield Name of the field with the model code
#' @param useeionamefield Name of the field with the model name
#' @return A emission factor table.
mapto2017NAICS <- function(table,model,useeiocodefield,useeionamefield, seffields) {
  
  # subset the model original crosswalk to just get model sectors and NAICS_6, which is 2012 NAICS
  xwalk <- model$crosswalk[,c("USEEIO","NAICS")]
  #Just get NAICS 6
  xwalk <- xwalk[nchar(xwalk$NAICS) == 6, ]
  # leaves some NAs, drop those rows
  xwalk <- na.omit(xwalk)
  
  table <- merge(table,xwalk, by.x = c(useeiocodefield), by.y = "USEEIO")
  
  # Load in 2012 to 2017 NAICS crosswalk
  naics_12_to_17 <- getNAICS2012to2017Concordances()
  
  # Merge this in with the table
  table <- merge(table,naics_12_to_17, by.x = c("NAICS"), by.y = "2012 NAICS Code")
  
  # Filter out rows where both NAICS 2017 and the SEF valuese are repeated
  table <- table[!duplicated(table[,c(seffields,"2017 NAICS Code")]),]
  
  # Multiple records for NAICS 17 exist where factors are unique
  table_mult <- table[duplicated(table[,"2017 NAICS Code"]) | duplicated(table[,"2017 NAICS Code"], fromLast = TRUE),]
  
  # Get the rows where NAICS 2017 factors are unique (rest of table)
  table_uni <- table[setdiff(rownames(table),rownames(table_mult)),]
  
  # For NAICS 2017 with multiple factors, use output to calculate a weighted average
  # First create a mapping between NAICS and model codes
  mapping <- table_mult[,c(useeiocodefield,"2017 NAICS Code")]
  colnames(mapping) <- c("USEEIO_Code","NAICS_Code")
  
  AllocationTable <- getCommodityOutput2NAICSAllocation(2019,mapping,model)
  
  # Merge table of multiple records with allocation factors
  table_mult_alloc <- merge(table_mult,AllocationTable,by.x=c("2017 NAICS Code",useeiocodefield),by.y=c("NAICS_Code","USEEIO_Code"))
  
  # Multiple SEF fields by allocation factor
  table_mult_alloc[,seffields] <- lapply(table_mult_alloc[, seffields], function(x,y) x * y, y = table_mult_alloc$allocation_factor)
  
  # Aggregate factors by NAICS 2017
  table_mult_alloc_agg <- aggregate(table_mult_alloc[,seffields], by = list(table_mult_alloc$`2017 NAICS Code`), sum)
  
    # Insert these back into table_mult after removing
  table_mult <- table_mult[!duplicated(table_mult[,"2017 NAICS Code"]),]
  
  # Start table of unique NAICS codes
  naics_17 <- naics_12_to_17[,c("2017 NAICS Code","2017 NAICS Title")]
  naics_17 <- naics_17[!duplicated(naics_17),]
  
  # Create tables from NAICS 17 adding in factors
  table_2017 <- merge(naics_17,table_mult_alloc_agg,by.x="2017 NAICS Code",by.y="Group.1")
  
  # Bind uni and mult back together
  table_2017 <- rbind(table_uni[ ,colnames(table_2017)],table_2017)
  
  return(table_2017)
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


#' Determine allocation factors between given NAICS and model sectors based on commodity output.
#' Slightly modified from https://github.com/USEPA/useeior/blob/6e6b2a6c73efce8a077af76857da71cae8b4bdbf/R/CrosswalkFunctions.R#L15
#' Changes: Use given Just filter for 6-digit NAICS; use commodity output in place of industry output 
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param model A mappings with fields USEEIO_Code and NAICS_Code between given model codes and given NAICS codes
#' @param year Year of model output.
#' @return A table of allocation factors for NAICS sectors for each model sector contributing to NAICS.
getCommodityOutput2NAICSAllocation <- function (year, mapping, model) {
  
  # Get output table for given year
  output <- model$MultiYearCommodityOutput[, as.character(year), drop = FALSE]
  output$Code <- gsub("/.*", "", row.names(output))
  output$Location <- gsub(".*/", "", row.names(output))
  # Merge mapping with Gross Output table to calculate allocation factors
  AllocationTable <- merge(mapping, output,
                           by.x = "USEEIO_Code", by.y = "Code", all.x = TRUE)
  AllocationTable$Output <- AllocationTable[, as.character(year)]
  # Insert placeholders for NAs in the "Output" column
  AllocationTable[is.na(AllocationTable)] <- 1
  # Aggregate Output for the same NAICS code
  sum_temp <- stats::aggregate(AllocationTable$Output, by = list(AllocationTable$NAICS_Code), sum)
  colnames(sum_temp) <- c("NAICS_Code", "SumOutput")
  AllocationTable <- merge(AllocationTable, sum_temp, by = "NAICS_Code", all.x = TRUE)
  # Calculate allocation factors
  AllocationTable$allocation_factor <- AllocationTable$Output/AllocationTable$SumOutput
  # Keep wanted columns
  AllocationTable <- AllocationTable[, c("NAICS_Code", "USEEIO_Code", "Location", "allocation_factor")]
  return(AllocationTable)
}
