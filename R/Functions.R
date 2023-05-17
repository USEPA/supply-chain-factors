#' Generate emission factor table that includes Supply-chain Emission Factor (SEF)
#' and Marginal Emission Factor (MEF) of GHGs
#' for US commodities/industries at the BEA Detail/Summary level of categorization.
#' @param model A complete USEEIO model.
#' @param price_adjusted_model A list of price-adjusted matrices from the USEEIO model.
#' @param margin A logical value indicating whether to produce Marginal Emission Factor.
#' @return A emission factor table.
generateEmissionFactorTable <- function(model, price_adjusted_model, margin = FALSE, characterized=TRUE) {
  dollaryear <- unique(sub(".*\\_", "", names(price_adjusted_model)))
  # Generate supply chain emission factors table
  if (margin) {
    if (characterized) {
      N_matrix <- paste0("N_margin_pur_", dollaryear)  
    } else {
      M_matrix <- paste0("M_margin_pur_", dollaryear) 
    }
    
  } else {
    if (characterized) {
      N_matrix <- paste0("N_pur_", dollaryear)
    } else {
      M_matrix <- paste0("M_pur_", dollaryear)
    }
  }
  
  if(characterized) {
    df <- as.data.frame(price_adjusted_model[[N_matrix]])#["GWP-AR4-100", ]
    
    flowableforind <- data.frame(Flowable=c("All GHGs","Minor GHGs"),Indicator=c("GWP-AR4-100","Other Greenhouse Gases"))
    df <- merge(df,flowableforind,by.x = 0, by.y="Indicator")
    df <- subset(df,select=-c(Row.names))
    df$Unit <- paste0("kg CO2e/", dollaryear, " USD",", purchaser price")  
  } else {
    
    df <- rbind(as.data.frame(price_adjusted_model[[M_matrix]],
                              row.names = gsub("/.*", "", rownames(price_adjusted_model[[M_matrix]]))))
    df$Flowable <- rownames(df) 
    df <- merge(df, model$SatelliteTables$flows[c("Flowable", "Unit")], by="Flowable", all.x=TRUE, sort=FALSE)
    df$Unit <- sapply(df$Unit, function(x) paste0(x, "/", dollaryear, " USD",", purchaser price"))  
  }

  
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
#' Starts by linking SEFs based on USEEIO codes to NAICS 2012, then uses a standard issue 2012->2017 crosswalk to 
#' 
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
  
  #drop NAICS 2012 info
  table <- subset(table,select=-c(NAICS,`Commodity Name`,`2012 NAICS Title`))
  
  # Filter out rows where both NAICS 2017 and the SEF values are repeated
  table <- table[!duplicated(table[,c(seffields,"2017 NAICS Code")]),]
  
  # First create a mapping between NAICS and model codes
  mapping <- table[,c(useeiocodefield,"2017 NAICS Code")]
  mapping <- mapping[!duplicated(mapping),]
  colnames(mapping) <- c("USEEIO_Code","NAICS_Code")
  
  # Multiple records for NAICS 17 exist where factors are unique - separate these out
  table_mult <- table[duplicated(table[,c("GHG","Unit","2017 NAICS Code")]) | duplicated(table[,c("GHG","Unit","2017 NAICS Code")], fromLast = TRUE),]
  
  # Get the rows where NAICS 2017 factors are unique (rest of table)
  table_uni <- table[setdiff(rownames(table),rownames(table_mult)),]
  
  # For NAICS 2017 with multiple factors, use output to calculate a weighted average
  AllocationTable <- getCommodityOutput2NAICSAllocation(2019,mapping,model)
  
  # Merge table of multiple records with allocation factors
  table_mult_alloc <- merge(table_mult,AllocationTable,by.x=c("2017 NAICS Code",useeiocodefield),by.y=c("NAICS_Code","USEEIO_Code"))
  
  # Multiple SEF fields by allocation factor
  table_mult_alloc[,seffields] <- lapply(table_mult_alloc[, seffields], function(x,y) x * y, y = table_mult_alloc$allocation_factor)
  
  # Aggregate factors by NAICS 2017
  table_mult_alloc_agg <- aggregate(table_mult_alloc[,seffields], by = list(table_mult_alloc$`2017 NAICS Code`,table_mult_alloc$GHG,table_mult_alloc$Unit), sum)
  colnames(table_mult_alloc_agg) <- c("2017 NAICS Code","GHG","Unit",seffields)
  
  # Start table of unique NAICS codes
  naics_17 <- naics_12_to_17[,c("2017 NAICS Code","2017 NAICS Title")]
  naics_17 <- naics_17[!duplicated(naics_17),]
  
  # Create tables from NAICS 17 adding in factors
  table_2017 <- merge(naics_17,table_mult_alloc_agg,by="2017 NAICS Code")
  
  # Bind uni and mult back together
  table_2017 <- rbind(table_uni[ ,colnames(table_2017)],table_2017)

  # Aggregate mapping to show all USEEIO codes by NAICS
  m <- aggregate(mapping[,"USEEIO_Code"], by = list(mapping$NAICS_Code), drop=FALSE, paste, collapse = ", ") 
  colnames(m) <- c("2017 NAICS Code", "Reference USEEIO Code")
  
  # Add mapping back in to keep useeio code along with factors as a reference
  table_2017 <- merge(table_2017,m,by="2017 NAICS Code")
  
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
  # Colname for 2012 NAICS name is"2012 NAICS Title\r\n(and specific piece of the 2012 industry that is contained in the 2017 industry)". Simplify it
  colnames(df)[2] <- "2012 NAICS Title"
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
