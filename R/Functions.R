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
