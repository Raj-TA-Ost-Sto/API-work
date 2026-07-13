api <- list(
  
  GDP =
    "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE1_EXPENDITURE,2.0/A..S1..B1GQ....USD_PPP.LR..?startPeriod=2008&dimensionAtObservation=AllDimensions&format=csvfile",
  
  Population =
    "https://sdmx.oecd.org/public/rest/data/OECD.ELS.SAE,DSD_POPULATION@DF_POP_HIST,1.0/..PS._T._T.?startPeriod=2008&dimensionAtObservation=AllDimensions&format=csvfile",
  
  GHG =
    "https://sdmx.oecd.org/public/rest/data/OECD.ENV.EPI,DSD_GG@DF_GREEN_GROWTH,1.1/.A.GHG_PBEM.T_CO2E._T?startPeriod=2008&dimensionAtObservation=AllDimensions&format=csvfile",
  
  GDP_Output =
    "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE1_OUTPUT,2.0/A.........V..?startPeriod=2008&dimensionAtObservation=AllDimensions&format=csvfile",
  
  EnvTax =
    "https://sdmx.oecd.org/public/rest/data/OECD.ENV.EPI,DSD_GG@DF_GREEN_GROWTH,1.1/.A...?startPeriod=2008&dimensionAtObservation=AllDimensions&format=csvfile",
  
  FossilFuel =
    "https://sdmx.oecd.org/public/rest/data/OECD.ENV.EPI,DSD_GG@DF_GREEN_GROWTH,1.1/.A.FFS_TTAX.PT_TAX_REV._T?startPeriod=2010&dimensionAtObservation=AllDimensions&format=csvfile",
  
  Technology =
    "https://sdmx.oecd.org/public/rest/data/OECD.ENV.EPI,DSD_GG@DF_GREEN_GROWTH,1.1/.A...?startPeriod=2008&dimensionAtObservation=AllDimensions&format=csvfile"
  
)

library(purrr)
library(readr)

raw_data <- imap(api, function(url,name){
  
  message("Downloading ",name)
  
  read_csv(url,
           show_col_types = FALSE)
  
})
GDP <- as.data.table(raw_data$GDP)

as.data.table(raw_data$Population)



























#
############################################################
## OECD PANEL CREATION
############################################################

library(data.table)
library(purrr)

############################################################
## Generic cleaning function
############################################################

clean_oecd <- function(df, dataset){
  
  dt <- as.data.table(df)
  
  ## Check required columns
  required <- c(
    "REF_AREA",
    "TIME_PERIOD",
    "UNIT_MEASURE",
    "OBS_VALUE"
  )
  
  missing_cols <- setdiff(required, names(dt))
  
  if(length(missing_cols) > 0){
    
    stop(
      paste(
        dataset,
        "is missing:",
        paste(missing_cols, collapse=", ")
      )
    )
    
  }
  
  ##########################################################
  ## Keep variables
  ##########################################################
  
  keep <- c(
    "REF_AREA",
    "TIME_PERIOD",
    "UNIT_MEASURE",
    "OBS_VALUE"
  )
  
  if("REF_YEAR_PRICE" %in% names(dt))
    keep <- c(keep,"REF_YEAR_PRICE")
  
  dt <- dt[, ..keep]
  
  ##########################################################
  ## Rename variables
  ##########################################################
  
  setnames(dt,
           
           old=c(
             "REF_AREA",
             "TIME_PERIOD",
             "UNIT_MEASURE",
             "OBS_VALUE"
           ),
           
           new=c(
             "country",
             "year",
             paste0(dataset,"_UNIT"),
             dataset
           )
           
  )
  
  if("REF_YEAR_PRICE" %in% names(dt))
    setnames(
      dt,
      "REF_YEAR_PRICE",
      paste0(dataset,"_BASEYEAR")
    )
  
  ##########################################################
  ## Convert types
  ##########################################################
  
  dt[, year := as.integer(year)]
  
  dt[, (dataset) := as.numeric(get(dataset))]
  
  ##########################################################
  ## Remove duplicates
  ##########################################################
  
  dt <- unique(dt)
  
  ##########################################################
  ## Sort
  ##########################################################
  
  setorder(dt,country,year)
  
  return(dt)
  
}

############################################################
## Clean every dataset automatically
############################################################

panel_list <- imap(
  raw_data,
  clean_oecd
)

############################################################
## Merge automatically
############################################################

panel <- Reduce(
  
  function(x,y){
    
    merge(
      
      x,
      y,
      
      by=c("country","year"),
      
      all=TRUE
      
    )
    
  },
  
  panel_list
  
)

############################################################
## Sort
############################################################

setorder(panel,country,year)

############################################################
## Save
############################################################

dir.create(
  "output",
  showWarnings=FALSE
)

fwrite(
  panel,
  "output/OECD_panel.csv",
  na=""
)

############################################################
## Summary
############################################################

cat("\n")

cat("-------------------------------------\n")

cat("Datasets processed :", length(raw_data), "\n")

cat("Countries          :", uniqueN(panel$country), "\n")

cat("Years              :", min(panel$year,na.rm=TRUE),
    "-",max(panel$year,na.rm=TRUE), "\n")

cat("Rows               :", nrow(panel), "\n")

cat("Columns            :", ncol(panel), "\n")

cat("-------------------------------------\n")

cat("Saved to output/OECD_panel.csv\n")
