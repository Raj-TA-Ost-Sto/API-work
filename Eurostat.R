############################################################
## EUROSTAT API Data 
############################################################

# Install packages (run once)
packages <- c(
  "eurostat",
  "dplyr",
  "readr",
  "purrr",
  "stringr"
)

new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]

if(length(new_packages)>0)
  install.packages(new_packages)

library(eurostat)
library(dplyr)
library(readr)
library(purrr)
library(stringr)

############################################################
# Create folders
############################################################

dir.create("data",showWarnings=FALSE)
dir.create("data/raw",showWarnings=FALSE)

dir.create("metadata",showWarnings=FALSE)
dir.create("metadata/variables",showWarnings=FALSE)
dir.create("metadata/dictionaries",showWarnings=FALSE)

############################################################
# Dataset list
############################################################

datasets <- c(
  
  "bd_size",
  
  "env_esst_gg",
  
  "env_ac_ainah_r2",
  
  "env_ac_taxind2",
  
  "env_ac_pefa04",
  
  "env_ac_ccminv",
  
  "env_wasgen",
  
  "env_wat_cat",
  
  "rd_e_berdfundr2",
  
  "nama_10_a64",
  
  "nama_10_a64_e"
  
)

############################################################
# Log file
############################################################

log <- tibble(
  dataset=character(),
  status=character(),
  message=character()
)

############################################################
# Download loop
############################################################

for(ds in datasets){
  
  cat("\n============================\n")
  cat("Dataset:",ds,"\n")
  cat("============================\n")
  
  tryCatch({
    
    ############################################################
    ## Download data
    ############################################################
    
    data <- get_eurostat(
      id=ds,
      time_format="num",
      cache=FALSE
    )
    
    write_csv(
      data,
      paste0("data/raw/",ds,".csv")
    )
    
    saveRDS(
      data,
      paste0("data/raw/",ds,".rds")
    )
    
    ############################################################
    ## Variable metadata
    ############################################################
    
    meta <- tibble(
      
      Variable=names(data),
      
      Class=sapply(data,class),
      
      Unique_Values=sapply(data,function(x){
        
        length(unique(x))
        
      })
      
    )
    
    write_csv(
      
      meta,
      
      paste0(
        
        "metadata/variables/",
        
        ds,
        
        "_variables.csv"
        
      )
      
    )
    
    ############################################################
    ## Download dictionaries
    ############################################################
    
    dimensions <- names(data)
    
    for(v in dimensions){
      
      if(v %in% c("values","time"))
        
        next
      
      dic <- tryCatch(
        
        get_eurostat_dic(v),
        
        error=function(e) NULL
        
      )
      
      if(!is.null(dic)){
        
        write_csv(
          
          dic,
          
          paste0(
            
            "metadata/dictionaries/",
            
            ds,
            
            "_",
            
            v,
            
            ".csv"
            
          )
          
        )
        
      }
      
    }
    
    ############################################################
    ## Update log
    ############################################################
    
    log <- bind_rows(
      
      log,
      
      tibble(
        
        dataset=ds,
        
        status="SUCCESS",
        
        message="Downloaded"
        
      )
      
    )
    
  },
  
  error=function(e){
    
    cat("FAILED:",ds,"\n")
    
    cat(e$message,"\n")
    
    log <<- bind_rows(
      
      log,
      
      tibble(
        
        dataset=ds,
        
        status="FAILED",
        
        message=e$message
        
      )
      
    )
    
  }
  
  )
  
}

############################################################
## Download Eurostat table of contents
############################################################

toc <- get_eurostat_toc()

write_csv(
  
  toc,
  
  "metadata/Eurostat_Table_of_Contents.csv"
  
)

############################################################
## Save log
############################################################

write_csv(
  
  log,
  
  "download_log.csv"
  
)

cat("\nFinished.\n")
############################################
# GET EUROSTAT FUNCTION 
# 
# get_eurostat(bd_size)

#packages <- c("eurostat","dplyr","readr","purrr","stringr","tibble")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages) > 0) install.packages(new_packages)

library(eurostat)
library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(tibble)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("metadata/variables", recursive = TRUE, showWarnings = FALSE)
dir.create("metadata/dictionaries", recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)

datasets <- c(
  "bd_size",
  "env_esst_gg",
  "env_ac_ainah_r2",
  "env_ac_taxind2",
  "env_ac_pefa04",
  "env_ac_ccminv",
  "env_wasgen",
  "env_wat_cat",
  "rd_e_berdfundr2",
  "nama_10_a64",
  "nama_10_a64_e"
)

label_map <- tibble::tribble(
  ~dataset, ~variable_code, ~variable_name, ~sv_name, ~unit_code, ~unit_name, ~nace,
  "bd_size", "ENT_NR", "Enterprises - number", "Antal företag", NA_character_, NA_character_, "yes",
  "env_esst_gg", "TRF_CUR_D9", "Current and capital transfers (including subsidies)", "Statliga bidrag", "MIO_EUR", "Million euro", "Total economy",
  "env_esst_gg", "TAXABT", "Tax abatements", "Skatteutgifter", "MIO_EUR", "Million euro", "Total economy",
  "env_esst_gg", "TRF_CUR_D9", "Current and capital transfers (including subsidies)", "Statliga bidrag", "PC_GDP", "Percentage of gross domestic product (GDP)", "Total economy",
  "env_esst_gg", "TAXABT", "Tax abatements", "Skatteutgifter", "PC_GDP", "Percentage of gross domestic product (GDP)", "Total economy",
  "env_ac_ainah_r2", "GHG", "Greenhouse gases", "Växthusgaser", "THS_T", "Thousand tonnes", "yes",
  "env_ac_ainah_r2", "GHG", "Greenhouse gases", "Växthusgaser", "KG_HAB", "Kilograms per capita", "yes",
  "env_ac_taxind2", "ENV", "Total environmental taxes", "Totala miljöskatter", "MIO_EUR", "Million euro", "yes",
  "env_ac_taxind2", "NRG", "Energy taxes", "Energiskatter", "MIO_EUR", "Million euro", "yes",
  "env_ac_pefa04", "EPRD_ICNS", "Intermediate consumption of energy products", "energiprodukter som insatsvara", "TJ", "Terajoule", "yes",
  "env_ac_pefa04", "NETDOM_EUSE", "Net domestic energy use", "Inhemsk energianvändning, netto", "TJ", "Terajoule", "yes",
  "env_ac_ccminv", "CCM", "Climate change mitigation", "Klimatinvesteringar", "MIO_EUR", "Million euro", "yes",
  "env_ac_ccminv", "CCM", "Climate change mitigation", "Klimatinvesteringar", "PC_GDP", "Percentage of gross domestic product (GDP)", "yes",
  "env_wasgen", "TOTAL", "Total waste", "Avfallsgenerering", "KG_HAB", "Kilograms per capita", "yes",
  "env_wasgen", "TOTAL", "Total waste", "Avfallsgenerering", "T", "Tonnes", "yes",
  "env_wat_cat", "SOWS", "Self and other water supply", "Vattenanvändning", "MIO_M3", "Million cubic metres", "yes",
  "rd_e_berdfundr2", "BES", "Business enterprise sector", "FOU_företagsinvestering", "MIO_PPS", "Million purchasing power standards (PPS)", "yes",
  "rd_e_berdfundr2", "GOV", "Government sector", "FOU_statlig investering", "MIO_PPS", "Million purchasing power standards (PPS)", "yes",
  "rd_e_berdfundr2", "BES", "Business enterprise sector", "FOU_företagsinvestering", "PC_GDP", "Percentage of gross domestic product (GDP)", "yes",
  "rd_e_berdfundr2", "GOV", "Government sector", "FOU_statlig investering", "PC_GDP", "Percentage of gross domestic product (GDP)", "yes",
  "nama_10_a64", "B1G", "Value added, gross", "Förädlingsvärde", "CP_MEUR", "Current prices, million euro", "yes",
  "nama_10_a64", "B1G", "Value added, gross", "Förädlingsvärde", "CLV20_MEUR", "Chain linked volumes (2020), million euro", "yes",
  "nama_10_a64_e", "EMP_DC", "Total employment domestic concept", "Arbetade timmar", "THS_HW", "Thousand hours worked", "yes",
  "nama_10_a64_e", "EMP_DC", "Total employment domestic concept", "Sysselsatta", "THS_PER", "Thousand persons", "yes",
  "nama_10_a64", "D1", "Compensation of employees", "Löner och sociala avgifter", "CP_MEUR", "Current prices, million euro", "yes",
  "nama_10_a64", "D1", "Compensation of employees", "Löner och sociala avgifter", "CLV20_MEUR", "Chain linked volumes (2020), million euro", "yes"
)

safe_get <- function(ds){
  tryCatch(get_eurostat(id = ds, time_format = "num", cache = FALSE), error = function(e) NULL)
}

results <- purrr::map_dfr(datasets, function(ds){
  dat <- safe_get(ds)
  if(is.null(dat)) {
    return(tibble(dataset = ds, status = "FAILED", rows = NA_integer_, cols = NA_integer_))
  }
  
  write_csv(dat, file.path("data/raw", paste0(ds, ".csv")))
  saveRDS(dat, file.path("data/raw", paste0(ds, ".rds")))
  
  meta <- tibble(
    variable = names(dat),
    class = purrr::map_chr(dat, ~ paste(class(.x), collapse = "|")),
    n_unique = purrr::map_int(dat, ~ length(unique(.x)))
  )
  write_csv(meta, file.path("metadata/variables", paste0(ds, "_variables.csv")))
  
  dims <- setdiff(names(dat), c("time", "values"))
  purrr::walk(dims, function(v){
    dic <- tryCatch(get_eurostat_dic(v), error = function(e) NULL)
    if(!is.null(dic)) write_csv(dic, file.path("metadata/dictionaries", paste0(ds, "_", v, ".csv")))
  })
  
  tibble(dataset = ds, status = "SUCCESS", rows = nrow(dat), cols = ncol(dat))
})

write_csv(results, "download_log.csv")
write_csv(label_map, file.path("output", "variable_dictionary.csv"))



