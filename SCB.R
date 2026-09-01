# SCB API

#Install and load packages
devtools::install_github("vilgothub/scbR")
library(scbR)
library(dplyr)
library(tidyr)
library(readxl)
library(purrr)
library(DDFConverter)

################################TO DO###########################################



# Load the list of API-URLS into R
URL_list <-
  read_xlsx(
    "C:/Users/amelia.bodin/Documents/Tillväxtanalys/Uppdateringar 2026/API augusti/SCB_API_URL.xlsx"
  )


#call_API_bransch <- c()
call_API_land <- c()
# Create a loop for every item, call get_scb_data and save
for (i in 1:nrow(URL_list)) {
  if (URL_list$Verktyg[i] == "land") {
    url <- as.character(URL_list$URL[i])
    call_API_land <- get_scb_data(url)

  } else {
    next
  }
}



entity_id_vars <- c(
  "Handelspartner", #only country
  "SNI2007",
  "SNI2007MI"
)

entity_name_vars <- c(
  "handelspartner", #only country
  "näringsgren SNI 2007",
  "näringsgren enligt SNI 2007"
)

dim_id_vars <- c(
  "Region",
  "Kon",
  "Fodelseregion",
  "Sektor",
  "Bransle",
  "Kostnadsslag",
  "TypavFoU",
  "OfarligtFarligt",
  "Avfallsslag",
  "TypAnv",
  "AmneMiljo",
  "MiljoomradeN",
  "MilSkattNiv",
  "Resultatraknposter"
)

dim_name_vars <- c(
  "region",
  "kön",
  "födelseregion",
  "sektor",
  "bränsletyp",
  "kostnadsslag",
  "typ av FoU",
  "egenskap",
  "avfallsslag enligt EWC-Stat",
  "typ av användning",
  "ämne",
  "miljöområde",
  "miljöskattenivå",
  "resultaträkningsposter"
)

remove_vars <- c(
  "datum"
)


process_api <- function(dat) {
  
  # Entities
  entity_id <- intersect(entity_id_vars, names(dat))
  entity_name <- intersect(entity_name_vars, names(dat))
  
  if (length(entity_id) > 0) {
    dat <- dat %>%
      rename(entity_id = all_of(entity_id[1]))
  }
  
  if (length(entity_name) > 0) {
    dat <- dat %>%
      rename(entity_name = all_of(entity_name[1]))
  }
  
  

  # Dimensions associations
   dimensions <- list(
    Region = "region",
    Kon = "kon",
    Fodelseregion = "fodelseregion",
    Sektor = "sektor",
    Bransle = "bransle",
    Kostnadsslag = "kostnadsslag",
    TypavFoU = "typavfou",
    OfarligtFarligt = "ofarligtfarligt",
    Avfallsslag = "avfallsslag",
    TypAnv = "typanv",
    AmneMiljo = "amnemiljo",
    MiljoomradeN = "miljoomrade",
    MilSkattNiv = "miljoskatteniva",
    Resultatraknposter = "resultatraknposter"
  )

  for (dim_id in names(dimensions)) {
    
    dim_name <- dimensions[[dim_id]]
    
    if (dim_id %in% names(dat)) {
      
      name_var <- dim_name_vars[
        match(dim_id, dim_id_vars)
      ]
      
      if (!is.na(name_var) && name_var %in% names(dat)) {
        
        dat <- dat %>%
          rename(
            !!paste0(dim_name, "_id") := all_of(dim_id),
            !!paste0(dim_name, "_name") := all_of(name_var)
          )
      }
    }
  }
  
  
  year_vars <- c(
    "Tid",
    "år",
    "år, oregelb",
    "vartannat år"
  )
  
  present_year <- intersect(year_vars, names(dat))
 
  # If "Tid" is present, use that, otherwise use whichever other is available 
  if (length(present_year) > 0) {
    
    if ("Tid" %in% present_year) {
      year_var <- "Tid"
    } else {
      year_var <- present_year[1]
    }
    
    dat <- dat %>%
      rename(year = all_of(year_var)) %>%
      select(-any_of(setdiff(present_year, year_var)))
  }
  
  dat <- dat %>%
    select(-any_of(remove_vars))
  
  id_vars <- c(
    intersect("entity_id", names(dat)),
    intersect("entity_name", names(dat)),
    grep("_(id|name)$", names(dat), value = TRUE),
    intersect("year", names(dat))
  )
  
  measure_vars <- setdiff(names(dat), id_vars)
  
  dat %>%
    pivot_longer(
      cols = all_of(measure_vars),
      names_to = "measure",
      values_to = "value"
    )
}



final_data <- URL_list %>%
  filter(Verktyg == "bransch") %>%
  pull(URL) %>%
  map_dfr(function(url) {
    
    print(url)
    
    get_scb_data(url) %>%
      process_api()
  })


id_cols <- setdiff(names(final_data), c("year", "value"))

final_data2 <- final_data %>%
  pivot_wider(
    id_cols = all_of(id_cols),
    names_from = year,
    values_from = value
  ) %>% 
  mutate(measure_id = tolower(substr(measure, 1, 5))) 
# %>% # Create variable IDs
  # mutate(across(entity_name, ~gsub(",", ":", .))) %>%  # Replace commas with colons
  # mutate(measure = gsub(
  #   "^([^,]+), ([^,]+), (.+)$",
  #   "\\1: \\2 (\\3)",
  #   measure
  # )) # Make the format Export: total (tkr)

# Lägg in TAGS

final_data2 <- 
  final_data2 %>% 
  mutate(tag_id = "a",
         tag_name = "Bransch A")

# Create DDF

create_multidimensional_ddf(
  dataset = final_data2,
  variable_id = "measure_id",
  variable_name = "measure",
  entity_id = "entity_id",
  entity_name = "entity_name",
  datacolumns = "32-77",
  tag_id = "tag_id",
  tag_name = "tag_name",
  multidim_id = c("region_id",
                  "kon_id",
                  "fodelseregion_id",
                  "resultatraknposter_id",
                  "miljoskatteniva_id",
                  "bransle_id",
                  "kostnadsslag_id",
                  "typavfou_id",
                  "ofarligtfarligt_id",
                  "avfallsslag_id",
                  "typanv_id",
                  "amnemiljo_id",
                  "sektor_id",
                  "miljoomrade_id"
                  ),
  multidim_name = c("region_name",
                    "kon_name",
                    "fodelseregion_name",
                    "resultatraknposter_name",
                    "miljoskatteniva_name",
                    "bransle_name",
                    "kostnadsslag_name",
                    "typavfou_name",
                    "ofarligtfarligt_name",
                    "avfallsslag_name",
                    "typanv_name",
                    "amnemiljo_name",
                    "sektor_name",
                    "miljoomrade_name"
  )
)
# 
# 
# create_multidimensional_datapoints(
#     dataset = final_data2,
#     variable_id = "measure_id",
#     entity_id = "entity_id",
#     datacolumns = "32-77",
#     multidimension = "region_id")
# 
# 


# Sort the variable names into correct groups (and remove extras)
# var_name = measure
# entity_name = label
# entity_id = code
# dim = what dimension/entity type (multidim)
# year columns = measure values

# Land data

colnames(call_API_land) <- c("entity_id",	"entity_name",	"Tid",	"år",	"datum",
                             "Varuimport från avsändningsland, totala värden, tkr",	"Varuexport till bestämmelseland, totala värden, tkr"
)

land_data <- call_API_land %>% 
  select(entity_id, entity_name,
         Tid, `Varuexport till bestämmelseland, totala värden, tkr`,
         `Varuimport från avsändningsland, totala värden, tkr`) %>% 
  pivot_longer(
    cols = (c(4:5)),               # specify identifier columns
    names_to = "measure",             # the name for the year column
    values_to = "value"            # the name for the value column
  ) %>% 
  pivot_wider(
    id_cols = c(entity_id, entity_name, measure),
    names_from = "Tid",
    values_from = "value"
  ) %>% 
  mutate(measure_id = tolower(substr(measure, 1, 10))) %>% # Create variable IDs
  mutate(across(entity_name, ~gsub(",", ":", .))) %>%  # Replace commas with colons
  mutate(measure = gsub(
    "^([^,]+), ([^,]+), (.+)$",
    "\\1: \\2 (\\3)",
    measure
  )) # Make the format Export: total (tkr)


# Save SCB-data locally in own folder | split country and sector data
write.csv(final_data2, file = "SCB_bransch.csv", row.names = FALSE, quote = FALSE)
write.csv(land_data, file = "SCB_land.csv", row.names = FALSE, quote = FALSE)
