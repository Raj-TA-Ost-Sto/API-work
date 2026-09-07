# Manual files to add

# SCB - 10
# Eurostat - 5
# OECD - ?

# Packages & import files 
library(readxl)
library(dplyr)
library(DDFConverter)
library(openxlsx)
library(tidyr)
library(purrr)
library(writexl)

a <- loadWorkbook("S:/Projekt/Tillväxtanalys/Arbete Hösten 2026/API kod/API-work-main/Manuella_variabler.xlsx")
sheetNames <- sheets(a)
for(i in 1:length(sheetNames)) {
  assign(sheetNames[i],readWorkbook(a,sheet = i))
}

# meta data
# ID <- read_xlsx("C:/Users/amelia.bodin/Documents/Tillväxtanalys/temp 20260428/ID-kopplingar.xlsx", 
#                 sheet = "Blad2")
# Länder <- read_xlsx("C:/Users/amelia.bodin/Documents/Tillväxtanalys/temp 20260428/ID-kopplingar.xlsx", 
#                     sheet = "Land")
# Bransch <- read_xlsx("C:/Users/amelia.bodin/Documents/Tillväxtanalys/temp 20260428/ID-kopplingar.xlsx", 
#                      sheet = "Bransch")
# Anv_kod <- read_xlsx("C:/Users/amelia.bodin/Documents/Tillväxtanalys/temp 20260428/ID-kopplingar.xlsx", 
#                      sheet = "Användningskod")
Länder <- read_xlsx("S:/Projekt/Tillväxtanalys/Vizabi/Uppdateringar 2026/Mars 2026/ID-kopplingar.xlsx", 
                                         sheet = "Land")
Bransch <- read_xlsx("S:/Projekt/Tillväxtanalys/Vizabi/Uppdateringar 2026/Mars 2026/ID-kopplingar.xlsx", 
                    sheet = "Bransch")
Anv_kod <- read_xlsx("S:/Projekt/Tillväxtanalys/Vizabi/Uppdateringar 2026/Mars 2026/ID-kopplingar.xlsx", 
                     sheet = "Användningskod")
# Clean data and merge with metadata (one at a time)

################################################################################
# Sveriges miljösubventioner per bransch
# Här ska bara tabellen med summor användas (domain är ingenting vi använder)
# Ta ut endast de cellerna 
bransch_miljösubv <- bransch_miljösubv[c(1:9),c(5:14)]

# Byt namn på kolumner
names(bransch_miljösubv)[1] <- "entity"
variable <- bransch_miljösubv[1, 1]
bransch_miljösubv$Variable <- variable
bransch_miljösubv <-
  bransch_miljösubv[, c(1, ncol(bransch_miljösubv), 2:(ncol(bransch_miljösubv) - 1))]
names(bransch_miljösubv)[3:ncol(bransch_miljösubv)] <-
  as.character(bransch_miljösubv[3, 3:ncol(bransch_miljösubv)])
bransch_miljösubv <- bransch_miljösubv[-c(1:3), -c(11)]

################################################################################
# Total fossil fuel support, % of total tax revenue											
fossilsubv <- fossilsubv[-c(3, 61:63),]
names(fossilsubv)[1] <- "entity"
fossilsubv$Variable  <- "Fossilsubventioner per totala skatter (procent)"
fossilsubv <- fossilsubv[, c(1, ncol(fossilsubv), 2:(ncol(fossilsubv) - 1))]
names(fossilsubv)[2:ncol(fossilsubv)] <- as.character(fossilsubv[2, 2:ncol(fossilsubv)])
fossilsubv <- fossilsubv[-c(1:2),]
fossilsubv[fossilsubv == ".."] <- NA 
names(fossilsubv)[2] <- "Variable"
# Ta bort white space i landnamn
fossilsubv$entity <- trimws(fossilsubv$entity)
fossilsubv$entity <- gsub(",", ":", fossilsubv$entity)

################################################################################
# Development of environment-related technologies, inventions per capita
names(miljö_patent)[1] <- "entity"
miljö_patent$Variable  <- "Miljöpatent per capita (antal patent / 1000)"
miljö_patent <- miljö_patent[, c(1, ncol(miljö_patent), 2:(ncol(miljö_patent) - 1))]
names(miljö_patent)[2:ncol(miljö_patent)] <- as.character(miljö_patent[2, 2:ncol(miljö_patent)])
miljö_patent <- miljö_patent[-c(1:2, 207),]
miljö_patent[miljö_patent == ".."] <- NA 
names(miljö_patent)[2] <- "Variable"
# Ta bort white space i landnamn
miljö_patent$entity <- trimws(miljö_patent$entity)
miljö_patent$entity <- gsub(",", ":", miljö_patent$entity)
################################################################################
# Environmentally related taxes, % GDP

names(miljöskatt_bnp)[1] <- "entity"
miljöskatt_bnp$Variable  <- "Miljörelaterade skatter som andel av BNP"
miljöskatt_bnp <- miljöskatt_bnp[, c(1, ncol(miljöskatt_bnp), 2:(ncol(miljöskatt_bnp) - 1))]
names(miljöskatt_bnp)[3:ncol(miljöskatt_bnp)] <- as.character(miljöskatt_bnp[4, 3:ncol(miljöskatt_bnp)])
miljöskatt_bnp <- miljöskatt_bnp[-c(1:4),]
miljöskatt_bnp[miljöskatt_bnp == ".."] <- NA 
# Ta bort white space i landnamn
miljöskatt_bnp$entity <- trimws(miljöskatt_bnp$entity)
miljöskatt_bnp$entity <- gsub(",", ":", miljöskatt_bnp$entity)


################################################################################

names(miljöskatt_total)[1] <- "entity"
miljöskatt_total$Variable  <- "Miljörelaterade skatter som procent av totala skatteintäkterna"
miljöskatt_total <- miljöskatt_total[, c(1, ncol(miljöskatt_total), 2:(ncol(miljöskatt_total) - 1))]
names(miljöskatt_total)[3:ncol(miljöskatt_total)] <- as.character(miljöskatt_total[4, 3:ncol(miljöskatt_total)])
miljöskatt_total <- miljöskatt_total[-c(1:4),]
miljöskatt_total[miljöskatt_total == ".." | miljöskatt_total == "U .."] <- NA 
# Ta bort white space i landnamn
miljöskatt_total$entity <- trimws(miljöskatt_total$entity)
miljöskatt_total$entity <- gsub(",", ":", miljöskatt_total$entity)


################################################################################
# Environmentally related R&D expenditure, % GDP
# r&d exp
# FOU per BNP

names(r_d_exp)[1] <- "entity"
r_d_exp$Variable  <- "FOU per BNP"
r_d_exp <- r_d_exp[, c(1, ncol(r_d_exp), 2:(ncol(r_d_exp) - 1))]
names(r_d_exp)[2:ncol(r_d_exp)] <- as.character(r_d_exp[2, 2:ncol(r_d_exp)])
r_d_exp <- r_d_exp[-c(1:2, 25:29),]
r_d_exp[r_d_exp == ".."] <- NA 
names(r_d_exp)[2] <- "Variable"
# Ta bort white space i landnamn
r_d_exp$entity <- trimws(r_d_exp$entity)
r_d_exp$entity <- gsub(",", ":", r_d_exp$entity)

################################################################################
# Utsläpp av koldioxid från tabeller och diagram årsluft statistiken (tidigare från körning till analysverktyg) = Utsläpp av koldioxid

Utsläpp_koldioxid <- Rådata_utsläpp_skattekrona[,1:17]

names(Utsläpp_koldioxid)[1] <- "entity"
Utsläpp_koldioxid$Variable  <- "Utsläpp av koldioxid"
Utsläpp_koldioxid <- 
  Utsläpp_koldioxid[, c(1, 2, ncol(Utsläpp_koldioxid), 3:(ncol(Utsläpp_koldioxid) - 1))]

names(Utsläpp_koldioxid)[2:ncol(Utsläpp_koldioxid)] <- 
  as.character(Utsläpp_koldioxid[2, 2:ncol(Utsläpp_koldioxid)])
Utsläpp_koldioxid <- Utsläpp_koldioxid[-c(1:2, 55:56),]
Utsläpp_koldioxid[Utsläpp_koldioxid == ".."] <- NA 
names(Utsläpp_koldioxid)[3] <- "Variable"
# Ta bort white space i landnamn
#Utsläpp_koldioxid$entity <- trimws(Utsläpp_koldioxid$Beskrivning)
Utsläpp_koldioxid$Beskrivning <- gsub(",", ":", Utsläpp_koldioxid$Beskrivning)
Utsläpp_koldioxid$Beskrivning <- gsub("*", "", Utsläpp_koldioxid$Beskrivning, fixed = TRUE)
Utsläpp_koldioxid$entity <- gsub("*", "", Utsläpp_koldioxid$entity, fixed = TRUE)

################################################################################
# Koldioxidskatt
Koldioxidskatt <- Rådata_utsläpp_skattekrona[,c(1:2, 18:32)]


names(Koldioxidskatt)[1] <- "entity"
Koldioxidskatt$Variable  <- "Koldioxidskatt"
Koldioxidskatt <- 
  Koldioxidskatt[, c(1, 2, ncol(Koldioxidskatt), 3:(ncol(Koldioxidskatt) - 1))]

names(Koldioxidskatt)[2:ncol(Koldioxidskatt)] <- 
  as.character(Koldioxidskatt[2, 2:ncol(Koldioxidskatt)])
Koldioxidskatt <- Koldioxidskatt[-c(1:2, 55:56),]
Koldioxidskatt[Koldioxidskatt == ".."] <- NA 
names(Koldioxidskatt)[3] <- "Variable"
# Ta bort white space i landnamn
#Koldioxidskatt$entity <- trimws(Koldioxidskatt$Beskrivning)
Koldioxidskatt$Beskrivning <- gsub(",", ":", Koldioxidskatt$Beskrivning)
Koldioxidskatt$Beskrivning <- gsub("*", "", Koldioxidskatt$Beskrivning, fixed = TRUE)
Koldioxidskatt$entity <- gsub("*", "", Koldioxidskatt$entity, fixed = TRUE)

################################################################################
# Utsläpp per skattekrona
utsläpp_skattekrona <- Rådata_utsläpp_skattekrona[,c(1:2, 33:47)]

names(utsläpp_skattekrona)[1] <- "entity"
utsläpp_skattekrona$Variable  <- "Utsläpp per skattekrona"
utsläpp_skattekrona <- 
  utsläpp_skattekrona[, c(1, 2, ncol(utsläpp_skattekrona), 3:(ncol(utsläpp_skattekrona) - 1))]

names(utsläpp_skattekrona)[2:ncol(utsläpp_skattekrona)] <- 
  as.character(utsläpp_skattekrona[2, 2:ncol(utsläpp_skattekrona)])
utsläpp_skattekrona <- utsläpp_skattekrona[-c(1:2, 55:56),]
utsläpp_skattekrona[utsläpp_skattekrona == ".."] <- NA 
names(utsläpp_skattekrona)[3] <- "Variable"
# Ta bort white space i landnamn
#utsläpp_skattekrona$entity <- trimws(utsläpp_skattekrona$Beskrivning)
utsläpp_skattekrona$Beskrivning <- gsub(",", ":", utsläpp_skattekrona$Beskrivning)
utsläpp_skattekrona$Beskrivning <- gsub("*", "", utsläpp_skattekrona$Beskrivning, fixed = TRUE)
utsläpp_skattekrona$entity <- gsub("*", "", utsläpp_skattekrona$entity, fixed = TRUE)
################################################################################

################################################################################
  # Sysselsatta
# Antal sysselsatta med miljörelaterad utbildning

miljörelaterad_utbildning <- Sysselsatta[,c(2:3,4)]

names(miljörelaterad_utbildning)[1] <- "entity"
miljörelaterad_utbildning$Variable  <- "Antal sysselsatta med miljörelaterad utbildning"
miljörelaterad_utbildning <- 
  miljörelaterad_utbildning[, c(1, 2, ncol(miljörelaterad_utbildning), 3:(ncol(miljörelaterad_utbildning) - 1))]

names(miljörelaterad_utbildning)[2:ncol(miljörelaterad_utbildning)] <- 
  c("Bransch", "Variable", "2022")
miljörelaterad_utbildning <- miljörelaterad_utbildning[-c(1:4),]

################################################################################
# Totalt antal sysselsatta
antal_sysselsatta <- Sysselsatta[,c(2:3,5)]

names(antal_sysselsatta)[1] <- "entity"
antal_sysselsatta$Variable  <- "Totalt antal sysselsatta"

antal_sysselsatta <- 
  antal_sysselsatta[, c(1, 2, ncol(antal_sysselsatta), 3:(ncol(antal_sysselsatta) - 1))]

names(antal_sysselsatta)[2:ncol(antal_sysselsatta)] <- 
  c("Bransch", "Variable", "2022")
antal_sysselsatta <- antal_sysselsatta[-c(1:4),]
################################################################################
# Totalt antal sysselsatta med examen
antal_sysselsatta_examen <- Sysselsatta[,c(2:3,6)]

names(antal_sysselsatta_examen)[1] <- "entity"
antal_sysselsatta_examen$Variable  <- "Totalt antal sysselsatta med examen"

antal_sysselsatta_examen <- 
  antal_sysselsatta_examen[, c(1, 2, ncol(antal_sysselsatta_examen), 3:(ncol(antal_sysselsatta_examen) - 1))]

names(antal_sysselsatta_examen)[2:ncol(antal_sysselsatta_examen)] <- 
  c("Bransch", "Variable", "2022")
antal_sysselsatta_examen <- antal_sysselsatta_examen[-c(1:4),]
################################################################################
# Andel med miljö examen av total examen 
miljö_examen_total_examen <- Sysselsatta[,c(2:3,7)]

names(miljö_examen_total_examen)[1] <- "entity"
miljö_examen_total_examen$Variable  <- "Andel med miljö examen av total examen"

miljö_examen_total_examen <- 
  miljö_examen_total_examen[, c(1, 2, ncol(miljö_examen_total_examen), 3:(ncol(miljö_examen_total_examen) - 1))]

names(miljö_examen_total_examen)[2:ncol(miljö_examen_total_examen)] <- 
  c("Bransch", "Variable", "2022")
miljö_examen_total_examen <- miljö_examen_total_examen[-c(1:4),]
################################################################################
# Andel miljöexamen av total
miljöexamen_av_total <- Sysselsatta[,c(2:3,8)]

names(miljöexamen_av_total)[1] <- "entity"
miljöexamen_av_total$Variable  <- "Andel miljöexamen av total"

miljöexamen_av_total <- 
  miljöexamen_av_total[, c(1, 2, ncol(miljöexamen_av_total), 3:(ncol(miljöexamen_av_total) - 1))]

names(miljöexamen_av_total)[2:ncol(miljöexamen_av_total)] <- 
  c("Bransch", "Variable", "2022")
miljöexamen_av_total <- miljöexamen_av_total[-c(1:4),]
################################################################################

################################################################################
# Utsläpp användning i leveransledet - MULTIDIM
Utsläpp_leverans_användning_multi <- Utsläpp_leverans_användning
names(Utsläpp_leverans_användning_multi)[1] <- "entity"
Utsläpp_leverans_användning_multi$Variable  <- "Växthusgasutsläpp i leveranskedjor: Område för slutlig användning"
Utsläpp_leverans_användning_multi <- 
  Utsläpp_leverans_användning_multi[, c(1, 2, ncol(Utsläpp_leverans_användning_multi), 3:(ncol(Utsläpp_leverans_användning_multi) - 1))]

names(Utsläpp_leverans_användning_multi)[2:ncol(Utsläpp_leverans_användning_multi)] <- 
  as.character(Utsläpp_leverans_användning_multi[11, 2:ncol(Utsläpp_leverans_användning_multi)])
names(Utsläpp_leverans_användning_multi)[2] <- "Anv_område"
names(Utsläpp_leverans_användning_multi)[3] <- "Variable"
Utsläpp_leverans_användning_multi <- Utsläpp_leverans_användning_multi[-c(1:11),-c(19:20)]
Utsläpp_leverans_användning_multi[Utsläpp_leverans_användning_multi == ".."] <- NA 

# ONLY LAND
Utsläpp_leverans_användning <- Utsläpp_leverans_användning_multi %>%
  select(-c(Anv_område)) %>%
  pivot_longer(cols = 3:17,
               values_to = "Värde",
               names_to = "År") %>%
  mutate(Värde = as.numeric(Värde)) %>%
  summarise(Värde = sum(Värde, na.rm = TRUE),
            .by = c(Variable, entity, År))

Utsläpp_leverans_användning <- Utsläpp_leverans_användning %>%
  pivot_wider(names_from = År, values_from = Värde)


################################################################################
# Utsläpp produktion i leveransledet - MULTIDIM
Utsläpp_leverans_prod_multi <- Utsläpp_leverans_prod
names(Utsläpp_leverans_prod_multi)[1] <- "entity"
Utsläpp_leverans_prod_multi$Variable  <- "Växthusgasutsläpp i leveranskedjor: Bransch där produktionsutsläpp uppstår"
Utsläpp_leverans_prod_multi <- 
  Utsläpp_leverans_prod_multi[, c(1, 2, ncol(Utsläpp_leverans_prod_multi), 3:(ncol(Utsläpp_leverans_prod_multi) - 1))]

names(Utsläpp_leverans_prod_multi)[2:ncol(Utsläpp_leverans_prod_multi)] <- 
  as.character(Utsläpp_leverans_prod_multi[5, 2:ncol(Utsläpp_leverans_prod_multi)])
names(Utsläpp_leverans_prod_multi)[2] <- "Bransch"
names(Utsläpp_leverans_prod_multi)[3] <- "Variable"
Utsläpp_leverans_prod_multi <- Utsläpp_leverans_prod_multi[-c(1:5),]
Utsläpp_leverans_prod_multi[Utsläpp_leverans_prod_multi == ".."] <- NA 


# ONLY LAND
Utsläpp_leverans_prod <- Utsläpp_leverans_prod_multi %>%
  select(-c(Bransch)) %>%
  pivot_longer(cols = 3:17,
               values_to = "Värde",
               names_to = "År") %>%
  mutate(Värde = as.numeric(Värde)) %>%
  summarise(Värde = sum(Värde, na.rm = TRUE),
            .by = c(Variable, entity, År))

Utsläpp_leverans_prod <- Utsläpp_leverans_prod %>%
  pivot_wider(names_from = År, values_from = Värde)


################################################################################

# Utsläpp konsumption i leveransledet - MULTIDIM
Utsläpp_leverans_kons_multi <- Utsläpp_leverans_kons
names(Utsläpp_leverans_kons_multi)[1] <- "entity"
Utsläpp_leverans_kons_multi$Variable  <- "Växthusgasutsläpp i leveranskedjor: Produktform för slutlig användning"
Utsläpp_leverans_kons_multi <- 
  Utsläpp_leverans_kons_multi[, c(1, 2, ncol(Utsläpp_leverans_kons_multi), 3:(ncol(Utsläpp_leverans_kons_multi) - 1))]

names(Utsläpp_leverans_kons_multi)[2:ncol(Utsläpp_leverans_kons_multi)] <- 
  as.character(Utsläpp_leverans_kons_multi[5, 2:ncol(Utsläpp_leverans_kons_multi)])
names(Utsläpp_leverans_kons_multi)[2] <- "Bransch"
names(Utsläpp_leverans_kons_multi)[3] <- "Variable"
Utsläpp_leverans_kons_multi <- Utsläpp_leverans_kons_multi[-c(1:5),]
Utsläpp_leverans_kons_multi[Utsläpp_leverans_kons_multi == ".."] <- NA 

# ONLY LAND
Utsläpp_leverans_kons <- Utsläpp_leverans_kons_multi %>%
  select(-c(Bransch)) %>%
  pivot_longer(cols = 3:17,
               values_to = "Värde",
               names_to = "År") %>%
  mutate(Värde = as.numeric(Värde)) %>%
  summarise(Värde = sum(Värde, na.rm = TRUE),
            .by = c(Variable, entity, År))

Utsläpp_leverans_kons <- Utsläpp_leverans_kons %>%
  pivot_wider(names_from = År, values_from = Värde)


################################################################################
# Utsläpp produktion
names(utsläpp_prod)[1] <- "entity"
utsläpp_prod$Variable  <- "Utsläpp av GHG i samband med produktionsleden"
utsläpp_prod <- utsläpp_prod[, c(1, ncol(utsläpp_prod), 2:(ncol(utsläpp_prod) - 1))]

names(utsläpp_prod)[2:ncol(utsläpp_prod)] <- 
  as.character(utsläpp_prod[1, 2:ncol(utsläpp_prod)])
utsläpp_prod <- utsläpp_prod[-c(1),]
utsläpp_prod[utsläpp_prod == ".."] <- NA 
names(utsläpp_prod)[2] <- "Variable"
################################################################################
# Kemikalier inbäddade i produkter

colnames(kem_inb) <- c("entity", "Bransch", "År", "Värde")

# MULTIDIMENSIONAL
Kemikalier_multidim <- kem_inb %>%
  pivot_wider(names_from = c(År), values_from = Värde) %>% 
  mutate(Variable = "Kemikalier inbäddade i produkter") %>% 
  select(entity, Bransch, Variable, everything())

# ONLY COUNTRY
Kemikalier <- kem_inb %>%
  mutate(Variable = "Kemikalier inbäddade i produkter") %>%
#  select(Variable, Land, År, Värde) %>%
  mutate(Värde = as.numeric(Värde)) %>%
  summarise(Värde = sum(Värde, na.rm = TRUE),
            .by = c(Variable, entity, År)) 
Kemikalier <- Kemikalier %>%
  pivot_wider(names_from = År, values_from = Värde)

################################################################################



# Split into bransch /land files 

# BRANSCH
bransch_sets <- list(
bransch_miljösubv,
Utsläpp_koldioxid,
Koldioxidskatt,
utsläpp_skattekrona,
miljörelaterad_utbildning,
antal_sysselsatta,
antal_sysselsatta_examen,
miljöexamen_av_total,
miljö_examen_total_examen,
utsläpp_prod)

# Turn all 'year'-column to numeric
bransch_sets <- bransch_sets %>%
  map( ~ .x %>%
         mutate(across(matches("^\\d{4}$"),
                       ~ as.numeric(.))))


final_bransch_dataset <- bransch_sets %>%
  map( ~ .x %>%
         pivot_longer(
           cols = matches("^\\d{4}$"),
           names_to = "year",
           values_to = "value"
         )) %>%
  bind_rows() %>%
  pivot_wider(names_from = year,
              values_from = value)

# LAND
land_sets <- list(
fossilsubv,
miljö_patent,
miljöskatt_bnp,
miljöskatt_total,
r_d_exp,
Utsläpp_leverans_användning,
Utsläpp_leverans_kons,
Utsläpp_leverans_prod,
Kemikalier)

# Turn all 'year'-column to numeric
land_sets <- land_sets %>%
  map( ~ .x %>%
         mutate(across(matches("^\\d{4}$"),
                       ~ as.numeric(.))))

final_land_dataset <- land_sets %>%
  map( ~ .x %>%
         pivot_longer(
           cols = matches("^\\d{4}$"),
           names_to = "year",
           values_to = "value"
         )) %>%
  bind_rows() %>%
  pivot_wider(names_from = year,
              values_from = value)


# MULTIDIM LAND (4 DFS)
################################################################################
multi_sets <- list(
Kemikalier_multidim,
Utsläpp_leverans_användning_multi,
Utsläpp_leverans_kons_multi,
Utsläpp_leverans_prod_multi)

# Turn all 'year'-column to numeric
multi_sets <- multi_sets %>%
  map( ~ .x %>%
         mutate(across(matches("^\\d{4}$"),
                       ~ as.numeric(.))))

final_land_dataset_multi <- multi_sets %>%
  map( ~ .x %>%
         pivot_longer(
           cols = matches("^\\d{4}$"),
           names_to = "year",
           values_to = "value"
         )) %>%
  bind_rows() %>%
  pivot_wider(names_from = year,
              values_from = value)

# Merge with metadata
################################################################################


robust_vlookup <- function(keys, lookup_table, lookup_key_col, return_cols,
                           ignore_case = TRUE, trim = TRUE, fill_na = NA) {
  # keys: vector of keys to look up (e.g., dataset$entity)
  # lookup_table: data.frame containing lookup data (e.g., Bransch)
  # lookup_key_col: name of the key column in lookup_table (string)
  # return_cols: vector of column names in lookup_table to return
  # ignore_case: convert to upper case for matching
  # trim: remove leading/trailing spaces
  # fill_na: value to fill when no match is found
  
  # Copy the lookup table to avoid modifying original
  lookup <- lookup_table
  
  # Convert to character
  lookup[[lookup_key_col]] <- as.character(lookup[[lookup_key_col]])
  keys <- as.character(keys)
  
  # Trim spaces if requested
  if (trim) {
    keys <- trimws(keys)
    lookup[[lookup_key_col]] <- trimws(lookup[[lookup_key_col]])
  }
  
  # Ignore case if requested
  if (ignore_case) {
    keys <- toupper(keys)
    lookup[[lookup_key_col]] <- toupper(lookup[[lookup_key_col]])
  }
  
  # Replace common non-breaking spaces with normal space
  keys <- gsub("\u00A0", " ", keys)
  lookup[[lookup_key_col]] <- gsub("\u00A0", " ", lookup[[lookup_key_col]])
  
  # Perform match
  idx <- match(keys, lookup[[lookup_key_col]])
  
  # Extract return columns
  result <- sapply(return_cols, function(col) {
    if (col %in% colnames(lookup)) {
      val <- lookup[[col]][idx]
      val[is.na(idx)] <- fill_na  # fill unmatched keys
      return(val)
    } else {
      # If column not found, return NA
      return(rep(fill_na, length(keys)))
    }
  }, simplify = FALSE)
  
  # Return as a data.frame
  result_df <- as.data.frame(result, stringsAsFactors = FALSE)
  
  return(result_df)
}
# Bransch
lookup_result <- robust_vlookup(
  keys = final_bransch_dataset$entity,
  lookup_table = Bransch,
  lookup_key_col = "name",
  return_cols = c("name", "bransch_id")   # only pick the column you need
)

final_bransch_dataset <- final_bransch_dataset %>% 
  cbind(lookup_result) %>%
  select(Variable, entity, bransch_id, everything())%>% 
  select(-c(name))


# Land
lookup_result <- robust_vlookup(
  keys = final_land_dataset$entity,
  lookup_table = Länder,
  lookup_key_col = "name",
  return_cols = c("name", "country")   # only pick the column you need
)

final_land_dataset <- final_land_dataset %>% 
  cbind(lookup_result) %>%
  select(Variable, entity, country, everything())%>% 
  select(-c(name))


# Multidimensional country
lookup_result_country <- robust_vlookup(
  keys = final_land_dataset_multi$entity,
  lookup_table = Länder,
  lookup_key_col = "name",
  return_cols = c("name", "country")   # only pick the column you need
)

# Multidimensional usage code
lookup_result_anv_omr <- robust_vlookup(
  keys = final_land_dataset_multi$Anv_område,
  lookup_table = Anv_kod,
  lookup_key_col = "Anvkod",
  return_cols = c("Anvkod", "usage_code")   # only pick the column you need
)

# Multidimensional bransch
lookup_result_bransch <- robust_vlookup(
  keys = final_land_dataset_multi$Bransch,
  lookup_table = Bransch,
  lookup_key_col = "name",
  return_cols = c("name", "bransch_id")   # only pick the column you need
)

final_land_dataset_multi <- final_land_dataset_multi %>% 
  cbind(lookup_result_country) %>%
  select(entity, country, Variable, everything()) %>% 
  select(-c(name))

final_land_dataset_multi <- final_land_dataset_multi %>% 
  cbind(lookup_result_anv_omr, lookup_result_bransch) %>%
  select(Variable, entity, country, usage_code, Anv_område, bransch_id, Bransch, everything()) %>% 
  select(-c(name, Anvkod))


# Save as Excel-files
write_xlsx(final_bransch_dataset, "S:/Projekt/Tillväxtanalys/Arbete Hösten 2026/API kod/API-work-main/Manuella_branscher.xlsx")
write_xlsx(final_land_dataset, "S:/Projekt/Tillväxtanalys/Arbete Hösten 2026/API kod/API-work-main/Manuella_länder.xlsx")
write_xlsx(final_land_dataset_multi, "S:/Projekt/Tillväxtanalys/Arbete Hösten 2026/API kod/API-work-main/Manuella_länder_multidim.xlsx")
