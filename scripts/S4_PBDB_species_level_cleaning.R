#### Not used - species level cleaning of PBDB ####

## Cleaning previously-identified (using divDyn cleansp function - when it didn't work!) special characters - umlaut
## Get all binomials beginning with Inoceramus sch
inoceramus_sch <- grep("Inoceramus sch",PBDB[,5], useBytes = T)

## Remove other 3 then replace remaining binomials with R friendly spelling
inoceramus_sch <- inoceramus_sch[!inoceramus_sch %in% grep("Inoceramus schluetheri",PBDB[,5], useBytes = T)]
inoceramus_sch <- inoceramus_sch[!inoceramus_sch %in% grep("Inoceramus schoendorfi",PBDB[,5], useBytes = T)]
inoceramus_sch <- inoceramus_sch[!inoceramus_sch %in% grep("Inoceramus schmidti",PBDB[,5], useBytes = T)]
PBDB[inoceramus_sch,5] <- "Inoceramus schoendorfi"

## After line 141 of script 1_5
## For phyla, classes, and species
no_phy_spec <- unique(c(which(PBDB$phylum == ""),
                        which(PBDB$class == ""),
                        which(PBDB$species == ""),
                        which(is.na(PBDB$phylum)),
                        which(is.na(PBDB$class)),
                        which(is.na(PBDB$species))))
if(length(no_phy_spec) > 0){
  PBDB_species <- PBDB[-no_phy_spec,]
} else {
  PBDB_species <- PBDB
}

## Species only
## Check accepted ranks
unique(PBDB$accepted_rank)

## Data only contains genus and species level entries. Start with identified rank
PBDB_species <- PBDB_species[PBDB_species$identified_rank=="species",]

## Clean up identified names, drop those not identified to species level
PBDB_species$short_name <- cleansp(PBDB_species$identified_name, misspells=T, stems=T)
PBDB_species <- PBDB_species[!is.na(PBDB_species$short_name),]

## Clean up accepted names, drop points not accepted
PBDB_species$accepted_name <- cleansp(PBDB_species$accepted_name, misspells=T, stems=T)
PBDB_species <- PBDB_species[!is.na(PBDB_species$accepted_name),]

## Define unique name combining phyla and accepted name. Should be complete as only working with accepted names, so no need to use short names
PBDB_species$unique_name <- paste(PBDB_species$phylum, PBDB_species$accepted_name)
#unac <- which(!PBDB_species$accepted_rank %in% "species")
#PBDB_species$unique_name[unac] <- paste(PBDB_species$phylum[unac], PBDB_species$short_name[unac])

## After line 147
## Change mixed lithologies to NA
PBDB_species$lithology1[grep('mixed', PBDB_species$lithology1)] <- NA

## Adding covariates
PBDB_species <- add.occ.covariate(PBDB_species, name = "occLith", ref = "lithology1", varsLabs = c("carb", "sili"), var1 = carb, var2 = clast)
PBDB_species <- add.occ.covariate(PBDB_species, name = "occEnv", ref = "environment", varsLabs = c("shal", "deep"), var1 = shallow, var2 = deep)
PBDB_species <- add.occ.covariate(PBDB_species, name = "occReef", ref = "environment", varsLabs = c("reef", "noRf"), var1 = reefal, var2 = nonreefal)

## Add ecology IDs
PBDB_species <- add.ecology.IDs(data = PBDB_species, key)

## Filter out entries with no ecology data
PBDB_species_eco <- PBDB_species[which(!is.na(PBDB_species[,"ecological_cat"])),]

## Reference filtering
species_RefRef <- refine.references(data = PBDB_species, level = "unique_name", quantiles = c(0.05, 0.95), drop.ref.singletons.first = T)

