#### Not used - species level cleaning of GBIF ####

## For phyla, classes, and species
no_phy_spec <- unique(c(which(is.na(GBIF_biv$phylum)),
                        which(is.na(GBIF_biv$class)),
                        which(is.na(GBIF_biv$species))))
if(length(no_phy_spec) > 0){
  GBIF_biv_species <- GBIF_biv[-no_phy_spec,]
} else {
  GBIF_biv_species <- GBIF_biv
}

## For phyla, classes, and species
no_phy_spec <- unique(c(which(is.na(GBIF_brach$phylum)),
                        which(is.na(GBIF_brach$class)),
                        which(is.na(GBIF_brach$species))))
if(length(no_phy_spec) > 0){
  GBIF_brach_species <- GBIF_brach[-no_phy_spec,]
} else {
  GBIF_brach_species <- GBIF_brach
}

## Next, prune out remaining entries not identified to species rank or lower (just in case we are missing any)
GBIF_biv_species <- GBIF_biv_species[which(GBIF_biv_species[,"taxonRank"] %in% c("SPECIES", "SUBSPECIES", "VARIETY", "FORM")),]
GBIF_brach_species <- GBIF_brach_species[which(GBIF_brach_species[,"taxonRank"] %in% c("SPECIES", "SUBSPECIES", "VARIETY", "FORM")),]

## Clean up species names, drop those not identified to species level
GBIF_biv_species$short_name <- cleansp(GBIF_biv_species$species, misspells=T, stems=T)
GBIF_biv_species <- GBIF_biv_species[!is.na(GBIF_biv_species$species),]

GBIF_brach_species$short_name <- cleansp(GBIF_brach_species$species, misspells=T, stems=T)
GBIF_brach_species <- GBIF_brach_species[!is.na(GBIF_brach_species$species),]

## Define unique name combining phyla and short name
GBIF_biv_species$unique_name <- paste(GBIF_biv_species$phylum, GBIF_biv_species$short_name)
GBIF_brach_species$unique_name <- paste(GBIF_brach_species$phylum, GBIF_brach_species$short_name)

## Run the function
GBIF_biv_species <- check.clean.metaData(GBIF_data = GBIF_biv_species, ISO.codes)
GBIF_brach_species <- check.clean.metaData(GBIF_data = GBIF_brach_species, ISO.codes)

## Export data
saveRDS(GBIF_biv_species, file = "data/GBIF_biv_species.Rds")
saveRDS(GBIF_brach_species, file = "data/GBIF_brach_species.Rds")

