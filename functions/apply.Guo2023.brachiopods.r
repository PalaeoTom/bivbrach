apply.Guo2023.brachiopods <- function(brach, GTS2020){
  brach[which(brach$genus=="Acanthorhynchia (Echinirhynchia)"),"genus"]<-c("Echinirhynchia")
  brach[which(brach$genus=="Acanthorhynchia (Acanthorhynchia)"),"genus"]<-c("Acanthorhynchia")
  brach[which(brach$genus=="Arzonella"),"genus"]<-c("Arzonellina") #The former is replaced by the latter
  brach[which(brach$genus=="Athyris (Seminula)"),"genus"]<-c("Composita")
  brach[which(brach$genus=="Athyris (Spirigera)"),"genus"]<-c("Athyris")
  brach[which(brach$genus=="Burmirhynchia (Burmirhynchia)"),"genus"]<-c("Burmirhynchia")
  brach[which(brach$genus=="Burmirhynchia (Hopkinsirhynchia)"),"genus"]<-c("Burmirhynchia")
  brach[which(brach$genus=="Chonetes (Paeckelmannia)"),"genus"]<-c("Tornquistia")
  brach[which(brach$genus=="Chonetina"),"genus"]<-c("Chonetella")
  brach[which(brach$genus=="Cretirhynchia (Harmignirhynchia)"),"genus"]<-c("Harmignirhynchia")
  brach[which(brach$genus=="Cretirhynchia (Lewesirhynchia)"),"genus"]<-c("Lewesirhynchia")
  brach[which(brach$genus=="Cretirhynchia (Cretirhynchia)"),"genus"]<-c("Cretirhynchia")
  brach[which(brach$genus=="Cyrtina (Cyrtina)"),"genus"]<-c("Cyrtina")
  brach[which(brach$genus=="Dictyothyris (Tegulithyris)"),"genus"]<-c("Tegulithyris")
  brach[which(brach$genus=="Dyoros (Dyoros)"),"genus"]<-c("Dyoros")
  brach[which(brach$genus=="Dyoros (Lissosia)"),"genus"]<-c("Dyoros")
  brach[which(brach$genus=="Dyoros (Tetragonetes)"),"genus"]<-c("Dyoros")
  brach[which(brach$genus=="Eudesia (Eudesia)"),"genus"]<-c("Eudesia")
  brach[which(brach$genus=="Eudesia (Sphriganaria)"),"genus"]<-c("Eudesia")
  brach[which(brach$genus=="Sphriganaria"),"genus"]<-c("Eudesia") #Sphriganaria was regarded as an independent genus in Treatise. It is regarded as a synonym of Eudesia following Almeras 2010
  brach[which(brach$genus=="Lacunosella (Lacunosella)"),"genus"]<-c("Lacunosella")
  brach[which(brach$genus=="Dichotomosella"),"genus"]<-c("Lacunosella")
  brach[which(brach$genus=="Lacunosella (Dichotomosella)"),"genus"]<-c("Lacunosella")
  brach[which(brach$genus=="Leiorhynchus (Leiorhynchus)"),"genus"]<-c("Leiorhynchus")
  brach[which(brach$genus=="Marginifera (Arenaria)"),"genus"]<-c("Marginifera")
  brach[which(brach$genus=="Marginifera (Marginifera)"),"genus"]<-c("Marginifera")
  brach[which(brach$genus=="Neochonetes (Huangichonetes)"),"genus"]<-c("Neochonetes")
  brach[which(brach$genus=="Neochonetes (Neochonetes)"),"genus"]<-c("Neochonetes")
  brach[which(brach$genus=="Neochonetes (Nongtaia)"),"genus"]<-c("Neochonetes")
  brach[which(brach$genus=="Neochonetes (Sommeriella)"),"genus"]<-c("Neochonetes")
  brach[which(brach$genus=="Neochonetes (Zechiella)"),"genus"]<-c("Neochonetes")
  brach[which(brach$genus=="Neochonetes (Zhongyingia)"),"genus"]<-c("Neochonetes")
  brach[which(brach$genus=="Neospirifer (Neospirifer)"),"genus"]<-c("Neospirifer")
  brach[which(brach$genus=="Neospirifer (Quadrospira)"),"genus"]<-c("Quadrospira")
  brach[which(brach$genus=="Ornithella (Delmontanella)"),"genus"]<-c("Delmontanella")
  brach[which(brach$genus=="Productus (Buxtonia)"),"genus"]<-c("Buxtonia")
  brach[which(brach$genus=="Productus (Ruthenia)"),"genus"]<-c("Waagenoconcha")
  brach[which(brach$genus=="Ptilorhynchia (Proteorhynchia)"),"genus"]<-c("Proteorhynchia")
  brach[which(brach$genus=="Rhynchonelloidea (Aalenirhynchia)"),"genus"]<-c("Rhynchonelloidea")
  brach[which(brach$genus=="Schizophoria (Schizophoria)"),"genus"]<-c("Schizophoria")
  brach[which(brach$genus=="Spiriferina (Mansuyella)"),"genus"]<-c("Callispirina")
  brach[which(brach$genus=="Spiriferina (Mentzelia)"),"genus"]<-c("Mentzelia")
  brach[which(brach$genus=="Stolmorhynchia (Praelacunosella)"),"genus"]<-c("Stolmorhynchia")
  brach[which(brach$genus=="Strophomena (Strophodonta)"),"genus"]<-c("Strophodonta")
  brach[which(brach$genus=="Terebratula (Waldheimia)"),"genus"]<-c("Terebratula")
  brach[which(brach$genus=="Tetrarhynchia (Rostrirhynchia)"),"genus"]<-c("Tetrarhynchia")
  brach[which(brach$genus=="Waagenoconcha (Gruntoconcha)"),"genus"]<-c("Waagenoconcha")
  brach[which(brach$genus=="Zeilleria (Zeilleria)"),"genus"]<-c("Zeilleria")
  brach[which(brach$genus=="Anchorhynchia"),"genus"]<-c("Ancorhynchia")
  brach[which(brach$genus=="Bakonithyris"),"genus"]<-c("Bakonyithyris")
  brach[which(brach$genus=="Choanoproductus"),"genus"]<-c("Monticulifera") #Following Waterhouse 2013
  brach[which(brach$genus=="Cryptospirifer"),"genus"]<-c("Permocryptospirifer")
  brach[which(brach$genus=="Glossothyris"),"genus"]<-c("Nucleata")
  brach[which(brach$genus=="Echyrosia"),"genus"]<-c("Burmirhynchia") #Following Almeras 2010
  brach[which(brach$genus=="Eurysites"),"genus"]<-c("Burmirhynchia") #Following Almeras 2010
  brach[which(brach$genus=="Glabauria"),"genus"]<-c("Echinalosia") #A subgenus of Echinalosia according to Waterhouse 2013
  brach[which(brach$genus=="Gubleria"),"genus"]<-c("Leptodus")
  brach[which(brach$genus=="Hectoria"),"genus"]<-c("Clavigera")
  brach[which(brach$genus=="Kotlaia"),"genus"]<-c("Acosarina") #Following Shen and Shi 2007
  brach[which(brach$genus=="Ladoliplica"),"genus"]<-c("Martinia")
  brach[which(brach$genus=="Lengwuella"),"genus"]<-c("Pseudolabaia") #Shen et al. 2017
  brach[which(brach$genus=="Pseudojisuina"),"genus"]<-c("Pseudolabaia") #Shen et al. 2017
  brach[which(brach$genus=="Lingula"),"genus"]<-c("Lingularia") #Many Late Paleozoic and Mesozoic lingulids may be better referred to Lingularia (see Treatise)
  brach[which(brach$genus=="Lissorhynchia"&brach$min_ma>=GTS2020[which(GTS2020$Interval=="Triassic"),"FAD"]),"genus"]<-c("Prelissorhynchia")
  brach[which(brach$genus=="Magniderbyia"),"genus"]<-c("Derbyia") #Following Treatise
  brach[which(brach$genus=="Miniliconcha"),"genus"]<-c("Carilya") #Waterhouse 2013
  brach[which(brach$genus=="Orthotetina"),"genus"]<-c("Orthothetina")
  brach[which(brach$genus=="Paramonticulifera"),"genus"]<-c("Monticulifera")
  brach[which(brach$genus=="Rectambitus"),"genus"]<-c("Juxathyris") #Following Shen et al. 2017
  brach[which(brach$genus=="Retaria"),"genus"]<-c("Kutorginella")
  brach[which(brach$genus=="Sanqiaothyris"),"genus"]<-c("Rhaetina")
  brach[which(brach$genus=="Rhaetinopsis"),"genus"]<-c("Rhaetina")
  brach[which(brach$genus=="Striatospica"),"genus"]<-c("Edriosteges") #Following Shen et al. 2017
  brach[which(brach$genus=="Thecidium"),"genus"]<-c("Thecidea")
  brach[which(brach$identified_name=="Eudesia (Xenorina) sp."),"genus"]<-c("Xenorina")
  brach[which(brach$primary_name=="Chonostegoidella"),"genus"]<-c("Chonostegoidella") #Following Shen et al. 2017. This genus was treated as a synonym of Alatoproductus in Treatise
  brach[which(brach$primary_name=="Genuspirifer"),"genus"]<-c("Paraspiriferina") #Following Shen et al. 2017

  #####Changing the ordinal assignment of some records
  brach[which(brach$genus=="Clavigera"),"order"]<-c("Athyridida") # some errors in PBDB data

  #####Changing the generic assignment of some species
  brach[which(brach$accepted_name=="Hybostenoscisma armenica"),"genus"]<-c("Stenoscisma") #Hybostenoscisma includes only the type species according to Shen et al. 2017
  brach[which(brach$accepted_name=="Uncinunellina multiplicata"),"genus"]<-c("Ancorhynchia") #Following Shen et al. 2017
  brach[which(brach$accepted_name=="Oxycolpella mulica"),"genus"]<-c("Qingthyris") #Following Sun et al. 2017
  brach[which(brach$accepted_name=="Piarorhynchella gujiaoensis"),"genus"]<-c("Abrekia") #Following Sun et al. 2017
  brach[which(brach$identified_name=="Piarorhynchia trinodosi"),"genus"]<-c("Nudirostralina") #=Nudirostralina trinodosi
  brach[which(brach$identified_name=="Plicatifera aksuensis"),"genus"]<-c("Rugoconcha") #Following Shen et al. 2017
  brach[which(brach$identified_name=="Plicatifera lipoensis"),"genus"]<-c("Rugoconcha") #Following Shen et al. 2017
  brach[which(brach$identified_name=="Plicatifera latisulcata"),"genus"]<-c("Rugoconcha") #Following Shen et al. 2017
  brach[which(brach$accepted_name=="Septaliphoria pulchra"),"genus"]<-c("Daghanirhynchia") #Following Sun et al. 2017
  brach[which(brach$accepted_name=="Tropeothyris immanis"),"genus"]<-c("Juralina") #Following Cooper 1983
  brach[which(brach$identified_name=="Waagenites barusiensis"),"genus"]<-c("Fusichonetes") #Following Shen et al. 2017
  brach[which(brach$accepted_name=="Costoconcha uncirostrum"),"genus"]<-c("Parahemiptychina") #Following Sun et al. 2017
  brach[which(brach$accepted_name=="Rhynchonella ordinaria"),"genus"]<-c("Bicepsirhynchia") #Following Sun et al. 2017


  #####Discarding some doubtful occurrences

  #Making a list of occurrences that will be discarded
  discard<-c()
  discard<-c(discard,which(brach$occurrence_no=="1069347")) #Stoyanow 1910 "Athyris" cf. planosulcata. This species may not belong to Actinoconchus
  discard<-c(discard,which(brach$ref_author=="Hoover"&brach$ref_pubyr=="1981")) #The ages of these collections are highly uncertain. Roadian-Wordian in Treatise
  discard<-c(discard,which(brach$collection_no=="100287")) #Hahn et al. 1985. The identifications of the brachiopod taxa maybe problematic
  discard<-c(discard,which(brach$occurrence_no=="812847")) #He et al. 2009 Caricula cf. salebrosa. This is a poorly preserved specimen.
  discard<-c(discard,which(brach$occurrence_no=="936529")) #The Cisuralian Chonostegoides ogbinensis described by Feng and Jiang 1978 may belong to another genus
  discard<-c(discard,which(brach$ref_author=="Wu"&brach$ref_pubyr=="1989")) #These taxa are mentioned but not illustrated by Wu (1989), and the ages of many taxa are inconsistent with those in Sun et al. 2017
  discard<-c(discard,which(brach$occurrence_no=="838144")) #Kiessling et al. 2011 Sphriganaria ? (subgenus of Eudesia) sp. The generic assignment is uncertain
  discard<-c(discard,which(brach$occurrence_no=="570479")) #Perez-Huerta and Sheldon 2006 Boloria ? sp. Only tentatively assigned to this genus. (Boloria is a synonym of Gruntia)
  discard<-c(discard,which(brach$occurrence_no=="946998")) #Kashirtsev 1959 Neospirifer ? kimsari ? may not belong to Koenigoria
  discard<-c(discard,which(brach$occurrence_no=="1026869")) #Reed 1944 Spirifer (Neospirifer) cf. kimsari may not belong to Koenigoria
  discard<-c(discard,which(brach$ref_author=="Jin et al."&brach$ref_pubyr=="1979"&brach$min_ma>=GTS2020[which(GTS2020$Interval=="Triassic"),"FAD"])) #Ages of these collections (from the Bayinhe and Nuoyinhe groups) are highly uncertain.
  discard<-c(discard,which(brach$occurrence_no=="1026979")) #Reed 1944 "Strophalosia" cf. excavata may not belong to Orthothrix
  discard<-c(discard,which(brach$occurrence_no=="149854")) #The Gzhelian Spirifer (Neospirifer) triplicatus was reassigned to Terebratuloidea?
  discard<-c(discard,which(brach$occurrence_no=="1391259")) #Popov and Zakharov 2017 Uniplicatorhynchia sp. is poorly studied
  discard<-c(discard,which(brach$accepted_name=="Antronaria zhesiensis")) #This species may belong to another genus
  discard<-c(discard,which(brach$accepted_name=="Glossothyropsis conigera")) #This species may belong to another genus
  discard<-c(discard,which(brach$accepted_name=="Pseudoglossothyris sulcata"))  #This species may be not belongs to Pseudoglossothyris (see Cooper 1983)
  discard<-c(discard,which(brach$occurrence_no=="Pocc811")) #"Lissorhynchia pentaplicata" may belong to another genus rather than Prelissorhynchia
  discard<-c(discard,which(brach$ref_author=="Buddington and Chapin"&brach$ref_pubyr=="1929")) #The taxa reported are poorly identified, and their ages are highly uncertain

  #Discarding
  brach<-brach[-discard,]

  #####Changing the ages of collections and formations

  brach[which(brach$formation=="Abbotsbury Ironstone"),"early_interval"]<-c("Cymodoce")
  brach[which(brach$formation=="Abbotsbury Ironstone"),"late_interval"]<-c("Cymodoce") #According to ammonoid zone

  brach[which(brach$formation=="Admiral"),"early_interval"]<-c("Early Artinskian")
  brach[which(brach$formation=="Admiral"),"late_interval"]<-c("Early Artinskian") #Lucas 2018

  brach[which(brach$formation=="Agoudim"&brach$late_interval=="Bathonian"),"late_interval"]<-c("Bajocian") #Ait Addi and Chafiki 2013

  brach[which(brach$formation=="Aiduna"),"early_interval"]<-c("Early Artinskian")
  brach[which(brach$formation=="Aiduna"),"late_interval"]<-c("Early Artinskian") #"Early Artinskian" in PBDB

  brach[which(brach$formation=="Akhura"&brach$early_interval=="Wuchiapingian"),"early_interval"]<-c("Late Wuchiapingian")
  brach[which(brach$formation=="Akhura"&brach$late_interval=="Wuchiapingian"),"late_interval"]<-c("Late Wuchiapingian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Alcobaca"),"early_interval"]<-c("Kimmeridgian")
  brach[which(brach$formation=="Alcobaca"),"late_interval"]<-c("Kimmeridgian")
  brach[which(brach$formation=="Alcoba莽a"),"early_interval"]<-c("Kimmeridgian")
  brach[which(brach$formation=="Alcoba莽a"),"late_interval"]<-c("Kimmeridgian") #Fürsich et al. 2022

  brach[which(brach$formation=="Almstrom Creek"&brach$early_interval=="Sinemurian"),"early_interval"]<-c("Late Sinemurian")
  brach[which(brach$formation=="Almstrom Creek"&brach$late_interval=="Sinemurian"),"late_interval"]<-c("Late Sinemurian") #According to the original reference

  brach[brach$collection_no%in%c(160644,160645,161031,161032,161107,161109,161110,160205,160647,160874,160875),"early_interval"]<-c("Aalensis")
  brach[brach$collection_no%in%c(160644,160645,161031,161032,161107,161109,161110,160205,160647,160874,160875),"late_interval"]<-c("Aalensis")
  brach[brach$collection_no%in%c(160808,160809),"early_interval"]<-c("Pseudoradiosa")
  brach[brach$collection_no%in%c(160808,160809),"late_interval"]<-c("Pseudoradiosa") #According to ammonoid Zone

  brach[brach$collection_no%in%c(224862,225391:225397,225401,225405,225406),"early_interval"]<-c("Margaritatus")
  brach[brach$collection_no%in%c(224862,225391:225397,225401,225405,225406),"late_interval"]<-c("Margaritatus")
  brach[brach$collection_no%in%c(225390,225398:225400,225402:225404),"early_interval"]<-c("Spinatum")
  brach[brach$collection_no%in%c(225390,225398:225400,225402:225404),"late_interval"]<-c("Spinatum") #According to ammonoid zone

  brach[which(brach$formation=="Amaral"),"early_interval"]<-c("Late Kimmeridgian")
  brach[which(brach$formation=="Amaral"),"late_interval"]<-c("Late Kimmeridgian") #Fürsich et al. 2022

  brach[which(brach$formation=="Ampthill Clay"),"early_interval"]<-c("Late Oxfordian")
  brach[which(brach$formation=="Ampthill Clay"),"late_interval"]<-c("Late Oxfordian") #Wright and Powell 2008

  brach[which(brach$formation=="Anyuan"),"early_interval"]<-c("Carnian")
  brach[which(brach$formation=="Anyuan"),"late_interval"]<-c("Norian")

  brach[which(brach$formation=="Argile de Villerville"),"early_interval"]<-c("Cautisnigrae")
  brach[which(brach$formation=="Argile de Villerville"),"late_interval"]<-c("Cautisnigrae") #According to ammonoid zone

  brach[which(brach$formation=="Argiles a Lopha gregarea"),"early_interval"]<-c("Plicatilis")
  brach[which(brach$formation=="Argiles a Lopha gregarea"),"late_interval"]<-c("Plicatilis") #According to ammonoid zone

  brach[brach$collection_no==198598,"early_interval"]<-c("Bifurcatus")
  brach[brach$collection_no==198598,"late_interval"]<-c("Bifurcatus")
  brach[brach$collection_no==198599,"early_interval"]<-c("Plicatilis")
  brach[brach$collection_no==198599,"late_interval"]<-c("Plicatilis") #According to ammonoid zone

  brach[brach$collection_no==47420,"early_interval"]<-c("Mutabilis")
  brach[brach$collection_no==47420,"late_interval"]<-c("Mutabilis") #According to ammonoid zone

  brach[which(brach$formation=="Asfal"),"early_interval"]<-c("Sevatian")
  brach[which(brach$formation=="Asfal"),"late_interval"]<-c("Sevatian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Aso"),"early_interval"]<-c("Tuvalian")
  brach[which(brach$formation=="Aso"),"late_interval"]<-c("Lacian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Assise a Gryphees"),"early_interval"]<-c("Aalensis")
  brach[which(brach$formation=="Assise a Gryphees"),"late_interval"]<-c("Aalensis") #According to ammonoid zone

  brach[which(brach$formation=="Assistance"),"early_interval"]<-c("Roadian")
  brach[which(brach$formation=="Assistance"),"late_interval"]<-c("Roadian") #Davydov et al. 2018

  brach[which(brach$formation=="Aston Limestone"),"early_interval"]<-c("Early Bajocian")
  brach[which(brach$formation=="Aston Limestone"),"late_interval"]<-c("Early Bajocian") #Barron et al. 1997

  brach[which(brach$formation=="Atkan"),"early_interval"]<-c("Early Capitanian")
  brach[which(brach$formation=="Atkan"),"late_interval"]<-c("Early Capitanian") #Biakov 2013

  brach[which(brach$formation=="Augusta Mountain"&brach$member=="lower"),"early_interval"]<-c("Longobardian")
  brach[which(brach$formation=="Augusta Mountain"&brach$member=="lower"),"late_interval"]<-c("Longobardian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Ayach'yaga"),"early_interval"]<-c("Late Kungurian")
  brach[which(brach$formation=="Ayach'yaga"),"late_interval"]<-c("Late Kungurian") #Kotlyar et al. 2018

  brach[which(brach$formation=="Baikur"),"early_interval"]<-c("Ufimian")
  brach[which(brach$formation=="Baikur"),"late_interval"]<-c("Roadian") #Wordian-Capitanian in PBDB, but Ufimian-Kazanian in Shishlova and Dubkova 2021

  brach[which(brach$formation=="Baitugan"),"early_interval"]<-c("Early Roadian")
  brach[which(brach$formation=="Baitugan"),"late_interval"]<-c("Early Roadian") #Lower Kazanian

  brach[which(brach$formation=="Baliqliq"&brach$early_interval=="Artinskian"),"early_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Baliqliq"&brach$late_interval=="Artinskian"),"late_interval"]<-c("Late Artinskian") #Chen and Shi 2003

  brach[which(brach$formation=="Banan"),"early_interval"]<-c("Tuvalian")
  brach[which(brach$formation=="Banan"),"late_interval"]<-c("Lacian") #Tong et al. 2021. Carnian in old references, but Norian in Tong et al. 2019, 2021

  brach[which(brach$formation=="Bap"),"early_interval"]<-c("Early Sakmarian")
  brach[which(brach$formation=="Bap"),"late_interval"]<-c("Early Sakmarian") #"Early Sakmarian" in PBDB

  brach[which(brach$formation=="Barabash"&brach$early_interval=="Wordian"),"early_interval"]<-c("Late Wordian")
  brach[which(brach$formation=="Barabash"&brach$late_interval=="Wordian"),"late_interval"]<-c("Late Wordian") #According to foram zone. Shi et al. 2022

  brach[brach$collection_no%in%c(210184,211278),"early_interval"]<-c("Margaritatus")
  brach[brach$collection_no%in%c(210184,211278),"late_interval"]<-c("Margaritatus")
  brach[brach$collection_no%in%c(211279,211285,211871),"early_interval"]<-c("Spinatum")
  brach[brach$collection_no%in%c(211279,211285,211871),"late_interval"]<-c("Spinatum")
  brach[brach$collection_no%in%c(211290,211292,211293,211295:211297,211872:211876,212055,212055:212069),"early_interval"]<-c("Tenuicostatum")
  brach[brach$collection_no%in%c(211290,211292,211293,211295:211297,211872:211876,212055,212055:212069),"late_interval"]<-c("Tenuicostatum")
  brach[brach$collection_no%in%c(211856:211869,212070,212071,212188:212191,212193,212197,212199,212200,212202,212203,212205,212206,212208,212212,212214,212216,212217),"early_interval"]<-c("Serpentinum")
  brach[brach$collection_no%in%c(211856:211869,212070,212071,212188:212191,212193,212197,212199,212200,212202,212203,212205,212206,212208,212212,212214,212216,212217),"late_interval"]<-c("Serpentinum")
  brach[brach$collection_no%in%c(211870,212218:212230),"early_interval"]<-c("Bifrons")
  brach[brach$collection_no%in%c(211870,212218:212230),"late_interval"]<-c("Bifrons") #According to ammonoid zone

  brach[which(brach$formation=="Barneston"),"early_interval"]<-c("Early Artinskian")
  brach[which(brach$formation=="Barneston"),"late_interval"]<-c("Early Artinskian") #According to Menning et al. 2006

  brach[which(brach$formation=="Bath Oolite"),"early_interval"]<-c("Retrocostatum")
  brach[which(brach$formation=="Bath Oolite"),"late_interval"]<-c("Retrocostatum") #Barron et al. 2012

  brach[which(brach$formation%in%c("Beattie","Beattie Limestone","Beattie Ls")),"early_interval"]<-c("Late Asselian")
  brach[which(brach$formation%in%c("Beattie","Beattie Limestone","Beattie Ls")),"late_interval"]<-c("Late Asselian") #Wahlman and West 2010

  brach[which(brach$formation=="Bechateur"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Bechateur"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Belebey"),"early_interval"]<-c("Roadian")
  brach[which(brach$formation=="Belebey"),"late_interval"]<-c("Roadian") #Kazanian

  brach[which(brach$formation=="Bell Canyon"&brach$member=="Lamar"),"early_interval"]<-c("Late Capitanian")
  brach[which(brach$formation=="Bell Canyon"&brach$member=="Lamar"),"late_interval"]<-c("Late Capitanian")
  brach[which(brach$formation=="Bell Canyon"&brach$member=="Rader"),"early_interval"]<-c("Early Capitanian")
  brach[which(brach$formation=="Bell Canyon"&brach$member=="Rader"),"late_interval"]<-c("Early Capitanian")
  brach[which(brach$formation=="Bell Canyon"&brach$member=="Hegler"),"early_interval"]<-c("Late Wordian")
  brach[which(brach$formation=="Bell Canyon"&brach$member=="Hegler"),"late_interval"]<-c("Late Wordian")
  brach[which(brach$formation=="Bell Canyon"&brach$member=="Pinery"&brach$early_interval=="Wordian"),"early_interval"]<-c("Late Wordian")
  brach[which(brach$formation=="Bell Canyon"&brach$member=="Pinery"&brach$early_interval=="Wordian"),"late_interval"]<-c("Late Wordian")
  brach[which(brach$formation=="Bell Canyon"&brach$member=="Pinery"&brach$early_interval=="Capitanian"),"early_interval"]<-c("Early Capitanian")
  brach[which(brach$formation=="Bell Canyon"&brach$member=="Pinery"&brach$early_interval=="Capitanian"),"late_interval"]<-c("Early Capitanian")
  brach[which(brach$formation=="Bell Canyon"&brach$early_interval=="Wordian"),"early_interval"]<-c("Late Wordian")
  brach[which(brach$formation=="Bell Canyon"&brach$late_interval=="Wordian"),"late_interval"]<-c("Late Wordian") #GTS2020

  brach[which(brach$formation=="Berry"),"early_interval"]<-c("Early Wordian")
  brach[which(brach$formation=="Berry"),"late_interval"]<-c("Early Wordian") #The age is early Wordian according to Shi et al. 2022

  brach[which(brach$formation=="Berkshire Oolite"),"early_interval"]<-c("Plicatilis")
  brach[which(brach$formation=="Berkshire Oolite"),"late_interval"]<-c("Plicatilis") #According to ammonoid zone

  brach[which(brach$formation=="Bhuj"&brach$member=="Umia"),"early_interval"]<-c("Late Tithonian")
  brach[which(brach$formation=="Bhuj"&brach$member=="Umia"),"late_interval"]<-c("Late Tithonian") #The other collection of the same horizon was given a late Tithonian age

  brach[which(brach$formation=="Birdlip Limestone"&brach$member=="Crickley"),"early_interval"]<-c("Murchisonae")
  brach[which(brach$formation=="Birdlip Limestone"&brach$member=="Crickley"),"late_interval"]<-c("Murchisonae")
  brach[which(brach$formation=="Birdlip Limestone"&brach$member=="Scottsquar"),"early_interval"]<-c("Bradfordensis")
  brach[which(brach$formation=="Birdlip Limestone"&brach$member=="Scottsquar"),"late_interval"]<-c("Bradfordensis") #According to ammonoid zone

  brach[which(brach$formation=="Birmensdorf Beds"),"early_interval"]<-c("Cordatum")
  brach[which(brach$formation=="Birmensdorf Beds"),"late_interval"]<-c("Transversarium") ##According to ammonoid zone

  brach[which(brach$formation=="Blaine"),"early_interval"]<-c("Roadian")
  brach[which(brach$formation=="Blaine"),"late_interval"]<-c("Early Wordian") #Kungurian in PBDB; Laurin and Hook 2022

  brach[which(brach$formation=="Blisworth Clay"),"early_interval"]<-c("Late Bathonian")
  brach[which(brach$formation=="Blisworth Clay"),"late_interval"]<-c("Late Bathonian") #Barron et al. 2012

  brach[which(brach$formation=="Blisworth Limestone"),"early_interval"]<-c("Subcontractus")
  brach[which(brach$formation=="Blisworth Limestone"),"late_interval"]<-c("Retrocostatum") #Barron et al. 2012

  brach[which(brach$formation=="Blue Anchor"),"early_interval"]<-c("Early Rhaetian")
  brach[which(brach$formation=="Blue Anchor"),"late_interval"]<-c("Early Rhaetian") #Following Flannery-Sutherland et al. 2022

  brach[brach$collection_no%in%c(29227:29230),"early_interval"]<-c("Planorbis")
  brach[brach$collection_no%in%c(29227:29230),"late_interval"]<-c("Planorbis")
  brach[brach$collection_no%in%c(29235:29240),"early_interval"]<-c("Bucklandi")
  brach[brach$collection_no%in%c(29235:29240),"late_interval"]<-c("Bucklandi")
  brach[brach$collection_no%in%c(29241:29242),"early_interval"]<-c("Semicostatum")
  brach[brach$collection_no%in%c(29241:29242),"late_interval"]<-c("Semicostatum")
  brach[brach$collection_no==176060,"early_interval"]<-c("Tilmani/spelae")
  brach[brach$collection_no==176060,"late_interval"]<-c("Tilmani/spelae")
  brach[brach$collection_no==176124,"early_interval"]<-c("Hettangian")
  brach[brach$collection_no==176124,"late_interval"]<-c("Early Sinemurian") #According to ammonoid zone

  brach[which(brach$formation=="Bortepa"),"early_interval"]<-c("Early Rhaetian")
  brach[which(brach$formation=="Bortepa"),"late_interval"]<-c("Early Rhaetian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Brisbois"),"early_interval"]<-c("Tuvalian")
  brach[which(brach$formation=="Brisbois"),"late_interval"]<-c("Tuvalian") #According to the original reference

  brach[which(brach$formation=="Broadford Beds"),"early_interval"]<-c("Early Sinemurian")
  brach[which(brach$formation=="Broadford Beds"),"late_interval"]<-c("Early Sinemurian") #Sinemurian in PBDB. This formation is Hettangian-early Sinemurian

  brach[which(brach$formation=="Broccatello"),"early_interval"]<-c("Early Sinemurian")
  brach[which(brach$formation=="Broccatello"),"late_interval"]<-c("Early Sinemurian") #Following Flannery-Sutherland et al. 2022

  brach[brach$collection_no%in%137003:137006,"early_interval"]<-c("Pelsonian")
  brach[brach$collection_no%in%137003:137006,"late_interval"]<-c("Illyrian") #Buchenstein Formation: Pelsonian-Ladinian; these collection were given an Anisian age

  brach[which(brach$formation=="Buko Limestone"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Buko Limestone"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Bulunkan"),"early_interval"]<-c("Late Rhaetian")
  brach[which(brach$formation=="Bulunkan"),"late_interval"]<-c("Late Rhaetian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Byrranga"),"early_interval"]<-c("Asselian")
  brach[which(brach$formation=="Byrranga"),"late_interval"]<-c("Early Artinskian") #Artinskian-Kungurian in PBDB, but Asselian-Artinskian in Shishlova and Dubkova 2021

  brach[which(brach$formation=="Cabacos"),"early_interval"]<-c("Middle Oxfordian")
  brach[which(brach$formation=="Cabacos"),"late_interval"]<-c("Middle Oxfordian") #Fürsich et al. 2022

  brach[brach$collection_no%in%c(29180:29184,29278,29280,29281,29283:29291,29293,29295:29302,29304,29305,29307,29309,29310:29321,29323:29333),"early_interval"]<-c("Semicostatum")
  brach[brach$collection_no%in%c(29180:29184,29278,29280,29281,29283:29291,29293,29295:29302,29304,29305,29307,29309,29310:29321,29323:29333),"late_interval"]<-c("Semicostatum")
  brach[brach$collection_no%in%c(29178:29179,29264:29277),"early_interval"]<-c("Bucklandi")
  brach[brach$collection_no%in%c(29178:29179,29264:29277),"late_interval"]<-c("Bucklandi")
  brach[brach$collection_no%in%c(29253:29254),"early_interval"]<-c("Planorbis")
  brach[brach$collection_no%in%c(29253:29254),"late_interval"]<-c("Planorbis") #According to ammonoid zone

  brach[which(brach$formation=="Calcare di Dosso dei Morti"),"early_interval"]<-c("Pelsonian")
  brach[which(brach$formation=="Calcare di Dosso dei Morti"),"late_interval"]<-c("Illyrian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Calcare di Prezzo"),"early_interval"]<-c("Pelsonian")
  brach[which(brach$formation=="Calcare di Prezzo"),"late_interval"]<-c("Illyrian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Capitan"&brach$early_interval=="Wordian"),"early_interval"]<-c("Late Wordian")
  brach[which(brach$formation=="Capitan"&brach$late_interval=="Wordian"),"late_interval"]<-c("Late Wordian") #Dunbar 1960

  brach[which(brach$formation=="Carditaschichten"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Carditaschichten"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Cassian"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Cassian"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Chambara"&brach$early_interval=="Norian"),"early_interval"]<-c("Alaunian")
  brach[which(brach$formation=="Chambara"&brach$late_interval=="Norian"),"late_interval"]<-c("Sevatian")
  brach[which(brach$formation=="Chambara"&brach$early_interval=="Rhaetian"),"early_interval"]<-c("Early Rhaetian")
  brach[which(brach$formation=="Chambara"&brach$late_interval=="Rhaetian"),"late_interval"]<-c("Early Rhaetian")
  brach[which(brach$formation=="Chambara"&brach$early_interval=="Late Triassic"),"early_interval"]<-c("Alaunian")
  brach[which(brach$formation=="Chambara"&brach$late_interval=="Late Triassic"),"late_interval"]<-c("Early Rhaetian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Chandalaz"&brach$early_interval=="Wordian"),"early_interval"]<-c("Late Wordian")
  brach[which(brach$formation=="Chandalaz"&brach$late_interval=="Wordian"),"late_interval"]<-c("Early Capitanian") #According to foram zone

  brach[which(brach$formation=="Cherry Canyon"&brach$early_interval=="Roadian"),"early_interval"]<-c("Late Roadian")
  brach[which(brach$formation=="Cherry Canyon"&brach$late_interval=="Roadian"),"late_interval"]<-c("Late Roadian")
  brach[which(brach$formation=="Cherry Canyon"&brach$early_interval=="Wordian"),"early_interval"]<-c("Early Wordian")
  brach[which(brach$formation=="Cherry Canyon"&brach$late_interval=="Wordian"),"late_interval"]<-c("Early Wordian") #GTS2020

  brach[brach$collection_no%in%156846:156857,"early_interval"]<-c("Wuchiapingian")
  brach[brach$collection_no%in%156846:156857,"late_interval"]<-c("Wuchiapingian") #Zeng 1995. "Changxing Formation" of these collections should be "Longtan Formation" according to the original reference

  brach[which(brach$formation=="Chapin Peak"),"early_interval"]<-c("Sevatian")
  brach[which(brach$formation=="Chapin Peak"),"late_interval"]<-c("Sevatian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Charmouth Mudstone"&brach$member=="Black Ven Marl"),"early_interval"]<-c("Late Sinemurian")
  brach[which(brach$formation=="Charmouth Mudstone"&brach$member=="Black Ven Marl"),"late_interval"]<-c("Late Sinemurian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Chavoley Beds"),"early_interval"]<-c("Early Oxfordian")
  brach[which(brach$formation=="Chavoley Beds"),"late_interval"]<-c("Middle Oxfordian") #According to the ammonoid zone

  brach[which(brach$formation=="Chhidrun"&brach$early_interval=="Wuchiapingian"),"early_interval"]<-c("Late Wuchiapingian")
  brach[which(brach$formation=="Chhidru"&brach$late_interval=="Wuchiapingian"),"late_interval"]<-c("Late Wuchiapingian") #Waterhouse 2010

  brach[which(brach$formation=="Cleveland Ironstone"&brach$member=="Penny Nab"),"early_interval"]<-c("Margaritatus")
  brach[which(brach$formation=="Cleveland Ironstone"&brach$member=="Penny Nab"),"late_interval"]<-c("Margaritatus") #According to the ammonoid zone

  brach[which(brach$formation=="Cleveland Ironstone"&brach$member=="Kettleness"),"early_interval"]<-c("Spinatum")
  brach[which(brach$formation=="Cleveland Ironstone"&brach$member=="Kettleness"),"late_interval"]<-c("Spinatum") #According to the ammonoid zone

  brach[which(brach$formation=="Clyde"&brach$early_interval=="Artinskian"),"early_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Clyde"&brach$late_interval=="Artinskian"),"late_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Clyde"&brach$early_interval=="Kungurian"),"early_interval"]<-c("Early Kungurian")
  brach[which(brach$formation=="Clyde"&brach$late_interval=="Kungurian"),"late_interval"]<-c("Early Kungurian") #Lucas 2018

  brach[which(brach$formation=="Clypeus Grit"&brach$early_interval=="Bajocian"),"early_interval"]<-c("Late Bajocian")
  brach[which(brach$formation=="Clypeus Grit"&brach$late_interval=="Bajocian"),"late_interval"]<-c("Late Bajocian")
  brach[which(brach$formation=="Clypeus Grit"&brach$early_interval=="Middle Jurassic"),"early_interval"]<-c("Late Bajocian")
  brach[which(brach$formation=="Clypeus Grit"&brach$late_interval=="Middle Jurassic"),"late_interval"]<-c("Bathonian") #Barron et al. 2012

  brach[which(brach$formation=="Cold Fish Volcanics"&brach$early_interval=="Pliensbachian"),"early_interval"]<-c("Early Pliensbachian")
  brach[which(brach$formation=="Cold Fish Volcanics"&brach$late_interval=="Pliensbachian"),"late_interval"]<-c("Early Pliensbachian") #Based on ages of other collections from the same formation

  brach[which(brach$formation=="Coleman Junction"),"early_interval"]<-c("Early Artinskian")
  brach[which(brach$formation=="Coleman Junction"),"late_interval"]<-c("Early Artinskian") #Lucas 2018

  brach[which(brach$formation=="Conchodon"),"early_interval"]<-c("Late Rhaetian")
  brach[which(brach$formation=="Conchodon"),"late_interval"]<-c("Late Rhaetian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Coralline Oolite"&brach$member=="Malton Oolite"),"early_interval"]<-c("Middle Oxfordian")
  brach[which(brach$formation=="Coralline Oolite"&brach$member=="Malton Oolite"),"late_interval"]<-c("Middle Oxfordian") #According to the ammonoid zone

  brach[which(brach$formation=="Cordevol"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Cordevol"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Cornwallis"),"early_interval"]<-c("Lacian")
  brach[which(brach$formation=="Cornwallis"),"late_interval"]<-c("Lacian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Coryphyllia Beds"),"early_interval"]<-c("Longobardian")
  brach[which(brach$formation=="Coryphyllia Beds"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Cotham"),"early_interval"]<-c("Late Rhaetian")
  brach[which(brach$formation=="Cotham"),"late_interval"]<-c("Late Rhaetian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Cs枚var"),"early_interval"]<-c("Tuvalian") #The formation name should be Csövar
  brach[which(brach$formation=="Cs枚var"),"late_interval"]<-c("Rhaetian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Cutoff"&brach$early_interval=="Kungurian"),"early_interval"]<-c("Late Kungurian")
  brach[which(brach$formation=="Cutoff"&brach$late_interval=="Kungurian"),"late_interval"]<-c("Late Kungurian")
  brach[which(brach$formation=="Cutoff"&brach$early_interval=="Roadian"),"early_interval"]<-c("Early Roadian")
  brach[which(brach$formation=="Cutoff"&brach$late_interval=="Roadian"),"late_interval"]<-c("Early Roadian") #GTS2020

  brach[which(brach$formation=="Dashizhai"),"early_interval"]<-c("Roadian")
  brach[which(brach$formation=="Dashizhai"),"late_interval"]<-c("Roadian") #Shen et al. 2021

  brach[brach$collection_no%in%187722:187732,"early_interval"]<-c("Griesbachian")
  brach[brach$collection_no%in%187722:187732,"late_interval"]<-c("Griesbachian") #Wang et al. 2017. Jianzishan section Daye Formation. Age of the brachiopod assemblage should be late Griesbachian according to Dai et al. 2018

  brach[which(brach$formation=="De Geerdalen"&brach$early_interval=="Norian"),"early_interval"]<-c("Lacian")
  brach[which(brach$formation=="De Geerdalen"&brach$late_interval=="Norian"),"late_interval"]<-c("Lacian") #Following Flannery-Sutherland et al. 2022
  brach[which(brach$formation=="De Geerdalen"&brach$early_interval=="Rhaetian"),"early_interval"]<-c("Lacian")
  brach[which(brach$formation=="De Geerdalen"&brach$late_interval=="Rhaetian"),"late_interval"]<-c("Lacian") #The Rhaetian records seem doubtful; they may be Lacian in age

  brach[which(brach$formation=="Degerb枚ls"),"early_interval"]<-c("Capitanian") #The formation name should be Degerböls
  brach[which(brach$formation=="Degerb枚ls"),"late_interval"]<-c("Capitanian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Derirong"),"early_interval"]<-c("Sevatian")
  brach[which(brach$formation=="Derirong"),"late_interval"]<-c("Sevatian") #This formation is Sevatian-Rhaetian in age; marine bivalves were reported from the lower part of this formation

  brach[which(brach$formation=="Dinwoody"&brach$early_interval=="Early Triassic"),"early_interval"]<-c("Induan")
  brach[which(brach$formation=="Dinwoody"&brach$late_interval=="Early Triassic"),"late_interval"]<-c("Induan") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Dog Creek"),"early_interval"]<-c("Late Roadian")
  brach[which(brach$formation=="Dog Creek"),"late_interval"]<-c("Early Wordian") #Kungurian in PBDB; Laurin and Hook 2022

  brach[which(brach$formation=="Doi Long"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Doi Long"),"late_interval"]<-c("Julian") #Lower part of the Carnian according to Feng et al. 2005

  brach[which(brach$formation=="Doi Yot"),"early_interval"]<-c("Aalenian")
  brach[which(brach$formation=="Doi Yot"),"late_interval"]<-c("Aalenian") #According to Kozai et al. 2011

  brach[which(brach$formation=="Dolomia Principale"),"early_interval"]<-c("Lacian")
  brach[which(brach$formation=="Dolomia Principale"),"late_interval"]<-c("Alaunian") #Dolomia Principale Formation: Tuvalian-Alaunian; only the Norian records are in the database and they are assigned to Lacian-Alaunian

  brach[which(brach$formation=="Dolomia stratificata"&brach$early_interval=="Carnian"),"early_interval"]<-c("Tuvalian")
  brach[which(brach$formation=="Dolomia stratificata"&brach$late_interval=="Carnian"),"late_interval"]<-c("Tuvalian")
  brach[which(brach$formation=="Dolomia stratificata"&brach$early_interval=="Norian"),"early_interval"]<-c("Lacian")
  brach[which(brach$formation=="Dolomia stratificata"&brach$late_interval=="Norian"),"late_interval"]<-c("Alaunian") #Dolomia stratificata Formation: Tuvalian-Alaunian

  brach[which(brach$formation=="Dolomite"),"early_interval"]<-c("Longobardian")
  brach[which(brach$formation=="Dolomite"),"late_interval"]<-c("Longobardian") #"Upper Ladinian" in PBDB

  brach[which(brach$formation=="Douling"),"early_interval"]<-c("Early Wuchiapingian")
  brach[which(brach$formation=="Douling"),"late_interval"]<-c("Early Wuchiapingian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Dun Glen"&brach$early_interval=="Norian"),"early_interval"]<-c("Lacian")
  brach[which(brach$formation=="Dun Glen"&brach$late_interval=="Norian"),"late_interval"]<-c("Lacian") #The species of this collection was reported from Lacian according to the original description

  brach[which(brach$formation=="Ebitiem"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Ebitiem"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Echii"&brach$early_interval=="Sakmarian"),"early_interval"]<-c("Late Sakmarian")
  brach[which(brach$formation=="Echii"&brach$late_interval=="Sakmarian"),"late_interval"]<-c("Late Sakmarian")
  brach[which(brach$formation=="Echiiskaya"&brach$early_interval=="Sakmarian"),"early_interval"]<-c("Late Sakmarian")
  brach[which(brach$formation=="Echiiskaya"&brach$late_interval=="Sakmarian"),"late_interval"]<-c("Late Sakmarian") #Upper Sakmarian in PBDB

  brach[which(brach$formation=="Echiiskaya"&brach$early_interval=="Artinskian"),"early_interval"]<-c("Early Artinskian")
  brach[which(brach$formation=="Echiiskaya"&brach$late_interval=="Artinskian"),"late_interval"]<-c("Early Artinskian") #Lower Artinskian in PBDB

  brach[which(brach$formation=="Echiy"&brach$early_interval=="Sakmarian"),"early_interval"]<-c("Late Sakmarian")
  brach[which(brach$formation=="Echiy"&brach$late_interval=="Sakmarian"),"late_interval"]<-c("Late Sakmarian") #Kutygin 2006

  brach[which(brach$formation=="Efremov"),"early_interval"]<-c("Asselian")
  brach[which(brach$formation=="Efremov"),"late_interval"]<-c("Early Artinskian") #Roadian in PBDB. According to Shishlova and Dubkova 2021

  brach[which(brach$formation=="Endybal-Echiy"&brach$early_interval=="Artinskian"),"early_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Endybal-Echiy"&brach$late_interval=="Artinskian"),"late_interval"]<-c("Late Artinskian") #"Upper part of the Artinskian stage" in PBDB

  brach[which(brach$formation=="Erfurt"),"early_interval"]<-c("Longobardian")
  brach[which(brach$formation=="Erfurt"),"late_interval"]<-c("Longobardian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Excelsior"),"early_interval"]<-c("Longobardian")
  brach[which(brach$formation=="Excelsior"),"late_interval"]<-c("Longobardian") #Following Flannery-Sutherland et al. 2022

  brach[brach$collection_no==83128,"early_interval"]<-c("Late Roadian")
  brach[brach$collection_no==83128,"late_interval"]<-c("Late Roadian") #Upper Antiya Formation, Upper Kazanian

  brach[brach$collection_no==102158,"early_interval"]<-c("Early Roadian")
  brach[brach$collection_no==102158,"late_interval"]<-c("Early Roadian") #Lower Antiya Formation, Lower Kazanian

  brach[which(brach$formation=="Falang"),"early_interval"]<-c("Longobardian")
  brach[which(brach$formation=="Falang"),"late_interval"]<-c("Carnian") #Zhao et al. 2021

  brach[which(brach$formation=="Fassan"),"early_interval"]<-c("Fassanian")
  brach[which(brach$formation=="Fassan"),"late_interval"]<-c("Fassanian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Fatra"),"early_interval"]<-c("Late Rhaetian")
  brach[which(brach$formation=="Fatra"),"late_interval"]<-c("Late Rhaetian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Feixianguan"&brach$early_interval=="Changhsingian"),"early_interval"]<-c("Late Changhsingian")
  brach[which(brach$formation=="Feixianguan"&brach$late_interval=="Changhsingian"),"late_interval"]<-c("Late Changhsingian") #P/T beds
  brach[which(brach$formation=="Feixianguan"&brach$early_interval=="Early Triassic"),"early_interval"]<-c("Induan")
  brach[which(brach$formation=="Feixianguan"&brach$late_interval=="Early Triassic"),"late_interval"]<-c("Induan") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Fdolomit (Main Dolomite)"&brach$early_interval=="Carnian"),"early_interval"]<-c("Tuvalian")
  brach[which(brach$formation=="Fdolomit (Main Dolomite)"&brach$late_interval=="Carnian"),"late_interval"]<-c("Tuvalian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Fels玫枚rs Limestone"),"early_interval"]<-c("Pelsonian")
  brach[which(brach$formation=="Fels玫枚rs Limestone"),"late_interval"]<-c("Pelsonian") #The formation name is Felsõörs Limestone; Anisian in PBDB

  brach[which(brach$formation=="Fels枚rs"),"early_interval"]<-c("Pelsonian")
  brach[which(brach$formation=="Fels枚rs"),"late_interval"]<-c("Pelsonian") #The formation name is Felsörs; Anisian in PBDB

  brach[which(brach$formation=="Fernie"&brach$early_interval=="Rhaetian"),"early_interval"]<-c("Late Rhaetian")
  brach[which(brach$formation=="Fernie"&brach$late_interval=="Rhaetian"),"late_interval"]<-c("Late Rhaetian") #The Fernie Formation is a Jurassic formation, but some bivalves from the basal part were reported to be of latest Rhaetian?

  brach[which(brach$formation=="Ferrugineus-Schicht"),"early_interval"]<-c("Middle Bathonian")
  brach[which(brach$formation=="Ferrugineus-Schicht"),"late_interval"]<-c("Late Bathonian") #The early and late intervals were reversed in PBDB

  brach[which(brach$formation=="Fort Riley"),"early_interval"]<-c("Early Artinskian")
  brach[which(brach$formation=="Fort Riley"),"late_interval"]<-c("Early Artinskian") #Menning et al. 2006

  brach[which(brach$formation=="Gabbs"&brach$member=="Lower"),"early_interval"]<-c("Sevatian")
  brach[which(brach$formation=="Gabbs"&brach$member=="Lower"),"late_interval"]<-c("Early Rhaetian")
  brach[which(brach$formation=="Gabbs"&brach$member%in%c("Middle","middle")),"early_interval"]<-c("Late Rhaetian")
  brach[which(brach$formation=="Gabbs"&brach$member%in%c("Middle","middle")),"late_interval"]<-c("Late Rhaetian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Gabbs"&brach$member=="Nun Mine"),"early_interval"]<-c("Sevatian")
  brach[which(brach$formation=="Gabbs"&brach$member=="Nun Mine"),"late_interval"]<-c("Early Rhaetian")
  brach[which(brach$formation=="Gabbs"&brach$member%in%c("Mount Hyatt","Muller Canyon")),"early_interval"]<-c("Late Rhaetian")
  brach[which(brach$formation=="Gabbs"&brach$member%in%c("Mount Hyatt","Muller Canyon")),"late_interval"]<-c("Late Rhaetian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Gansingen Dolomite"),"early_interval"]<-c("Tuvalian")
  brach[which(brach$formation=="Gansingen Dolomite"),"late_interval"]<-c("Tuvalian") #Following Flannery-Sutherland et al. 2022

  brach[brach$collection_no==171314,"early_interval"]<-c("Late Pliensbachian")
  brach[brach$collection_no==171314,"late_interval"]<-c("Late Pliensbachian") #According to the original reference

  brach[which(brach$formation=="Gerennavar"&brach$early_interval=="Changhsingian"),"early_interval"]<-c("Late Changhsingian")
  brach[which(brach$formation=="Gerennavar"&brach$late_interval=="Changhsingian"),"late_interval"]<-c("Late Changhsingian") #"Boundary shale", Latest Changhsingian

  brach[which(brach$formation=="Gerennavar Limestone"&brach$early_interval=="Changhsingian"),"early_interval"]<-c("Late Changhsingian")
  brach[which(brach$formation=="Gerennavar Limestone"&brach$late_interval=="Changhsingian"),"late_interval"]<-c("Late Changhsingian") #"Boundary shale", Latest Changhsingian

  brach[which(brach$formation=="Germig"&brach$early_interval=="Rhaetian"),"early_interval"]<-c("Late Rhaetian")
  brach[which(brach$formation=="Germig"&brach$late_interval=="Rhaetian"),"late_interval"]<-c("Late Rhaetian") #According to the original reference

  brach[which(brach$formation=="Gevanim"&brach$early_interval=="Anisian"),"early_interval"]<-c("Pelsonian")
  brach[which(brach$formation=="Gevanim"&brach$late_interval=="Anisian"),"late_interval"]<-c("Illyrian") #"Upper Anisian" in PBDB

  brach[which(brach$formation=="Ghalilah"),"early_interval"]<-c("Sevatian")
  brach[which(brach$formation=="Ghalilah"),"late_interval"]<-c("Rhaetian") #The Triassic records are given a Sevatian-Rhaetian age

  brach[which(brach$formation=="Gijon"&brach$early_interval=="Rhaetian"),"early_interval"]<-c("Late Rhaetian")
  brach[which(brach$formation=="Gijon"&brach$late_interval=="Rhaetian"),"late_interval"]<-c("Late Rhaetian")
  brach[which(brach$formation=="Gij贸n"&brach$early_interval=="Rhaetian"),"early_interval"]<-c("Late Rhaetian")
  brach[which(brach$formation=="Gij贸n"&brach$late_interval=="Rhaetian"),"late_interval"]<-c("Late Rhaetian")
  brach[which(brach$formation=="Gij贸n"&brach$early_interval=="Hettangian"),"early_interval"]<-c("Planorbis")
  brach[which(brach$formation=="Gij贸n"&brach$late_interval=="Hettangian"),"late_interval"]<-c("Planorbis") #The formation name is Gijón. Following the original reference

  brach[brach$collection_no%in%225514:225517,"early_interval"]<-c("Late Capitanian")
  brach[brach$collection_no%in%225514:225517,"late_interval"]<-c("Late Capitanian") #"Uppermost Capitanian"

  brach[which(brach$formation=="Gorno"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Gorno"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Grabfeld"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Grabfeld"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Grantsville"),"early_interval"]<-c("Longobardian")
  brach[which(brach$formation=="Grantsville"),"late_interval"]<-c("Longobardian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Grey Beds"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Grey Beds"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Gryphite Grits"),"early_interval"]<-c("Early Bajocian")
  brach[which(brach$formation=="Gryphite Grits"),"late_interval"]<-c("Early Bajocian")
  brach[which(brach$formation=="Lower Trigonia Grit"),"early_interval"]<-c("Early Bajocian")
  brach[which(brach$formation=="Lower Trigonia Grit"),"late_interval"]<-c("Early Bajocian") #Barron et al. 2012

  brach[which(brach$formation=="Gungdang"),"early_interval"]<-c("Olenekian")
  brach[which(brach$formation=="Gungdang"),"late_interval"]<-c("Olenekian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Guemgouma"),"early_interval"]<-c("Early Bajocian")
  brach[which(brach$formation=="Guemgouma"),"late_interval"]<-c("Early Bajocian") #According to the ammonoid zone

  brach[which(brach$formation=="Gujo"),"early_interval"]<-c("Changhsingian")
  brach[which(brach$formation=="Gujo"),"late_interval"]<-c("Changhsingian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Gundara"&brach$early_interval=="Kungurian"),"early_interval"]<-c("Late Kungurian")
  brach[which(brach$formation=="Gundara"&brach$late_interval=="Kungurian"),"late_interval"]<-c("Late Kungurian")
  brach[which(brach$formation=="Gundara"&brach$early_interval=="Roadian"),"early_interval"]<-c("Early Roadian")
  brach[which(brach$formation=="Gundara"&brach$late_interval=="Roadian"),"late_interval"]<-c("Early Roadian") #Gundara Formation: Upper Kungurian-Lower Roadian in Angiolini et al. 2016

  brach[which(brach$formation=="Gusinaya"&brach$early_interval=="Sakmarian"),"early_interval"]<-c("Late Sakmarian")
  brach[which(brach$formation=="Gusinaya"&brach$late_interval=="Sakmarian"),"late_interval"]<-c("Late Sakmarian")
  brach[which(brach$formation=="Gusinaya"&brach$early_interval=="Artinskian"),"early_interval"]<-c("Early Artinskian")
  brach[which(brach$formation=="Gusinaya"&brach$late_interval=="Artinskian"),"late_interval"]<-c("Early Artinskian") #Biakov 2013

  brach[which(brach$formation=="Gusinozemelskaya"),"early_interval"]<-c("Ufimian")
  brach[which(brach$formation=="Gusinozemelskaya"),"late_interval"]<-c("Ufimian") #Ufimian

  brach[which(brach$formation=="Gypsum Spring"),"early_interval"]<-c("Late Bajocian")
  brach[which(brach$formation=="Gypsum Spring"),"late_interval"]<-c("Late Bajocian") #The marine fossils were reported from the Middle Member, which is Late Bajocian in age

  brach[which(brach$formation=="Habo beds"&brach$late_interval=="Oxfordian"),"early_interval"]<-c("Late Callovian")
  brach[which(brach$formation=="Habo beds"&brach$late_interval=="Oxfordian"),"late_interval"]<-c("Early Oxfordian") #The youngest part of these beds, Callovian-Oxfordian in PBDB

  brach[which(brach$formation=="Halleck"),"early_interval"]<-c("Sakmarian")
  brach[which(brach$formation=="Halleck"),"late_interval"]<-c("Artinskian") #Asselian in PBDB. But it is possibly middle-Late Cisuralian.

  brach[brach$collection_no==117444,"early_interval"]<-c("Kungurian")
  brach[brach$collection_no==117444,"late_interval"]<-c("Kungurian") #The age of this collection should be Kungurian according to Lee et al. 2019

  brach[which(brach$formation=="Hambleton Oolite"),"early_interval"]<-c("Early Oxfordian")
  brach[which(brach$formation=="Hambleton Oolite"),"late_interval"]<-c("Early Oxfordian") #According to the ammonoid zone

  brach[which(brach$formation=="Hanwang"&brach$member=="Lower"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Hanwang"&brach$member=="Lower"),"late_interval"]<-c("Julian")  #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Hauptdolomit"),"early_interval"]<-c("Lacian")
  brach[which(brach$formation=="Hauptdolomit"),"late_interval"]<-c("Alaunian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Heiberg"),"early_interval"]<-c("Alaunian")
  brach[which(brach$formation=="Heiberg"),"late_interval"]<-c("Alaunian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Helmsdale Boulder Beds"&brach$early_interval=="Kimmeridgian"),"early_interval"]<-c("Late Kimmeridgian")
  brach[which(brach$formation=="Helmsdale Boulder Beds"&brach$late_interval=="Kimmeridgian"),"late_interval"]<-c("Late Kimmeridgian") #According to the ammonoid zone

  brach[which(brach$formation=="Helmsdale Boulder Beds"&brach$early_interval=="Tithonian"),"early_interval"]<-c("Early Tithonian")
  brach[which(brach$formation=="Helmsdale Boulder Beds"&brach$late_interval=="Tithonian"),"late_interval"]<-c("Early Tithonian") #According to the ammonoid zone

  brach[which(brach$formation=="Heshan"&brach$early_interval=="Changhsingian"),"early_interval"]<-c("Early Changhsingian")
  brach[which(brach$formation=="Heshan"&brach$late_interval=="Changhsingian"),"late_interval"]<-c("Early Changhsingian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Hess"&brach$early_interval=="Artinskian"),"early_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Hess"&brach$late_interval=="Artinskian"),"late_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Hess"&brach$early_interval=="Kungurian"),"early_interval"]<-c("Early Kungurian")
  brach[which(brach$formation=="Hess"&brach$late_interval=="Kungurian"),"late_interval"]<-c("Early Kungurian") #GTS2020

  brach[which(brach$formation=="Hidegkut"),"early_interval"]<-c("Smithian")
  brach[which(brach$formation=="Hidegkut"),"late_interval"]<-c("Smithian") #Voros 2009

  brach[which(brach$formation=="Hirabara"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Hirabara"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Hirobatake"),"early_interval"]<-c("Olenekian")
  brach[which(brach$formation=="Hirobatake"),"late_interval"]<-c("Olenekian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Hoang Mai"),"early_interval"]<-c("Pelsonian")
  brach[which(brach$formation=="Hoang Mai"),"late_interval"]<-c("Illyrian") #"Late Anisian" in PBDB

  brach[which(brach$formation=="Hollenberg"),"early_interval"]<-c("Early Kungurian")
  brach[which(brach$formation=="Hollenberg"),"late_interval"]<-c("Early Kungurian") #Kungurian in PBDB. Lower Leonardian. Maybe Late Artinskian?

  brach[which(brach$formation=="Hong Hoi"&brach$early_interval=="Carnian"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Hong Hoi"&brach$late_interval=="Carnian"),"late_interval"]<-c("Julian") #According to Feng et al. 2005

  brach[which(brach$formation=="Hong Ngai"),"early_interval"]<-c("Late Wuchiapingian")
  brach[which(brach$formation=="Hong Ngai"),"late_interval"]<-c("Griesbachian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Hornos-Siles"),"early_interval"]<-c("Longobardian")
  brach[which(brach$formation=="Hornos-Siles"),"late_interval"]<-c("Longobardian") #"Upper Ladinian" in PBDB

  brach[which(brach$formation=="Hosselkus"),"early_interval"]<-c("Tuvalian")
  brach[which(brach$formation=="Hosselkus"),"late_interval"]<-c("Tuvalian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Hound Island Volcanics"),"early_interval"]<-c("Sevatian")
  brach[which(brach$formation=="Hound Island Volcanics"),"late_interval"]<-c("Sevatian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Hoyt Canyon"),"early_interval"]<-c("Sevatian")
  brach[which(brach$formation=="Hoyt Canyon"),"late_interval"]<-c("Sevatian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Huai Tak"),"early_interval"]<-c("Early Changhsingian")
  brach[which(brach$formation=="Huai Tak"),"late_interval"]<-c("Early Changhsingian")
  brach[which(brach$formation=="Huai Thak"),"early_interval"]<-c("Early Changhsingian")
  brach[which(brach$formation=="Huai Thak"),"late_interval"]<-c("Early Changhsingian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Huanggangliang"),"early_interval"]<-c("Wordian")
  brach[which(brach$formation=="Huanggangliang"),"late_interval"]<-c("Wordian") #Shen et al. 2021

  brach[which(brach$formation=="Huangzhishan"&brach$early_interval=="Changhsingian"),"early_interval"]<-c("Late Changhsingian")
  brach[which(brach$formation=="Huangzhishan"&brach$late_interval=="Changhsingian"),"late_interval"]<-c("Late Changhsingian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Hueco"&brach$member=="Robledo Mountains"),"early_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Hueco"&brach$member=="Robledo Mountains"),"late_interval"]<-c("Late Artinskian") #Lucas et al. 2015

  brach[which(brach$formation=="Hueco Canyon"&brach$early_interval=="Artinskian"),"early_interval"]<-c("Early Artinskian")
  brach[which(brach$formation=="Hueco Canyon"&brach$late_interval=="Artinskian"),"late_interval"]<-c("Early Artinskian") #Wolfcampian

  brach[which(brach$formation=="Huobachong"),"early_interval"]<-c("Alaunian")
  brach[which(brach$formation=="Huobachong"),"late_interval"]<-c("Early Rhaetian") #Tong et al. 2019, 2021

  brach[which(brach$formation=="Hybe"),"early_interval"]<-c("Late Rhaetian")
  brach[which(brach$formation=="Hybe"),"late_interval"]<-c("Late Rhaetian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Ilibei"),"early_interval"]<-c("Early Sakmarian")
  brach[which(brach$formation=="Ilibei"),"late_interval"]<-c("Early Sakmarian") #Tastubian

  brach[brach$collection_no%in%c(75986,75985),"early_interval"]<-c("Lacian")
  brach[brach$collection_no%in%c(75986,75985),"late_interval"]<-c("Lacian") #Upper Jiapila Formation
  brach[brach$collection_no==123659,"early_interval"]<-c("Carnian")
  brach[brach$collection_no==123659,"late_interval"]<-c("Lacian") #Unknown horizon of the Jiapila Formation

  brach[which(brach$formation=="Jieshandaji"),"early_interval"]<-c("Early Oxfordian") #The formation name should be Jieshandaban
  brach[which(brach$formation=="Jieshandaji"),"late_interval"]<-c("Early Oxfordian") #Sun et al. 2017

  brach[which(brach$formation=="Jinji"&brach$early_interval=="Hettangian"),"early_interval"]<-c("Late Hettangian")
  brach[which(brach$formation=="Jinji"&brach$late_interval=="Hettangian"),"late_interval"]<-c("Late Hettangian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Jungle Creek"&brach$early_interval=="Artinskian"),"early_interval"]<-c("Early Artinskian")
  brach[which(brach$formation=="Jungle Creek"&brach$late_interval=="Artinskian"),"late_interval"]<-c("Early Artinskian") #Waterhouse 2018

  brach[which(brach$formation=="Juripu"&brach$early_interval=="Capitanian"),"early_interval"]<-c("Late Capitanian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Kaibab"&brach$early_interval=="Kungurian"),"early_interval"]<-c("Late Kungurian")
  brach[which(brach$formation=="Kaibab"&brach$late_interval=="Kungurian"),"late_interval"]<-c("Early Roadian") #Lucas and Henderson 2021

  brach[which(brach$formation=="Kamar-e-Mehdi"&brach$member=="Echellon Limestone"),"early_interval"]<-c("Middle Callovian")
  brach[which(brach$formation=="Kamar-e-Mehdi"&brach$member=="Echellon Limestone"),"late_interval"]<-c("Late Callovian") #This member is the lowest member of the formation

  brach[which(brach$formation=="Kamishak"),"early_interval"]<-c("Sevatian")
  brach[which(brach$formation=="Kamishak"),"late_interval"]<-c("Sevatian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Kamosho"),"early_interval"]<-c("Lacian")
  brach[which(brach$formation=="Kamosho"),"late_interval"]<-c("Alaunian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Kang Pla"),"early_interval"]<-c("Tuvalian")
  brach[which(brach$formation=="Kang Pla"),"late_interval"]<-c("Tuvalian") #Feng et al. 2005

  brach[which(brach$formation=="Kangshare"&brach$early_interval=="Changhsingian"),"early_interval"]<-c("Late Changhsingian")
  brach[which(brach$formation=="Kangshare"&brach$late_interval=="Changhsingian"),"late_interval"]<-c("Late Changhsingian") #Shen et al. 2010

  brach[which(brach$formation=="Kankarin"),"early_interval"]<-c("Early Artinskian")
  brach[which(brach$formation=="Kankarin"),"late_interval"]<-c("Early Artinskian") #"Early Artinskian" in PBDB

  brach[which(brach$formation=="Kap Leslie"&brach$member%in%c("pernaryggen","Pernaryggen")),"early_interval"]<-c("Middle Volgian")
  brach[which(brach$formation=="Kap Leslie"&brach$member%in%c("pernaryggen","Pernaryggen")),"late_interval"]<-c("Middle Volgian") #Surlyk et al. 2021
  brach[which(brach$formation=="Kap Leslie"&brach$member=="Krebsedal"),"early_interval"]<-c("Early Volgian")
  brach[which(brach$formation=="Kap Leslie"&brach$member=="Krebsedal"),"late_interval"]<-c("Middle Volgian") #Surlyk et al. 2021
  brach[which(brach$formation=="Kap Leslie"&brach$member=="Astartedal"),"early_interval"]<-c("Middle Volgian")
  brach[which(brach$formation=="Kap Leslie"&brach$member=="Astartedal"),"late_interval"]<-c("Middle Volgian") #Surlyk et al. 2021
  brach[which(brach$formation=="Kap Leslie"&brach$member=="Aldinger Elv"),"early_interval"]<-c("Late Oxfordian")
  brach[which(brach$formation=="Kap Leslie"&brach$member=="Aldinger Elv"),"late_interval"]<-c("Late Oxfordian") #Surlyk et al. 2021

  brach[which(brach$formation=="Kapp Starostin"&brach$member=="Selanderneset"),"early_interval"]<-c("Wuchiapingian")
  brach[which(brach$formation=="Kapp Starostin"&brach$member=="Selanderneset"),"late_interval"]<-c("Wuchiapingian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Kapp Starostin"&brach$member=="Voringen"&brach$early_interval=="Artinskian"),"early_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Kapp Starostin"&brach$member=="Voringen"&brach$late_interval=="Artinskian"),"late_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Kapp Starostin"&brach$member=="Voringen"&brach$early_interval=="Kungurian"),"early_interval"]<-c("Early Kungurian")
  brach[which(brach$formation=="Kapp Starostin"&brach$member=="Voringen"&brach$late_interval=="Kungurian"),"late_interval"]<-c("Early Kungurian") #Lee et al. 2019

  brach[which(brach$formation=="Kapp Starostin"&brach$early_interval=="Artinskian"),"early_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Kapp Starostin"&brach$late_interval=="Artinskian"),"late_interval"]<-c("Late Artinskian") #The basal part of this formation (Voringen Member) is late Artinskian-early Kungurian according to Lee et al. 2019

  brach[which(brach$formation=="Kapp Toscana"&brach$early_interval=="Ladinian"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Kapp Toscana"&brach$late_interval=="Ladinian"),"late_interval"]<-c("Julian")
  brach[which(brach$formation=="Kapp Toscana"&brach$early_interval=="Norian"),"early_interval"]<-c("Lacian")
  brach[which(brach$formation=="Kapp Toscana"&brach$late_interval=="Norian"),"late_interval"]<-c("Lacian") #This formation is Julian-Lacian

  brach[which(brach$formation=="Kapp Toscana"&brach$member=="Tschermarkfjellet"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Kapp Toscana"&brach$member=="Tschermarkfjellet"),"late_interval"]<-c("Julian")
  brach[which(brach$formation=="Kapp Toscana"&brach$member=="Tschermakfjellet"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Kapp Toscana"&brach$member=="Tschermakfjellet"),"late_interval"]<-c("Julian") #The lower member

  brach[which(brach$formation=="Karadan"&brach$early_interval=="Norian"),"early_interval"]<-c("Alaunian")
  brach[which(brach$formation=="Karadan"&brach$late_interval=="Norian"),"late_interval"]<-c("Alaunian") #The two bivalve zones are largely overlaped with Alaunian

  brach[which(brach$formation=="Karadzhatykskaya"),"early_interval"]<-c("Spathian")
  brach[which(brach$formation=="Karadzhatykskaya"),"late_interval"]<-c("Spathian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Karazin"),"early_interval"]<-c("Aegean")
  brach[which(brach$formation=="Karazin"),"late_interval"]<-c("Aegean") #This formation includes some middle and late Anisian collections, but the bivalves are from the "lower Anisian"

  brach[which(brach$formation=="Karchowice"),"early_interval"]<-c("Pelsonian")
  brach[which(brach$formation=="Karchowice"),"late_interval"]<-c("Illyrian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation%in%c("Kardosret","Kardosr茅i M茅szko","Kardosr茅ti M茅szko")),"early_interval"]<-c("Hettangian")
  brach[which(brach$formation%in%c("Kardosret","Kardosr茅i M茅szko","Kardosr茅ti M茅szko")),"late_interval"]<-c("Hettangian") #Kardosréi Mészko, Kardosréti Mészko. Voros 2022

  brach[which(brach$formation=="Kashiwadaira"),"early_interval"]<-c("Wordian")
  brach[which(brach$formation=="Kashiwadaira"),"late_interval"]<-c("Wordian")
  brach[which(brach$formation=="Takakurayama"),"early_interval"]<-c("Wordian")
  brach[which(brach$formation=="Takakurayama"),"late_interval"]<-c("Wordian") #Following Ehiro 2022

  brach[which(brach$formation=="Kattamardzanaj"),"early_interval"]<-c("Longobardian")
  brach[which(brach$formation=="Kattamardzanaj"),"late_interval"]<-c("Longobardian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Kawhia Point Siltstone"),"early_interval"]<-c("Early Kimmeridgian")
  brach[which(brach$formation=="Kawhia Point Siltstone"),"late_interval"]<-c("Early Kimmeridgian") #"Early Kimmeridgian" in PBDB

  brach[which(brach$formation=="Kayitou"&brach$early_interval=="Changhsingian"),"early_interval"]<-c("Late Changhsingian")
  brach[which(brach$formation=="Kayitou"&brach$late_interval=="Changhsingian"),"late_interval"]<-c("Late Changhsingian") #P-T transitional bed

  brach[which(brach$formation=="Kellaways Clay"),"early_interval"]<-c("Early Callovian")
  brach[which(brach$formation=="Kellaways Clay"),"late_interval"]<-c("Early Callovian") #Scotney et al. 2012

  brach[which(brach$formation=="Kellaways Sand"),"early_interval"]<-c("Early Callovian")
  brach[which(brach$formation=="Kellaways Sand"),"late_interval"]<-c("Early Callovian") #Scotney et al. 2012

  brach[which(brach$formation=="Kendlbach"&brach$early_interval=="Rhaetian"),"early_interval"]<-c("Late Rhaetian")
  brach[which(brach$formation=="Kendlbach"&brach$late_interval=="Rhaetian"),"late_interval"]<-c("Late Rhaetian")
  brach[which(brach$formation=="Kendlbach"&brach$early_interval=="Hettangian"),"early_interval"]<-c("Planorbis")
  brach[which(brach$formation=="Kendlbach"&brach$late_interval=="Hettangian"),"late_interval"]<-c("Planorbis") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Keziliqiman"&brach$early_interval=="Kungurian"),"early_interval"]<-c("Early Kungurian")
  brach[which(brach$formation=="Keziliqiman"&brach$late_interval=="Kungurian"),"late_interval"]<-c("Early Kungurian") #Chen and Shi 2006

  brach[which(brach$formation=="Khabakh"&brach$early_interval=="Kungurian"),"early_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Khabakh"&brach$late_interval=="Kungurian"),"late_interval"]<-c("Late Artinskian") #Shi 2006 the Echian Horizon

  brach[which(brach$formation=="Khachik"&brach$early_interval=="Wuchiapingian"),"early_interval"]<-c("Early Wuchiapingian")
  brach[which(brach$formation=="Khachik"&brach$late_interval=="Wuchiapingian"),"late_interval"]<-c("Early Wuchiapingian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Khalalin"),"early_interval"]<-c("Ufimian")
  brach[which(brach$formation=="Khalalin"),"late_interval"]<-c("Ufimian") #Aphanaia korkodonica Zone, Ufimian, according to Biakov 2012

  brach[which(brach$formation=="Khaldzin"),"early_interval"]<-c("Early Kungurian")
  brach[which(brach$formation=="Khaldzin"),"late_interval"]<-c("Early Kungurian") #Biakov 2012; this species is common at the end of the Aphanaia lima Zone

  brach[which(brach$formation=="Khanalichan"),"early_interval"]<-c("Artinskian")
  brach[which(brach$formation=="Khanalichan"),"late_interval"]<-c("Artinskian") #Kungurian in Abramov and Grigorieva 1988, Artinskian in Klets 2000. The Sigskaya Formation below this formation is Sakmarian

  brach[which(brach$formation=="Khangsar"),"early_interval"]<-c("Induan")
  brach[which(brach$formation=="Khangsar"),"late_interval"]<-c("Induan") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Khorokytskaya"),"early_interval"]<-c("Late Asselian")
  brach[which(brach$formation=="Khorokytskaya"),"late_interval"]<-c("Early Sakmarian") #"Upper Asselian-Lower Sakmarian" in PBDB

  brach[which(brach$formation=="Khao Phra"),"early_interval"]<-c("Late Kungurian")
  brach[which(brach$formation=="Khao Phra"),"late_interval"]<-c("Late Kungurian") #Xu et al. 2022

  brach[which(brach$formation=="Khovsgol"&brach$early_interval=="Artinskian"),"early_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Khovsgol"&brach$late_interval=="Artinskian"),"late_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Khovsgol"&brach$early_interval=="Wordian"),"early_interval"]<-c("Kungurian")
  brach[which(brach$formation=="Khovsgol"&brach$late_interval=="Wordian"),"late_interval"]<-c("Kungurian") #Manankov et al. 2006 and the original reference

  brach[which(brach$formation=="Khun Huai"),"early_interval"]<-c("Toarcian")
  brach[which(brach$formation=="Khun Huai"),"late_interval"]<-c("Middle Aalenian") #According to Kozai et al. 2011

  brach[which(brach$formation=="Khunamuh"&brach$early_interval=="Changhsingian"),"late_interval"]<-c("Late Changhsingian")
  brach[which(brach$formation=="Khunamuh"&brach$early_interval=="Changhsingian"),"early_interval"]<-c("Late Changhsingian") #Changhsingian-Griesbachian in PBDB. Member E1 should be the latest Permian

  brach[brach$collection_no%in%c("1911","1613","1912"),"early_interval"]<-c("Baylei")
  brach[brach$collection_no%in%c("1911","1613","1912"),"late_interval"]<-c("Baylei") #According to ammonoid zone

  brach[brach$collection_no%in%c("8574","8600","8657"),"early_interval"]<-c("Mutabilis")
  brach[brach$collection_no%in%c("8574","8600","8657"),"late_interval"]<-c("Mutabilis") #According to ammonoid zone

  brach[brach$collection_no==38494,"early_interval"]<-c("Cymodoce")
  brach[brach$collection_no==38494,"late_interval"]<-c("Mutabilis") #According to ammonoid zone

  brach[which(brach$formation=="Kitamatadani"),"early_interval"]<-c("Early Pliensbachian")
  brach[which(brach$formation=="Kitamatadani"),"late_interval"]<-c("Early Pliensbachian") #Nakada et al. 2021

  brach[which(brach$formation=="Klingnau"),"early_interval"]<-c("Bathonian")
  brach[which(brach$formation=="Klingnau"),"late_interval"]<-c("Bathonian") #The "Varians Beds" may be middle Bathonian in age

  brach[which(brach$formation=="Klyuchevskaya"),"early_interval"]<-c("Early Wuchiapingian")
  brach[which(brach$formation=="Klyuchevskaya"),"late_interval"]<-c("Early Wuchiapingian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Kobyuma"),"early_interval"]<-c("Ufimian")
  brach[which(brach$formation=="Kobyuma"),"late_interval"]<-c("Roadian") #Davydov et al. 2022

  brach[which(brach$formation=="Koessen"&brach$member%in%c("Hochalm","Hochalm, Unit 1","Hochalm, Unit 2","Hochalm, Unit 3","Hochalm, Unit 4")),"early_interval"]<-c("Early Rhaetian")
  brach[which(brach$formation=="Koessen"&brach$member%in%c("Hochalm","Hochalm, Unit 1","Hochalm, Unit 2","Hochalm, Unit 3","Hochalm, Unit 4")),"late_interval"]<-c("Early Rhaetian")
  brach[which(brach$formation=="Koessen"&brach$member%in%c("Eiberg, Unit 1","Eiberg, Unit1","Eiberg, Unit 2","Eiberg, Unit 3")),"early_interval"]<-c("Late Rhaetian")
  brach[which(brach$formation=="Koessen"&brach$member%in%c("Eiberg, Unit 1","Eiberg, Unit1","Eiberg, Unit 2","Eiberg, Unit 3")),"late_interval"]<-c("Late Rhaetian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Konglomeratbank 3"),"early_interval"]<-c("Bithynian")
  brach[which(brach$formation=="Konglomeratbank 3"),"late_interval"]<-c("Bithynian") #"Bithynian" in PBDB

  brach[which(brach$formation=="Kossen"&brach$member%in%c("2","3","Unit 3","Unit 4","Hochalm","Hochalm, Unit 1","Hochalm,Unit1","Hochalm, Unit1","Hochalm, Unit 2",
  "Hochalm,Unit2","Holchalm, Unit 2","Hochalm, Unit3","Hochalm,Unit3","Hochalm, Unit 3","Holchalm, Unit 3","Hochalm,Unit4","Hochalm, Unit 4")),"early_interval"]<-c("Early Rhaetian")
  brach[which(brach$formation=="Kossen"&brach$member%in%c("2","3","Unit 3","Unit 4","Hochalm","Hochalm, Unit 1","Hochalm,Unit1","Hochalm, Unit1","Hochalm, Unit 2",
  "Hochalm,Unit2","Holchalm, Unit 2","Hochalm, Unit3","Hochalm,Unit3","Hochalm, Unit 3","Holchalm, Unit 3","Hochalm,Unit4","Hochalm, Unit 4")),"late_interval"]<-c("Early Rhaetian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Kossen"&brach$member%in%c("6月7日","Unit 6-7","Eiberg","Eiberg, Unit 1","Eiberg, Unit 2","Eiberg, Unit 3","Eiberg, Unit 4")),"early_interval"]<-c("Late Rhaetian")
  brach[which(brach$formation=="Kossen"&brach$member%in%c("6月7日","Unit 6-7","Eiberg","Eiberg, Unit 1","Eiberg, Unit 2","Eiberg, Unit 3","Eiberg, Unit 4")),"late_interval"]<-c("Late Rhaetian") #"6月7日" was 6-7; following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Kowhai Point Siltstone"),"early_interval"]<-c("Early Kimmeridgian")
  brach[which(brach$formation=="Kowhai Point Siltstone"),"late_interval"]<-c("Early Kimmeridgian") #"Early Kimmeridgian" in PBDB

  brach[which(brach$formation=="Kozhevnikov"&brach$early_interval=="Capitanian"),"early_interval"]<-c("Early Capitanian")
  brach[which(brach$formation=="Kozhevnikov"&brach$late_interval=="Capitanian"),"late_interval"]<-c("Early Capitanian") #Maitaia bella Zone, lower Capitanian

  brach[which(brach$formation=="Kulu"&brach$early_interval=="Wuchiapingian"),"early_interval"]<-c("Late Wuchiapingian")
  brach[which(brach$formation=="Kulu"&brach$late_interval=="Wuchiapingian"),"late_interval"]<-c("Late Wuchiapingian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Kupferschiefer"),"early_interval"]<-c("Early Wuchiapingian")
  brach[which(brach$formation=="Kupferschiefer"),"late_interval"]<-c("Early Wuchiapingian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Kurotaki"),"early_interval"]<-c("Smithian")
  brach[which(brach$formation=="Kurotaki"),"late_interval"]<-c("Smithian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Kusano"),"early_interval"]<-c("Olenekian")
  brach[which(brach$formation=="Kusano"),"late_interval"]<-c("Olenekian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Laishike"),"early_interval"]<-c("Tuvalian")
  brach[which(brach$formation=="Laishike"),"late_interval"]<-c("Tuvalian") #Zhao et al. 2021

  brach[which(brach$formation=="Lamei"),"early_interval"]<-c("Olenekian")
  brach[which(brach$formation=="Lamei"),"late_interval"]<-c("Olenekian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Lanashan"),"early_interval"]<-c("Alaunian")
  brach[which(brach$formation=="Lanashan"),"late_interval"]<-c("Alaunian") #The brachiopods from the Lanashan formation are similar to those from the Qulonggongba Formation, suggesting the same age

  brach[which(brach$formation=="Langobard"),"early_interval"]<-c("Longobardian")
  brach[which(brach$formation=="Langobard"),"late_interval"]<-c("Longobardian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Lazurnaya Bay"&brach$early_interval=="Olenekian"),"early_interval"]<-c("Smithian")
  brach[which(brach$formation=="Lazurnaya Bay"&brach$late_interval=="Olenekian"),"late_interval"]<-c("Smithian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Leikoupo"),"early_interval"]<-c("Anisian")
  brach[which(brach$formation=="Leikoupo"),"late_interval"]<-c("Anisian") #Following Flannery-Sutherland et al. 2022

  brach[brach$collection_no==225384,"early_interval"]<-c("Margaritatus")
  brach[brach$collection_no==225384,"late_interval"]<-c("Margaritatus") #According to ammonoid zone

  brach[brach$collection_no%in%225385:225386,"early_interval"]<-c("Spinatum")
  brach[brach$collection_no%in%225385:225386,"late_interval"]<-c("Spinatum") #According to ammonoid zone

  brach[which(brach$formation=="Lengwu"&brach$member=="Shiziling"),"early_interval"]<-c("Early Capitanian")
  brach[which(brach$formation=="Lengwu"&brach$member=="Shiziling"),"late_interval"]<-c("Early Capitanian") #Lower member of the Lengwu Formation
  brach[which(brach$formation=="Lengwu"&brach$member=="Cunhouling"),"early_interval"]<-c("Late Capitanian")
  brach[which(brach$formation=="Lengwu"&brach$member=="Cunhouling"),"late_interval"]<-c("Late Capitanian") #Upper member of the Lengwu Formation

  brach[which(brach$formation=="Liangshan"),"early_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Liangshan"),"late_interval"]<-c("Late Artinskian") #Shen et al. 2021

  brach[which(brach$formation=="Liard"&brach$early_interval=="Ladinian"),"early_interval"]<-c("Longobardian")
  brach[which(brach$formation=="Liard"&brach$late_interval=="Ladinian"),"late_interval"]<-c("Longobardian") #"Upper Ladinian" in PBDB

  brach[which(brach$formation=="Liard"&brach$early_interval=="Carnian"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Liard"&brach$late_interval=="Carnian"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Licha"&brach$early_interval=="Artinskian"),"early_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Licha"&brach$late_interval=="Artinskian"),"late_interval"]<-c("Early Kungurian") #Shen et al. 2017

  brach[which(brach$formation=="Licha"&brach$early_interval=="Asselian"),"early_interval"]<-c("Gzhelian")
  brach[which(brach$formation=="Licha"&brach$late_interval=="Asselian"),"late_interval"]<-c("Asselian") #Shen et al. 2017

  brach[which(brach$formation=="Liegende Bankkalk"),"early_interval"]<-c("Beckeri")
  brach[which(brach$formation=="Liegende Bankkalk"),"late_interval"]<-c("Beckeri") #The ammonoid zone

  brach[which(brach$formation=="Lilstock"),"early_interval"]<-c("Late Rhaetian")
  brach[which(brach$formation=="Lilstock"),"late_interval"]<-c("Late Rhaetian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Lindemans Bugt"),"early_interval"]<-c("Middle Volgian")
  brach[which(brach$formation=="Lindemans Bugt"),"late_interval"]<-c("Middle Volgian") #"Middle Volgian" in PBDB

  brach[which(brach$formation=="Longdongchuan"),"early_interval"]<-c("Late Changhsingian")
  brach[which(brach$formation=="Longdongchuan"),"late_interval"]<-c("Late Changhsingian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Longyin"),"early_interval"]<-c("Early Artinskian")
  brach[which(brach$formation=="Longyin"),"late_interval"]<-c("Early Artinskian") #The brachiopods from this formation were believed to be Sakmarian in age (Shen et al. 2017), but the ammonoids suggest an early Artinskian age (Zhou 2017)

  brach[which(brach$formation=="Loping"&brach$member=="Wangpanli"),"early_interval"]<-c("Early Changhsingian")
  brach[which(brach$formation=="Loping"&brach$member=="Wangpanli"),"late_interval"]<-c("Early Changhsingian") #Following Shen et al. 2017

  brach[brach$collection_no%in%204121:204123,"early_interval"]<-c("Davoei")
  brach[brach$collection_no%in%204121:204123,"late_interval"]<-c("Davoei") #According to ammonoid zone

  brach[brach$collection_no%in%204124:204125,"early_interval"]<-c("Ibex")
  brach[brach$collection_no%in%204124:204125,"late_interval"]<-c("Ibex") #According to ammonoid zone

  brach[brach$collection_no%in%204126:204129,"early_interval"]<-c("Jamesoni")
  brach[brach$collection_no%in%204126:204129,"late_interval"]<-c("Jamesoni") #According to ammonoid zone

  brach[brach$collection_no%in%204130:204133,"early_interval"]<-c("Raricostatum")
  brach[brach$collection_no%in%204131:204133,"late_interval"]<-c("Raricostatum") #According to ammonoid zone

  brach[brach$collection_no==204146,"early_interval"]<-c("Oxynotum")
  brach[brach$collection_no==204146,"late_interval"]<-c("Oxynotum") #According to ammonoid zone

  brach[brach$collection_no%in%204147:204148,"early_interval"]<-c("Obtusum")
  brach[brach$collection_no%in%204147:204148,"late_interval"]<-c("Obtusum") #According to ammonoid zone

  brach[brach$collection_no%in%204149:204151,"early_interval"]<-c("Turneri")
  brach[brach$collection_no%in%204149:204151,"late_interval"]<-c("Turneri") #According to ammonoid zone

  brach[brach$collection_no%in%204152:204154,"early_interval"]<-c("Semicostatum")
  brach[brach$collection_no%in%204152:204154,"late_interval"]<-c("Semicostatum") #According to ammonoid zone

  brach[brach$collection_no%in%204155:204157,"early_interval"]<-c("Bucklandi")
  brach[brach$collection_no%in%204155:204157,"late_interval"]<-c("Bucklandi") #According to ammonoid zone

  brach[brach$collection_no%in%204158:204159,"early_interval"]<-c("Angulata")
  brach[brach$collection_no%in%204158:204159,"late_interval"]<-c("Angulata") #According to ammonoid zone

  brach[brach$collection_no%in%204160:204161,"early_interval"]<-c("Planorbis")
  brach[brach$collection_no%in%204160:204161,"late_interval"]<-c("Planorbis") #According to ammonoid zone

  brach[which(brach$formation=="Lower"&brach$member=="White Lias Limestone"),"early_interval"]<-c("Late Rhaetian")
  brach[which(brach$formation=="Lower"&brach$member=="White Lias Limestone"),"late_interval"]<-c("Late Rhaetian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Lower Calcareous Grit"),"early_interval"]<-c("Mariae")
  brach[which(brach$formation=="Lower Calcaerous Grit"),"late_interval"]<-c("Cordatum") #According to ammonoid zone

  brach[which(brach$formation=="Lower Calcareous Grit"),"early_interval"]<-c("Early Oxfordian")
  brach[which(brach$formation=="Lower Calcareous Grit"),"late_interval"]<-c("Early Oxfordian") #According to ammonoid zone

  brach[which(brach$formation=="Lower Freestone"),"early_interval"]<-c("Aalenian")
  brach[which(brach$formation=="Lower Freestone"),"late_interval"]<-c("Aalenian") #Barron et al. 2012

  brach[brach$collection_no==204107,"early_interval"]<-c("Early Aalenian")
  brach[brach$collection_no==204107,"late_interval"]<-c("Early Aalenian") #Barron et al. 2012

  brach[which(brach$formation=="Lower Pospelovka"),"early_interval"]<-c("Early Kungurian")
  brach[which(brach$formation=="Lower Pospelovka"),"late_interval"]<-c("Early Kungurian") #Lower member, lower Kungurian

  brach[which(brach$formation=="Lower Shale-Siltstone"),"early_interval"]<-c("Late Tithonian")
  brach[which(brach$formation=="Lower Shale-Siltstone"),"late_interval"]<-c("Late Tithonian") #"Upper Tithonian" in PBDB

  brach[which(brach$formation=="Lugu"),"early_interval"]<-c("Late Kungurian")
  brach[which(brach$formation=="Lugu"),"late_interval"]<-c("Late Kungurian") #Xu et al. 2022

  brach[which(brach$formation=="Luning"&brach$early_interval=="Carnian"),"early_interval"]<-c("Tuvalian")
  brach[which(brach$formation=="Luning"&brach$late_interval=="Carnian"),"late_interval"]<-c("Tuvalian")
  brach[which(brach$formation=="Luning"&brach$early_interval=="Norian"),"early_interval"]<-c("Lacian")
  brach[which(brach$formation=="Luning"&brach$late_interval=="Norian"),"late_interval"]<-c("Lacian")
  brach[which(brach$formation=="Luning"&brach$early_interval=="Kaihikuan"),"early_interval"]<-c("Tuvalian")
  brach[which(brach$formation=="Luning"&brach$late_interval=="Kaihikuan"),"late_interval"]<-c("Lacian")

  brach[which(brach$formation=="luning"&brach$early_interval=="Carnian"),"early_interval"]<-c("Tuvalian")
  brach[which(brach$formation=="luning"&brach$late_interval=="Carnian"),"late_interval"]<-c("Tuvalian")
  brach[which(brach$formation=="luning"&brach$early_interval=="Norian"),"early_interval"]<-c("Lacian")
  brach[which(brach$formation=="luning"&brach$late_interval=="Norian"),"late_interval"]<-c("Lacian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Lunzer"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Lunzer"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Luojiadashan"),"early_interval"]<-c("Lacian")
  brach[which(brach$formation=="Luojiadashan"),"late_interval"]<-c("Lacian") #Tong et al. 2021

  brach[which(brach$formation=="Luolou"&brach$early_interval=="Changhsingian"),"early_interval"]<-c("Late Changhsingian")
  brach[which(brach$formation=="Luolou"&brach$late_interval=="Changhsingian"),"late_interval"]<-c("Late Changhsingian") #"Uppermost Changhsingian"

  brach[which(brach$formation=="Magan"),"early_interval"]<-c("Early Capitanian")
  brach[which(brach$formation=="Magan"),"late_interval"]<-c("Early Capitanian") #Maitaia bella Zone, lower Capitanian. Biakov 2013

  brach[which(brach$formation=="Magiveem"&brach$early_interval=="Gzhelian"),"late_interval"]<-c("Early Asselian") #"Gzhelian-lower Asselian" in PBDB

  brach[which(brach$formation=="Malton Oolite"),"early_interval"]<-c("Middle Oxfordian")
  brach[which(brach$formation=="Malton Oolite"),"late_interval"]<-c("Middle Oxfordian")  #According to ammonoid zone

  brach[which(brach$formation=="Mayang"),"early_interval"]<-c("Asselian")
  brach[which(brach$formation=="Mayang"),"late_interval"]<-c("Kungurian") #Lopingian in PBDB, but the "Stepanoviella" (=Cimmeriella) assemblage shows an Cisuralian age. See Shen et al. 2017

  brach[which(brach$formation=="Mangzongrong"),"early_interval"]<-c("Kungurian")
  brach[which(brach$formation=="Mangzongrong"),"late_interval"]<-c("Kungurian") #Lopingian in PBDB, but the "Stepanoviella" (=Cimmeriella) assemblage shows an Cisuralian age. Kungurian according to Xu et al. 2022

  brach[which(brach$formation=="Marnes de Latrecey"&brach$early_interval=="Late Oxfordian"),"early_interval"]<-c("Bifurcatus")
  brach[which(brach$formation=="Marnes de Latrecey"&brach$late_interval=="Late Oxfordian"),"late_interval"]<-c("Bifurcatus") #According to ammonoid zone

  brach[which(brach$formation=="Marsyangdi"),"early_interval"]<-c("Late Changhsingian")
  brach[which(brach$formation=="Marsyangdi"),"late_interval"]<-c("Late Changhsingian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Martin Bridge"&brach$early_interval=="Carnian"),"early_interval"]<-c("Tuvalian")
  brach[which(brach$formation=="Martin Bridge"&brach$late_interval=="Carnian"),"late_interval"]<-c("Tuvalian")
  brach[which(brach$formation=="Martin Bridge"&brach$late_interval=="Rhaetian"),"late_interval"]<-c("Norian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Matmor"),"early_interval"]<-c("Athleta")
  brach[which(brach$formation=="Matmor"),"late_interval"]<-c("Athleta") #According to ammonoid zone

  brach[which(brach$formation=="McCarthy"&brach$early_interval=="Norian"),"early_interval"]<-c("Sevatian")
  brach[which(brach$formation=="McCarthy"&brach$late_interval=="Norian"),"late_interval"]<-c("Sevatian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Megen"),"early_interval"]<-c("Asselian")
  brach[which(brach$formation=="Megen"),"late_interval"]<-c("Early Artinskian") #Makoshin and Kutygin 2020

  brach[which(brach$formation=="Mianwali"&brach$early_interval=="Changhsingian"),"early_interval"]<-c("Late Changhsingian")
  brach[which(brach$formation=="Mianwali"&brach$late_interval=="Changhsingian"),"late_interval"]<-c("Late Changhsingian") #PT transitional beds

  brach[which(brach$formation=="Middle Calcareous Grit"),"early_interval"]<-c("Plicatilis")
  brach[which(brach$formation=="Middle Calcareous Grit"),"late_interval"]<-c("Plicatilis") #According to ammonoid zone

  brach[which(brach$formation=="Milanovo"),"early_interval"]<-c("Fassanian")
  brach[which(brach$formation=="Milanovo"),"late_interval"]<-c("Fassanian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Mitarai"),"early_interval"]<-c("Late Tithonian")
  brach[which(brach$formation=="Mitarai"),"late_interval"]<-c("Early Berriasian") #Haggart and Matsukawa 2020

  brach[brach$collection_no%in%221258:221262,"early_interval"]<-c("Early Capitanian")
  brach[brach$collection_no%in%221258:221262,"late_interval"]<-c("Early Capitanian") #Maitaia bella Zone, lower Capitanian. Biakov 2013

  brach[brach$collection_no%in%221263:221265,"early_interval"]<-c("Early Capitanian")
  brach[brach$collection_no%in%221263:221265,"late_interval"]<-c("Early Capitanian") #Maitaia belliformis Zone, upper Capitanian. Biakov 2013

  brach[brach$collection_no==221266,"early_interval"]<-c("Late Wuchiapingian")
  brach[brach$collection_no==221266,"late_interval"]<-c("Early Changhsingian") #Lower Intomodesma costatum Zone. Biakov 2013

  brach[which(brach$formation=="Moewaka"),"early_interval"]<-c("Late Callovian")
  brach[which(brach$formation=="Moewaka"),"late_interval"]<-c("Middle Oxfordian") #"Early Heterian" in PBDB, late Callovian-early or middle Oxfordian

  brach[which(brach$formation=="Mol"),"early_interval"]<-c("Early Capitanian")
  brach[which(brach$formation=="Mol"),"late_interval"]<-c("Early Capitanian")
  brach[which(brach$formation=="Molskaya"),"early_interval"]<-c("Early Capitanian")
  brach[which(brach$formation=="Molskaya"),"late_interval"]<-c("Early Capitanian") #Biakov 2013

  brach[which(brach$formation=="Mont Grand"&brach$early_interval=="Late Toarcian"),"early_interval"]<-c("Aalensis")
  brach[which(brach$formation=="Mont Grand"&brach$late_interval=="Late Toarcian"),"late_interval"]<-c("Aalensis") #According to ammonoid zone

  brach[which(brach$formation=="Montejunto"),"early_interval"]<-c("Middle Oxfordian")
  brach[which(brach$formation=="Montejunto"),"late_interval"]<-c("Late Oxfordian") #Turner et al. 2017

  brach[which(brach$formation=="Moran"),"early_interval"]<-c("Sakmarian")
  brach[which(brach$formation=="Moran"),"late_interval"]<-c("Sakmarian") #Lucas 2018

  brach[which(brach$formation=="Morkvashi"),"early_interval"]<-c("Late Roadian")
  brach[which(brach$formation=="Morkvashi"),"late_interval"]<-c("Late Roadian") #Upper Kazanian

  brach[which(brach$formation=="Mount Greene"),"early_interval"]<-c("Asselian")
  brach[which(brach$formation=="Mount Greene"),"late_interval"]<-c("Sakmarian") #Kungurian in PBDB, but the brachiopods possible show an Asselian-Sakmarian age according to Waterhouse 2018 and Shi and Waterhouse 1996

  brach[brach$collection_no==20666,"early_interval"]<-c("Illyrian")
  brach[brach$collection_no==20666,"late_interval"]<-c("Illyrian") #This age of this collection should be the late Anisian according to the original reference

  brach[which(brach$formation=="Mughanniyya"&brach$early_interval=="Middle Jurassic"),"early_interval"]<-c("Callovian")
  brach[which(brach$formation=="Mughanniyya"&brach$late_interval=="Middle Jurassic"),"late_interval"]<-c("Callovian") #According to other collections of this formation

  brach[which(brach$formation=="Mugochan"),"early_interval"]<-c("Ufimian")
  brach[which(brach$formation=="Mugochan"),"late_interval"]<-c("Roadian") #Ufimian-Kazanian according to Biakov 2013

  brach[which(brach$formation=="Muong Trai"),"early_interval"]<-c("Longobardian")
  brach[which(brach$formation=="Muong Trai"),"late_interval"]<-c("Longobardian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Muschelkalk"&brach$member%in%c("Oberer Trochitenkalk","Ober Trochitenkalk","Trochitenkalk","Ceratitenschichten")),"early_interval"]<-c("Pelsonian")
  brach[which(brach$formation=="Muschelkalk"&brach$member%in%c("Oberer Trochitenkalk","Ober Trochitenkalk","Trochitenkalk","Ceratitenschichten")),"late_interval"]<-c("Illyrian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Muschelkalk"&brach$member=="Bank der klein terrebrat"),"early_interval"]<-c("Longobardian")
  brach[which(brach$formation=="Muschelkalk"&brach$member=="Bank der klein terrebrat"),"late_interval"]<-c("Longobardian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="N1"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="N1"),"late_interval"]<-c("Julian")
  brach[which(brach$formation=="N2"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="N2"),"late_interval"]<-c("Julian")
  brach[which(brach$formation=="N3"),"early_interval"]<-c("Tuvalian")
  brach[which(brach$formation=="N3"),"late_interval"]<-c("Tuvalian")
  brach[which(brach$formation=="N4"),"early_interval"]<-c("Lacian")
  brach[which(brach$formation=="N4"),"late_interval"]<-c("Lacian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Na Khuat"),"early_interval"]<-c("Ladinian")
  brach[which(brach$formation=="Na Khuat"),"late_interval"]<-c("Ladinian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Nabeyama"),"early_interval"]<-c("Kungurian")
  brach[which(brach$formation=="Nabeyama"),"late_interval"]<-c("Kungurian") #The Roadian collection should be Kungurian according to Tazawa et al. 2016

  brach[which(brach$formation=="Nadaska Limestones"),"early_interval"]<-c("Illyrian")
  brach[which(brach$formation=="Nadaska Limestones"),"late_interval"]<-c("Fassanian") #Near the Anisian/Ladinian boundry

  brach[which(brach$formation=="Naifa"&brach$member=="Billum"),"early_interval"]<-c("Late Oxfordian")
  brach[which(brach$formation=="Naifa"&brach$member=="Billum"),"late_interval"]<-c("Early Kimmeridgian")
  brach[which(brach$formation=="Naifa"&brach$member=="Kilya"),"early_interval"]<-c("Late Kimmeridgian")
  brach[which(brach$formation=="Naifa"&brach$member=="Kilya"),"late_interval"]<-c("Early Tithonian") #According to the original reference

  brach[which(brach$formation=="Nakatsuka"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Nakatsuka"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Nam Loong"),"early_interval"]<-c("Artinskian")
  brach[which(brach$formation=="Nam Loong"),"late_interval"]<-c("Artinskian") #Late Artinskian? See Ueno et al. 2015

  brach[which(brach$formation=="Nam Sam"),"early_interval"]<-c("Pelsonian")
  brach[which(brach$formation=="Nam Sam"),"late_interval"]<-c("Illyrian") #Fossils largely in upper part. Pelsonian forams were reported from the upper part of this formation by Miyahigashi et al. 2017

  brach[which(brach$formation=="Nam Tham"),"early_interval"]<-c("Fassanian")
  brach[which(brach$formation=="Nam Tham"),"late_interval"]<-c("Fassanian") #"Early Ladinian" in PBDB

  brach[which(brach$formation=="Nanlong"),"early_interval"]<-c("Wordian")
  brach[which(brach$formation=="Nanlong"),"late_interval"]<-c("Capitanian") #Late Maokouan in the original reference

  brach[which(brach$formation=="Naocangjiangou"),"early_interval"]<-c("Anisian")
  brach[which(brach$formation=="Naocangjiangou"),"late_interval"]<-c("Anisian") #Aegean-Bithynian in PBDB, but some brachiopod species are from the middle-late Anisian in the adjacent Qilian Mountains

  brach[which(brach$formation=="Narawara"),"early_interval"]<-c("Olenekian")
  brach[which(brach$formation=="Narawara"),"late_interval"]<-c("Olenekian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Nayband"&brach$member%in%c("Howz-e Khan","Howz-e-Khan")),"early_interval"]<-c("Early Rhaetian")
  brach[which(brach$formation=="Nayband"&brach$member%in%c("Howz-e Khan","Howz-e-Khan")),"late_interval"]<-c("Early Rhaetian")
  brach[which(brach$formation=="Nayband"&brach$member=="Howz-e-Sheikh"),"early_interval"]<-c("Sevatian")
  brach[which(brach$formation=="Nayband"&brach$member=="Howz-e-Sheikh"),"late_interval"]<-c("Early Rhaetian")
  brach[which(brach$formation=="Nayband"&brach$early_interval=="Norian"),"early_interval"]<-c("Alaunian")
  brach[which(brach$formation=="Nayband"&brach$late_interval=="Norian"),"late_interval"]<-c("Sevatian") #Sabbaghiyan et al. 2020

  brach[which(brach$formation=="Nekuchan"&brach$early_interval=="Changhsingian"),"early_interval"]<-c("Late Changhsingian")
  brach[which(brach$formation=="Nekuchan"&brach$late_interval=="Changhsingian"),"late_interval"]<-c("Late Changhsingian") #Biakov et al. 2018

  brach[which(brach$formation=="Nenyugin"),"early_interval"]<-c("Late Roadian")
  brach[which(brach$formation=="Nenyugin"),"late_interval"]<-c("Wordian") #Biakov 2013

  brach[brach$collection_no==110234,"early_interval"]<-c("Callovian")
  brach[brach$collection_no==110234,"late_interval"]<-c("Callovian") #Upper Member, Nieniexiongla Formation, Callovian according to Sun et al. 2017

  brach[which(brach$formation=="Nikitin"),"early_interval"]<-c("Late Changhsingian")
  brach[which(brach$formation=="Nikitin"),"late_interval"]<-c("Late Changhsingian") #"Late Changhsingian" in PBDB

  brach[which(brach$formation=="Nishinakayama"&brach$early_interval=="Pliensbachian"),"early_interval"]<-c("Late Pliensbachian")
  brach[which(brach$formation=="Nishinakayama"&brach$late_interval=="Pliensbachian"),"late_interval"]<-c("Late Pliensbachian")
  brach[which(brach$formation=="Nishinakayama"&brach$early_interval=="Toarcian"),"early_interval"]<-c("Early Toarcian")
  brach[which(brach$formation=="Nishinakayama"&brach$late_interval=="Toarcian"),"late_interval"]<-c("Early Toarcian") #Nakada et al. 2021

  brach[which(brach$formation=="Nordenskj枚ld"),"early_interval"]<-c("Late Tithonian")
  brach[which(brach$formation=="Nordenskj枚ld"),"late_interval"]<-c("Late Tithonian") #Nordenskjöld Formation. Ameghino Member

  brach[which(brach$formation=="Nothe Grits"),"early_interval"]<-c("Plicatilis")
  brach[which(brach$formation=="Nothe Grits"),"late_interval"]<-c("Plicatilis") #According to ammonoid zone

  brach[which(brach$formation=="Nunuluka"),"early_interval"]<-c("Sevatian")
  brach[which(brach$formation=="Nunuluka"),"late_interval"]<-c("Sevatian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Nusplingen Limestone"),"early_interval"]<-c("Beckeri")
  brach[which(brach$formation=="Nusplingen Limestone"),"late_interval"]<-c("Beckeri") #According to ammonoid zone

  brach[which(brach$formation=="Oberrhaet"),"early_interval"]<-c("Late Rhaetian")
  brach[which(brach$formation=="Oberrhaet"),"late_interval"]<-c("Late Rhaetian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Ohautira"&brach$early_interval=="Toarcian"),"early_interval"]<-c("Bajocian")
  brach[which(brach$formation=="Ohautira"&brach$late_interval=="Hettangian"),"late_interval"]<-c("Callovian") #"Late Temaikan" in PBDB

  brach[which(brach$formation=="Ohautira Conglomerate"),"early_interval"]<-c("Bajocian")
  brach[which(brach$formation=="Ohautira Conglomerate"),"late_interval"]<-c("Callovian") #"Late Temaikan" in PBDB

  brach[which(brach$formation=="Ohinereru"),"early_interval"]<-c("Late Oxfordian")
  brach[which(brach$formation=="Ohinereru"),"late_interval"]<-c("Early Kimmeridgian") #"Middle Heterian" in PBDB

  brach[which(brach$collection_no%in%c("35723","56292","63763","63764","133848","134103","134110","134127","134135","182757","182758","182759","182760","182784")),"early_interval"]<-c("Late Oxfordian")
  brach[which(brach$collection_no%in%c("35723","56292","63763","63764","133848","134103","134110","134127","134135","182757","182758","182759","182760","182784")),"late_interval"]<-c("Early Kimmeridgian") #"Middle Heterian"
  brach[which(brach$collection_no%in%c("63745","63746","63747","63749")),"early_interval"]<-c("Oxfordian")
  brach[which(brach$collection_no%in%c("63745","63746","63747","63749")),"late_interval"]<-c("Oxfordian") #"Lower Heterian"
  brach[which(brach$collection_no%in%c("182718","182719","182738")),"early_interval"]<-c("Late Oxfordian")
  brach[which(brach$collection_no%in%c("182718","182719","182738")),"late_interval"]<-c("Kimmeridgian") #"Middle-Late Heterian"
  brach[which(brach$collection_no%in%c("63766","63765")),"early_interval"]<-c("Tithonian")
  brach[which(brach$collection_no%in%c("63766","63765")),"late_interval"]<-c("Tithonian") #"Puaroan (Waikatoan substage)". These assignments are based on the correlation of fossils, not the absolute ages of the New Zealand regional stages

  brach[which(brach$formation=="Olinala"),"early_interval"]<-c("Late Capitanian")
  brach[which(brach$formation=="Olinala"),"late_interval"]<-c("Late Capitanian") #"Late Capitanian" in PBDB

  brach[which(brach$formation=="Olynian"),"early_interval"]<-c("Late Roadian")
  brach[which(brach$formation=="Olynian"),"late_interval"]<-c("Late Roadian") #Kolymia plicata bivalve zone, late Kazanian

  brach[which(brach$formation=="Omchak"&brach$early_interval=="Wuchiapingian"),"early_interval"]<-c("Early Wuchiapingian")
  brach[which(brach$formation=="Omchak"&brach$late_interval=="Wuchiapingian"),"late_interval"]<-c("Early Wuchiapingian")
  brach[which(brach$formation=="Omchak"&brach$early_interval=="Capitanian"),"early_interval"]<-c("Late Capitanian")
  brach[which(brach$formation=="Omchak"&brach$late_interval=="Capitanian"),"late_interval"]<-c("Late Capitanian") #Biakov 2013

  brach[which(brach$formation=="Oolite Marl"),"early_interval"]<-c("Aalenian")
  brach[which(brach$formation=="Oolite Marl"),"late_interval"]<-c("Aalenian") #Barron et al. 2012

  brach[which(brach$formation=="Oolithe de Trouville"),"early_interval"]<-c("Plicatilis")
  brach[which(brach$formation=="Oolithe de Trouville"),"late_interval"]<-c("Plicatilis") #According to ammonoid zone

  brach[which(brach$formation=="Oraka"),"early_interval"]<-c("Late Oxfordian")
  brach[which(brach$formation=="Oraka"),"late_interval"]<-c("Early Kimmeridgian") #"Middle Heterian" in PBDB

  brach[which(brach$formation=="Orn"&brach$early_interval=="Asselian"),"early_interval"]<-c("Early Asselian")
  brach[which(brach$formation=="Orn"&brach$late_interval=="Asselian"),"late_interval"]<-c("Early Asselian") #"Early Asselian" in PBDB
  brach[which(brach$formation=="Orn"&brach$early_interval=="Sakmarian"),"early_interval"]<-c("Early Sakmarian")
  brach[which(brach$formation=="Orn"&brach$late_interval=="Sakmarian"),"late_interval"]<-c("Early Sakmarian") #"Early Sakmarian" in PBDB

  brach[which(brach$formation=="Oro"),"early_interval"]<-c("Olenekian")
  brach[which(brach$formation=="Oro"),"late_interval"]<-c("Olenekian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Osgodby"&brach$member=="Redcliff"),"early_interval"]<-c("Koenigi")
  brach[which(brach$formation=="Osgodby"&brach$member=="Redcliff"),"late_interval"]<-c("Koenigi")
  brach[which(brach$formation=="Osgodby"&brach$member=="Hackness Rock"),"early_interval"]<-c("Lamberti")
  brach[which(brach$formation=="Osgodby"&brach$member=="Hackness Rock"),"late_interval"]<-c("Lamberti") #According to ammonoid zone

  brach[which(brach$formation=="Osipa"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Osipa"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Osmington Oolite"),"early_interval"]<-c("Transversarium")
  brach[which(brach$formation=="Osmington Oolite"),"late_interval"]<-c("Transversarium") #According to ammonoid zone

  brach[which(brach$formation=="Osobb"),"early_interval"]<-c("Lacian")
  brach[which(brach$formation=="Osobb"),"late_interval"]<-c("Lacian") #According to the original reference

  brach[which(brach$formation=="Otaniyama"),"early_interval"]<-c("Berriasian")
  brach[which(brach$formation=="Otaniyama"),"late_interval"]<-c("Berriasian") #Haggart and Matsukawa 2020

  brach[which(brach$formation=="Ozerninskaya"),"early_interval"]<-c("Late Kungurian")
  brach[which(brach$formation=="Ozerninskaya"),"late_interval"]<-c("Late Kungurian") #Kotlyar et al. 2018

  brach[which(brach$collection_no%in%c("225540","225541","225542","225546","225547","225551","225552","225554","225555","225556")),"early_interval"]<-c("Spinatum")
  brach[which(brach$collection_no%in%c("225540","225541","225542","225546","225547","225551","225552","225554","225555","225556")),"late_interval"]<-c("Spinatum") #According to ammonoid zone

  brach[which(brach$collection_no%in%c("225543","225548")),"early_interval"]<-c("Tenuicostatum")
  brach[which(brach$collection_no%in%c("225543","225548")),"late_interval"]<-c("Tenuicostatum") #According to ammonoid zone

  brach[which(brach$collection_no%in%c("225544","225549")),"early_interval"]<-c("Falciferum")
  brach[which(brach$collection_no%in%c("225544","225549")),"late_interval"]<-c("Falciferum") #According to ammonoid zone

  brach[which(brach$collection_no%in%c("225545","225550")),"early_interval"]<-c("Bifrons")
  brach[which(brach$collection_no%in%c("225545","225550")),"late_interval"]<-c("Bifrons") #According to ammonoid zone

  brach[which(brach$collection_no==225553),"early_interval"]<-c("Margaritatus")
  brach[which(brach$collection_no==225553),"late_interval"]<-c("Margaritatus") #According to ammonoid zone

  brach[which(brach$formation=="Painten"),"early_interval"]<-c("Beckeri")
  brach[which(brach$formation=="Painten"),"late_interval"]<-c("Beckeri") #According to ammonoid zone

  brach[brach$collection_no%in%c("47801","47905","47907","47908","47913"),"early_interval"]<-c("Wuchiapingian")
  brach[brach$collection_no%in%c("47801","47905","47907","47908","47913"),"late_interval"]<-c("Wuchiapingian")
  brach[brach$collection_no%in%c("47929","47958","47968","48000"),"early_interval"]<-c("Changhsingian")
  brach[brach$collection_no%in%c("47929","47958","47968","48000"),"late_interval"]<-c("Changhsingian") #Pamucak Formation. Ages following Angioloni et al. 2007

  brach[which(brach$collection_no==210188),"early_interval"]<-c("Planorbis")
  brach[which(brach$collection_no==210188),"late_interval"]<-c("Planorbis") #According to ammonoid zone

  brach[which(brach$formation=="Pangjang"),"early_interval"]<-c("Griesbachian")
  brach[which(brach$formation=="Pangjang"),"late_interval"]<-c("Griesbachian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Pardonet"&brach$early_interval=="Carnian"),"early_interval"]<-c("Tuvalian")
  brach[which(brach$formation=="Pardonet"&brach$late_interval=="Carnian"),"late_interval"]<-c("Tuvalian")
  brach[which(brach$formation=="Pardonet"&brach$early_interval=="Julian"),"early_interval"]<-c("Tuvalian")
  brach[which(brach$formation=="Pardonet"&brach$late_interval=="Julian"),"late_interval"]<-c("Tuvalian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Park City"&brach$member=="Grandeur"),"early_interval"]<-c("Kungurian")
  brach[which(brach$formation=="Park City"&brach$member=="Grandeur"),"late_interval"]<-c("Kungurian")
  brach[which(brach$formation=="Park City"&brach$member=="Ervay"),"early_interval"]<-c("Late Wuchiapingian")
  brach[which(brach$formation=="Park City"&brach$member=="Ervay"),"late_interval"]<-c("Early Changhsingian")
  brach[which(brach$formation=="Park City"&brach$member=="Franson"),"early_interval"]<-c("Early Wuchiapingian")
  brach[which(brach$formation=="Park City"&brach$member=="Franson"),"late_interval"]<-c("Early Wuchiapingian") #Wistort and Ritterbush 2022

  brach[which(brach$formation=="Passage Beds"&brach$cc=="UK"),"early_interval"]<-c("Cordatum")
  brach[which(brach$formation=="Passage Beds"&brach$cc=="UK"),"late_interval"]<-c("Cordatum") #According to ammonoid zone

  brach[which(brach$formation=="Pastakh"),"early_interval"]<-c("Spathian")
  brach[which(brach$formation=="Pastakh"),"late_interval"]<-c("Spathian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Pastanakhskaya/Ystannakhskaya"),"early_interval"]<-c("Spathian")
  brach[which(brach$formation=="Pastanakhskaya/Ystannakhskaya"),"late_interval"]<-c("Spathian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Payand茅"),"early_interval"]<-c("Alaunian")
  brach[which(brach$formation=="Payand茅"),"late_interval"]<-c("Sevatian") #The formation name should be Payandé. Mojica and Prinz-Grimm 2000

  brach[which(brach$collection_no==204106),"early_interval"]<-c("Murchisonae")
  brach[which(brach$collection_no==204106),"late_interval"]<-c("Murchisonae") #Barron et al. 2012

  brach[which(brach$formation=="Pechishchi"),"early_interval"]<-c("Late Roadian")
  brach[which(brach$formation=="Pechishchi"),"late_interval"]<-c("Late Roadian") #"Upper Kazanian" in PBDB

  brach[which(brach$formation=="Peschanka"),"early_interval"]<-c("Lacian")
  brach[which(brach$formation=="Peschanka"),"late_interval"]<-c("Alaunian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Pha Daeng"),"early_interval"]<-c("Carnian")
  brach[which(brach$formation=="Pha Daeng"),"late_interval"]<-c("Carnian") #Feng et al. 2005

  brach[which(brach$formation=="Philippovian"),"early_interval"]<-c("Early Kungurian")
  brach[which(brach$formation=="Philippovian"),"late_interval"]<-c("Early Kungurian") #"Lower Kungurian" in PBDB

  brach[which(brach$formation=="Phosphoria"&brach$member=="Ervay"),"early_interval"]<-c("Late Wuchiapingian")
  brach[which(brach$formation=="Phosphoria"&brach$member=="Ervay"),"late_interval"]<-c("Early Changhsingian")
  brach[which(brach$formation=="Phosphoria"&brach$member=="Tosi Chert"),"early_interval"]<-c("Late Wuchiapingian")
  brach[which(brach$formation=="Phosphoria"&brach$member=="Tosi Chert"),"late_interval"]<-c("Early Changhsingian")
  brach[which(brach$formation=="Phosphoria"&brach$member=="Tosi"),"early_interval"]<-c("Late Wuchiapingian")
  brach[which(brach$formation=="Phosphoria"&brach$member=="Tosi"),"late_interval"]<-c("Early Changhsingian")
  brach[which(brach$formation=="Phosphoria"&brach$member=="Retor"),"early_interval"]<-c("Late Wuchiapingian")
  brach[which(brach$formation=="Phosphoria"&brach$member=="Retor"),"late_interval"]<-c("Late Wuchiapingian")
  brach[which(brach$formation=="Phosphoria"&brach$member=="Retort Phosphatic Shale"),"early_interval"]<-c("Late Wuchiapingian")
  brach[which(brach$formation=="Phosphoria"&brach$member=="Retort Phosphatic Shale"),"late_interval"]<-c("Late Wuchiapingian")
  brach[which(brach$formation=="Phosphoria"&brach$member=="Franson"),"early_interval"]<-c("Early Wuchiapingian")
  brach[which(brach$formation=="Phosphoria"&brach$member=="Franson"),"late_interval"]<-c("Early Wuchiapingian")
  brach[which(brach$formation=="Phosphoria"&brach$member=="Rex Chert"),"early_interval"]<-c("Early Wuchiapingian")
  brach[which(brach$formation=="Phosphoria"&brach$member=="Rex Chert"),"late_interval"]<-c("Early Wuchiapingian")
  brach[which(brach$formation=="Phosphoria"&brach$member=="Rex"),"early_interval"]<-c("Early Wuchiapingian")
  brach[which(brach$formation=="Phosphoria"&brach$member=="Rex"),"late_interval"]<-c("Early Wuchiapingian") #Wistort and Ritterbush 2022

  brach[which(brach$collection_no%in%c("10701","10702")),"early_interval"]<-c("Ibex")
  brach[which(brach$collection_no%in%c("10701","10702")),"late_interval"]<-c("Davoei")  #According to ammonoid zone

  brach[which(brach$collection_no%in%c("33850","33851","33853","33855","36896")),"early_interval"]<-c("Davoei")
  brach[which(brach$collection_no%in%c("33850","33851","33853","33855","36896")),"late_interval"]<-c("Spinatum")  #According to ammonoid zone

  brach[which(brach$formation=="Pionerskii"),"early_interval"]<-c("Wordian")
  brach[which(brach$formation=="Pionerskii"),"late_interval"]<-c("Wordian") #Kolymia multiformis Zone, Wordian

  brach[which(brach$formation=="Piqiang"&brach$early_interval=="Artinskian"),"early_interval"]<-c("Early Artinskian")
  brach[which(brach$formation=="Piqiang"&brach$late_interval=="Artinskian"),"late_interval"]<-c("Early Artinskian") #"Early Artinskian" in PBDB

  brach[which(brach$formation=="Pit"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Pit"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Portland Limestone"),"early_interval"]<-c("Anguiformis")
  brach[which(brach$formation=="Portland Limestone"),"late_interval"]<-c("Anguiformis") #According to ammonoid zone

  brach[which(brach$formation=="Pounawea"),"early_interval"]<-c("Early Callovian")
  brach[which(brach$formation=="Pounawea"),"late_interval"]<-c("Middle Callovian") #Gardner and Campbell 2002. "Molluscan taxa from Pounawea Formation have strong Callovian affinities."

  brach[which(brach$formation=="Prikazan"),"early_interval"]<-c("Late Roadian")
  brach[which(brach$formation=="Prikazan"),"late_interval"]<-c("Late Roadian") #Upper Kazanian

  brach[which(brach$formation=="Privolninskaya"),"early_interval"]<-c("Late Changhsingian")
  brach[which(brach$formation=="Privolninskaya"),"late_interval"]<-c("Late Changhsingian") #Biakov and Kutygin 2021

  brach[which(brach$formation=="Privolynin"),"early_interval"]<-c("Late Wuchiapingian")
  brach[which(brach$formation=="Privolynin"),"late_interval"]<-c("Changhsingian") #Davydov et al. 2022

  brach[which(brach$formation=="Pueblo"),"early_interval"]<-c("Asselian")
  brach[which(brach$formation=="Pueblo"),"late_interval"]<-c("Asselian") #Lucas 2018

  brach[which(brach$formation=="Putnam"),"early_interval"]<-c("Early Artinskian")
  brach[which(brach$formation=="Putnam"),"late_interval"]<-c("Early Artinskian") #Lucas 2018. Coleman Junction ls

  brach[which(brach$formation=="Qieerma"),"early_interval"]<-c("Pelsonian")
  brach[which(brach$formation=="Qieerma"),"late_interval"]<-c("Illyrian") #Upper part of the Anisian Junzihe Group

  brach[which(brach$formation=="Qingyan"),"early_interval"]<-c("Pelsonian")
  brach[which(brach$formation=="Qingyan"),"late_interval"]<-c("Illyrian") #These collections were from the Leidapo and Yuqing members of the middle-upper Anisian. The Yingshangpo records of Chen J. et al. 2010 will be discarded

  brach[which(brach$formation=="Qipan"&brach$early_interval=="Artinskian"),"early_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Qipan"&brach$late_interval=="Artinskian"),"late_interval"]<-c("Late Artinskian") #According to brachiopod zone

  brach[which(brach$collection_no%in%62740:62753),"early_interval"]<-c("Early Kungurian")
  brach[which(brach$collection_no%in%62740:62753),"late_interval"]<-c("Early Kungurian")
  brach[which(brach$collection_no%in%62757:62764),"early_interval"]<-c("Late Kungurian")
  brach[which(brach$collection_no%in%62757:62764),"late_interval"]<-c("Late Kungurian") #According to brachiopod zone

  brach[which(brach$formation=="Qixia"&brach$early_interval=="Artinskian"),"early_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Qixia"&brach$late_interval=="Artinskian"),"late_interval"]<-c("Late Artinskian") #Shen et al. 2021

  brach[which(brach$formation=="Qubuerga"&brach$ref_author=="Xu et al."),"early_interval"]<-c("Wuchiapingian")
  brach[which(brach$formation=="Qubuerga"&brach$ref_author=="Xu et al."),"late_interval"]<-c("Early Changhsingian") #According to the original reference

  brach[which(brach$formation=="Qudi"),"early_interval"]<-c("Artinskian")
  brach[which(brach$formation=="Qudi"),"late_interval"]<-c("Artinskian") #Following Shen et al. 2021

  brach[which(brach$formation=="Qulonggongba"),"early_interval"]<-c("Alaunian")
  brach[which(brach$formation=="Qulonggongba"),"late_interval"]<-c("Alaunian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Quy Lang"),"early_interval"]<-c("Pelsonian")
  brach[which(brach$formation=="Quy Lang"),"late_interval"]<-c("Pelsonian") #Shigeta et al. 2010

  brach[which(brach$formation=="Ramla"),"early_interval"]<-c("Early Bathonian")
  brach[which(brach$formation=="Ramla"),"late_interval"]<-c("Early Bathonian") #Ahmad et al. 2017

  brach[which(brach$formation=="Ramsau Dolomite"&brach$early_interval=="Carnian"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Ramsau Dolomite"&brach$late_interval=="Carnian"),"late_interval"]<-c("Julian") #Missoni and Gawlick 2011

  brach[which(brach$formation=="Rat Buri"),"early_interval"]<-c("Roadian")
  brach[which(brach$formation=="Rat Buri"),"late_interval"]<-c("Roadian") #Waterhouse 2013

  brach[which(brach$formation=="Red Eagle Limestone"),"early_interval"]<-c("Early Asselian")
  brach[which(brach$formation=="Red Eagle Limestone"),"late_interval"]<-c("Early Asselian") #Wahlman and West 2010

  brach[which(brach$formation=="Redcar Mudstone"),"early_interval"]<-c("Late Rhaetian")
  brach[which(brach$formation=="Redcar Mudstone"),"late_interval"]<-c("Late Rhaetian") #The original reference

  brach[which(brach$formation=="Reiflingerkalk"),"early_interval"]<-c("Illyrian")
  brach[which(brach$formation=="Reiflingerkalk"),"late_interval"]<-c("Illyrian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Renggeri Marl"),"early_interval"]<-c("Mariae")
  brach[which(brach$formation=="Renggeri Marl"),"late_interval"]<-c("Transversarium") #According to ammonoid zone

  brach[which(brach$formation=="Reshetnikovo"),"early_interval"]<-c("Late Kungurian")
  brach[which(brach$formation=="Reshetnikovo"),"late_interval"]<-c("Early Roadian") #Kotlyar 2015

  brach[which(brach$formation=="Rezi Dolomite"),"early_interval"]<-c("Sevatian")
  brach[which(brach$formation=="Rezi Dolomite"),"late_interval"]<-c("Sevatian") #Haas et al. 2022

  brach[which(brach$formation=="Richthofen"),"early_interval"]<-c("Illyrian")
  brach[which(brach$formation=="Richthofen"),"late_interval"]<-c("Illyrian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Ringstead Coral Bed"),"early_interval"]<-c("Baylei")
  brach[which(brach$formation=="Ringstead Coral Bed"),"late_interval"]<-c("Baylei") #According to ammonoid zone

  brach[which(brach$formation=="Riva di Solto"),"early_interval"]<-c("Sevatian")
  brach[which(brach$formation=="Riva di Solto"),"late_interval"]<-c("Sevatian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Road"&brach$early_interval=="Roadian"),"early_interval"]<-c("Early Roadian")
  brach[which(brach$formation=="Road"&brach$late_interval=="Roadian"),"late_interval"]<-c("Early Roadian")
  brach[which(brach$formation=="Road Canyon"&brach$early_interval=="Roadian"),"early_interval"]<-c("Early Roadian")
  brach[which(brach$formation=="Road Canyon"&brach$late_interval=="Roadian"),"late_interval"]<-c("Early Roadian")
  brach[which(brach$formation=="Road Canyon"&brach$early_interval=="Kungurian"),"early_interval"]<-c("Late Kungurian")
  brach[which(brach$formation=="Road Canyon"&brach$late_interval=="Kungurian"),"late_interval"]<-c("Late Kungurian") #Menning 2006

  brach[which(brach$formation=="Roca Shale"),"early_interval"]<-c("Early Asselian")
  brach[which(brach$formation=="Roca Shale"),"late_interval"]<-c("Early Asselian") #Wahlman and West 2010

  brach[which(brach$formation=="Rodiles"&brach$early_interval=="Late Toarcian"),"early_interval"]<-c("Aalensis")
  brach[which(brach$formation=="Rodiles"&brach$late_interval=="Late Toarcian"),"late_interval"]<-c("Aalensis")
  brach[which(brach$formation=="Rodiles"&brach$early_interval=="Levesquei"),"early_interval"]<-c("Pseudoradiosa")
  brach[which(brach$formation=="Rodiles"&brach$late_interval=="Levesquei"),"late_interval"]<-c("Pseudoradiosa") #According to ammonoid zone

  brach[which(brach$formation=="Rustler"),"early_interval"]<-c("Early Changhsingian")
  brach[which(brach$formation=="Rustler"),"late_interval"]<-c("Early Changhsingian") #"Early Changhsingian" in PBDB

  brach[which(brach$formation=="Sadlerochit"),"early_interval"]<-c("Roadian")
  brach[which(brach$formation=="Sadlerochit"),"late_interval"]<-c("Roadian") #Lee et al. 2019

  brach[which(brach$formation=="Sakhana"),"early_interval"]<-c("Ufimian")
  brach[which(brach$formation=="Sakhana"),"late_interval"]<-c("Ufimian") #According to the original reference

  brach[which(brach$formation=="Salmon River"),"early_interval"]<-c("Late Toarcian")
  brach[which(brach$formation=="Salmon River"),"late_interval"]<-c("Early Bajocian") #Gagnon et al. 2012

  brach[which(brach$formation=="Salperton Limestone"&brach$early_interval=="Bajocian"),"early_interval"]<-c("Late Bajocian")
  brach[which(brach$formation=="Salperton Limestone"&brach$late_interval=="Bajocian"),"late_interval"]<-c("Late Bajocian") #Barron et al. 2012

  brach[which(brach$formation=="San Cassiano"&brach$early_interval=="Ladinian"),"early_interval"]<-c("Longobardian")
  brach[which(brach$formation=="San Cassiano"&brach$late_interval=="Ladinian"),"late_interval"]<-c("Longobardian") #"Uppermost Ladinian"
  brach[which(brach$formation=="San Cassiano"&brach$early_interval=="Carnian"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="San Cassiano"&brach$late_interval=="Carnian"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="San Hipolito"&brach$member=="Limestone"),"early_interval"]<-c("Sevatian")
  brach[which(brach$formation=="San Hipolito"&brach$member=="Limestone"),"late_interval"]<-c("Sevatian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$collection_no==1573),"early_interval"]<-c("Cautisnigrae")
  brach[which(brach$collection_no==1573),"late_interval"]<-c("Cautisnigrae") #According to brachiopod zone
  brach[which(brach$collection_no%in%1567:1568),"early_interval"]<-c("Pseudocordata")
  brach[which(brach$collection_no%in%1567:1568),"late_interval"]<-c("Pseudocordata") #According to brachiopod zone

  brach[which(brach$formation=="Sanhedong"),"early_interval"]<-c("Lacian")
  brach[which(brach$formation=="Sanhedong"),"late_interval"]<-c("Lacian") #Wu et al. 2022

  brach[which(brach$formation=="Sanqiao"),"early_interval"]<-c("Tuvalian")
  brach[which(brach$formation=="Sanqiao"),"late_interval"]<-c("Norian") #Carnian in old references, Norian in Tong et al. 2021

  brach[which(brach$formation=="Santa Clara"),"early_interval"]<-c("Tuvalian")
  brach[which(brach$formation=="Santa Clara"),"late_interval"]<-c("Tuvalian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Santa Linya"&brach$early_interval=="Late Toarcian"),"early_interval"]<-c("Aalensis") #"Toarcian-Aalenian boundary beds"

  brach[which(brach$collection_no%in%225387:225388),"early_interval"]<-c("Tenuicostatum")
  brach[which(brach$collection_no%in%225387:225388),"late_interval"]<-c("Tenuicostatum")
  brach[which(brach$collection_no==225389),"early_interval"]<-c("Serpentinum")
  brach[which(brach$collection_no==225389),"late_interval"]<-c("Serpentinum") #According to ammonoid zone

  brach[which(brach$formation=="Sarga"),"early_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Sarga"),"late_interval"]<-c("Late Artinskian") #Naugolnykh 2020

  brach[which(brach$formation=="Savina"),"early_interval"]<-c("Late Capitanian")
  brach[which(brach$formation=="Savina"),"late_interval"]<-c("Late Capitanian") #See Biakov 2013

  brach[which(brach$formation=="Sawtooth"&brach$early_interval=="Bajocian"),"early_interval"]<-c("Late Bajocian")
  brach[which(brach$formation=="Sawtooth"&brach$late_interval=="Bajocian"),"late_interval"]<-c("Late Bajocian")
  brach[which(brach$formation=="Sawtooth"&brach$early_interval=="Bathonian"),"early_interval"]<-c("Early Bathonian")
  brach[which(brach$formation=="Sawtooth"&brach$late_interval=="Bathonian"),"late_interval"]<-c("Early Bathonian") #Parcell and Williams 2005

  brach[which(brach$formation=="Scarborough"),"early_interval"]<-c("Humphriesianum")
  brach[which(brach$formation=="Scarborough"),"late_interval"]<-c("Humphriesianum") #Barron et al. 2012

  brach[which(brach$formation=="Schlern 1"),"early_interval"]<-c("Illyrian")
  brach[which(brach$formation=="Schlern 1"),"late_interval"]<-c("Fassanian")
  brach[which(brach$collection_no==49915),"early_interval"]<-c("Illyrian")
  brach[which(brach$collection_no==49915),"late_interval"]<-c("Illyrian") #According to the original reference

  brach[which(brach$formation=="Schlern"&brach$ref_author=="Friesenbichler et al."),"early_interval"]<-c("Longobardian")
  brach[which(brach$formation=="Schlern"&brach$ref_author=="Friesenbichler et al."),"late_interval"]<-c("Longobardian")
  brach[which(brach$collection_no%in%202042:202043),"early_interval"]<-c("Julian")
  brach[which(brach$collection_no%in%202042:202043),"late_interval"]<-c("Julian") #According to the original reference

  brach[which(brach$formation=="Schn枚ll"),"early_interval"]<-c("Hettangian")
  brach[which(brach$formation=="Schn枚ll"),"late_interval"]<-c("Hettangian")
  brach[which(brach$formation=="Sch枚ll"),"early_interval"]<-c("Hettangian")
  brach[which(brach$formation=="Sch枚ll"),"late_interval"]<-c("Hettangian") #The formation name should be Schnöll. According to the original reference

  brach[which(brach$formation=="Schuchert Dal"),"early_interval"]<-c("Changhsingian")
  brach[which(brach$formation=="Schuchert Dal"),"late_interval"]<-c("Changhsingian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="scissum Beds"),"early_interval"]<-c("Early Aalenian")
  brach[which(brach$formation=="scissum Beds"),"late_interval"]<-c("Early Aalenian") #Barron et al. 2012

  brach[which(brach$formation=="Sedrina"&brach$early_interval=="Early Hettangian"),"early_interval"]<-c("Planorbis")
  brach[which(brach$formation=="Sedrina"&brach$late_interval=="Early Hettangian"),"late_interval"]<-c("Planorbis")
  brach[which(brach$formation=="Sedrina"&brach$early_interval=="Late Hettangian"),"early_interval"]<-c("Angulata")
  brach[which(brach$formation=="Sedrina"&brach$late_interval=="Late Hettangian"),"late_interval"]<-c("Angulata") #According to ammonoid zone

  brach[which(brach$formation=="Selander"),"early_interval"]<-c("Wuchiapingian")
  brach[which(brach$formation=="Selander"),"late_interval"]<-c("Wuchiapingian") #Blomeier et al. 2013

  brach[which(brach$formation=="Senja"),"early_interval"]<-c("Early Changhsingian")
  brach[which(brach$formation=="Senja"),"late_interval"]<-c("Early Changhsingian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Seven Rivers"),"early_interval"]<-c("Early Capitanian")
  brach[which(brach$formation=="Seven Rivers"),"late_interval"]<-c("Early Capitanian") #Nestell and Nestell 2006

  brach[which(brach$formation=="Shalem Colony"),"early_interval"]<-c("Early Asselian")
  brach[which(brach$formation=="Shalem Colony"),"late_interval"]<-c("Early Asselian") #Lucas et al. 2015

  brach[which(brach$formation=="Shedhorn"),"early_interval"]<-c("Wuchiapingian")
  brach[which(brach$formation=="Shedhorn"),"late_interval"]<-c("Early Changhsingian")
  brach[which(brach$formation=="Shedhorn"&brach$member=="Lower"),"early_interval"]<-c("Early Wuchiapingian")
  brach[which(brach$formation=="Shedhorn"&brach$member=="Lower"),"late_interval"]<-c("Early Wuchiapingian")
  brach[which(brach$formation=="Shedhorn"&brach$member=="Upper"),"early_interval"]<-c("Late Wuchiapingian")
  brach[which(brach$formation=="Shedhorn"&brach$member=="Upper"),"late_interval"]<-c("Early Changhsingian") #Wistort and Ritterbush 2022

  brach[which(brach$formation=="Sherkirtinskie"),"early_interval"]<-c("Ufimian")
  brach[which(brach$formation=="Sherkirtinskie"),"late_interval"]<-c("Ufimian") #"Ufimian" in PBDB

  brach[which(brach$formation=="Sheshma"),"early_interval"]<-c("Ufimian")
  brach[which(brach$formation=="Sheshma"),"late_interval"]<-c("Ufimian") #"Ufimian" in PBDB

  brach[which(brach$formation=="Sheshmian"),"early_interval"]<-c("Ufimian")
  brach[which(brach$formation=="Sheshmian"),"late_interval"]<-c("Early Roadian") #"Ufimian/Lower Kazanian" in PBDB

  brach[which(brach$formation%in%c("Shikhan","Shikhansk")),"early_interval"]<-c("Late Asselian")
  brach[which(brach$formation%in%c("Shikhan","Shikhansk")),"late_interval"]<-c("Late Asselian") #Nassichuk 1995; GTS2020

  brach[which(brach$formation=="Shinatani"),"early_interval"]<-c("Early Toarcian")
  brach[which(brach$formation=="Shinatani"),"late_interval"]<-c("Middle Toarcian") #Nakada et al. 2021. The overlying Otakidani Formation is Late Toarcian in age

  brach[which(brach$formation=="Shionosawa Limestone"),"early_interval"]<-c("Smithian")
  brach[which(brach$formation=="Shionosawa Limestone"),"late_interval"]<-c("Smithian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Shugurovo"),"early_interval"]<-c("Ufimian")
  brach[which(brach$formation=="Shugurovo"),"late_interval"]<-c("Early Roadian") #"Ufimian/Lower Kazanian" in PBDB

  brach[which(brach$formation=="Shuizhutang"),"early_interval"]<-c("Late Wuchiapingian")
  brach[which(brach$formation=="Shuizhutang"),"late_interval"]<-c("Late Wuchiapingian") #Between Wangpanli and Longtan formations

  brach[which(brach$formation=="Shurtan"),"early_interval"]<-c("Early Kungurian")
  brach[which(brach$formation=="Shurtan"),"late_interval"]<-c("Early Kungurian") #"Lower Kungurian" in PBDB

  brach[which(brach$formation=="Skinner Ranch"&brach$early_interval=="Artinskian"),"early_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Skinner Ranch"&brach$late_interval=="Artinskian"),"late_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Skinner Ranch"&brach$early_interval=="Kungurian"),"early_interval"]<-c("Early Kungurian")
  brach[which(brach$formation=="Skinner Ranch"&brach$late_interval=="Kungurian"),"late_interval"]<-c("Early Kungurian") #Menning 2006

  brach[which(brach$formation=="Smolegowa"),"early_interval"]<-c("Bajocian")
  brach[which(brach$formation=="Smolegowa"),"late_interval"]<-c("Bajocian") #Wierzbowski et al. 2004

  brach[which(brach$formation=="Sobral"&brach$early_interval=="Kimmeridgian"),"early_interval"]<-c("Late Kimmeridgian")
  brach[which(brach$formation=="Sobral"&brach$late_interval=="Kimmeridgian"),"late_interval"]<-c("Late Kimmeridgian") #Fürsich et al. 2022

  brach[which(brach$formation=="Sogur"),"early_interval"]<-c("Sakmarian")
  brach[which(brach$formation=="Sogur"),"late_interval"]<-c("Artinskian") #Klets et al. 2000

  brach[which(brach$formation=="Sokolin"),"early_interval"]<-c("Kungurian")
  brach[which(brach$formation=="Sokolin"),"late_interval"]<-c("Kungurian") #Roadian in PBDB. Bundikov et al. 2020

  brach[which(brach$formation=="Soktui"),"early_interval"]<-c("Wordian")
  brach[which(brach$formation=="Soktui"),"late_interval"]<-c("Early Capitanian") #Biakov 2002, 2003

  brach[which(brach$formation=="Solikamsk"),"early_interval"]<-c("Ufimian")
  brach[which(brach$formation=="Solikamsk"),"late_interval"]<-c("Ufimian") #"The layers are assigned to the Solikamsk horizon of the Ufimian"

  brach[which(brach$formation=="Spilsby Sandstone"),"early_interval"]<-c("Middle Volgian")
  brach[which(brach$formation=="Spilsby Sandstone"),"late_interval"]<-c("Middle Volgian") #"Middle Volgian" in PBDB
  brach[which(brach$collection_no==157894),"early_interval"]<-c("Middle Volgian")
  brach[which(brach$collection_no==157894),"late_interval"]<-c("Late Volgian") #"Middle TO upper Volgian" in PBDB

  brach[which(brach$formation=="Staffin Bay"),"early_interval"]<-c("Early Callovian")
  brach[which(brach$formation=="Staffin Bay"),"late_interval"]<-c("Early Callovian") #According to ammonoid zone

  brach[which(brach$collection_no==163707),"early_interval"]<-c("Middle Callovian")
  brach[which(brach$collection_no==163707),"late_interval"]<-c("Middle Callovian")
  brach[which(brach$collection_no==163721),"early_interval"]<-c("Plicatilis")
  brach[which(brach$collection_no==163721),"late_interval"]<-c("Plicatilis") #According to ammonoid zone

  brach[which(brach$formation=="Staratel"&brach$early_interval=="Wuchiapingian"),"early_interval"]<-c("Late Wuchiapingian")
  brach[which(brach$formation=="Staratel"&brach$late_interval=="Wuchiapingian"),"late_interval"]<-c("Late Wuchiapingian")
  brach[which(brach$formation=="Staratel"&brach$early_interval=="Changhsingian"),"early_interval"]<-c("Early Changhsingian")
  brach[which(brach$formation=="Staratel"&brach$late_interval=="Changhsingian"),"late_interval"]<-c("Early Changhsingian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="steinalm Limestones"),"early_interval"]<-c("Anisian")
  brach[which(brach$formation=="steinalm Limestones"),"late_interval"]<-c("Anisian")
  brach[which(brach$formation=="Steinalm Limestones"),"early_interval"]<-c("Anisian")
  brach[which(brach$formation=="Steinalm Limestones"),"late_interval"]<-c("Anisian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Stonesfield Slate"),"early_interval"]<-c("Middle Bathonian")
  brach[which(brach$formation=="Stonesfield Slate"),"late_interval"]<-c("Middle Bathonian") #Barron et al. 2012

  brach[which(brach$formation=="Strimbes Limestone"),"early_interval"]<-c("Sinemurian")
  brach[which(brach$formation=="Strimbes Limestone"),"late_interval"]<-c("Pliensbachian") #"Sinemurian-Pliensbachian" in PBDB

  brach[which(brach$formation=="Stuttgart"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Stuttgart"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Staratel"&brach$early_interval=="Wuchiapingian"),"early_interval"]<-c("Late Wuchiapingian")
  brach[which(brach$formation=="Staratel"&brach$late_interval=="Wuchiapingian"),"late_interval"]<-c("Late Wuchiapingian")

  brach[which(brach$formation=="Sungjar"),"early_interval"]<-c("Olenekian")
  brach[which(brach$formation=="Sungjar"),"late_interval"]<-c("Olenekian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Sullivan Peak"&brach$early_interval=="Kungurian"),"early_interval"]<-c("Early Kungurian")
  brach[which(brach$formation=="Sullivan Peak"&brach$late_interval=="Kungurian"),"late_interval"]<-c("Early Kungurian") #Menning 2006

  brach[which(brach$formation=="Sumra"&brach$early_interval=="Norian"),"early_interval"]<-c("Sevatian")
  brach[which(brach$formation=="Sumra"&brach$late_interval=="Norian"),"late_interval"]<-c("Sevatian") #Following Flannery-Sutherland et al. 2022. The age of this formation is uncertain. Some authors suggest it belongs to Rhaetian

  brach[which(brach$formation=="Sundance"&brach$member=="Stockade Beaver Shale"),"early_interval"]<-c("Late Bathonian")
  brach[which(brach$formation=="Sundance"&brach$member=="Stockade Beaver Shale"),"late_interval"]<-c("Late Bathonian") #Massare et al. 2014

  brach[which(brach$formation=="S茫o Gi茫o"&brach$early_interval=="Levesquei"),"early_interval"]<-c("Pseudoradiosa")
  brach[which(brach$formation=="S茫o Gi茫o"&brach$late_interval=="Levesquei"),"late_interval"]<-c("Pseudoradiosa") #The formation name should be São Gião. Levesquei should be a Late Toarcian zone, but the absolute age is Bajocian in fossilbrush

  brach[which(brach$formation=="Tak Fa"),"early_interval"]<-c("Kungurian")
  brach[which(brach$formation=="Tak Fa"),"late_interval"]<-c("Kungurian") #Xu et al. 2022

  brach[which(brach$formation=="Takakurayama"),"early_interval"]<-c("Wuchiapingian")
  brach[which(brach$formation=="Takakurayama"),"late_interval"]<-c("Wuchiapingian") #Tazawa et al. 2015

  brach[which(brach$formation=="Talpa"),"early_interval"]<-c("Early Kungurian")
  brach[which(brach$formation=="Talpa"),"late_interval"]<-c("Early Kungurian") #Lucas 2018. May be Late Artinskian

  brach[which(brach$formation=="Talung"&brach$ref_author=="Shen et al."),"early_interval"]<-c("Late Changhsingian")
  brach[which(brach$formation=="Talung"&brach$ref_author=="Shen et al."),"late_interval"]<-c("Late Changhsingian") #"The uppermost Changhsingian" in PBDB

  brach[which(brach$stratgroup=="Taungnyo"),"early_interval"]<-c("Late Kungurian")
  brach[which(brach$stratgroup=="Taungnyo"),"late_interval"]<-c("Late Kungurian") #Xu et al. 2022

  brach[which(brach$formation=="Tansill"),"early_interval"]<-c("Late Capitanian")
  brach[which(brach$formation=="Tansill"),"late_interval"]<-c("Late Capitanian") #Nestell and Nestell 2006

  brach[which(brach$formation=="Tartalinskaya"),"early_interval"]<-c("Spathian")
  brach[which(brach$formation=="Tartalinskaya"),"late_interval"]<-c("Spathian") #"Late Olenekian"

  brach[which(brach$formation=="Tastuba"),"early_interval"]<-c("Early Sakmarian")
  brach[which(brach$formation=="Tastuba"),"late_interval"]<-c("Early Sakmarian") #Kotlyar et al. 2018

  brach[which(brach$formation=="Tataouina"),"early_interval"]<-c("Callovian")
  brach[which(brach$formation=="Tataouina"),"late_interval"]<-c("Callovian")
  brach[which(brach$formation=="Tataouine"),"early_interval"]<-c("Callovian")
  brach[which(brach$formation=="Tataouine"),"late_interval"]<-c("Callovian") #Alméras et al., 2005

  brach[which(brach$formation=="Taynton Stone"),"early_interval"]<-c("Middle Bathonian")
  brach[which(brach$formation=="Taynton Stone"),"late_interval"]<-c("Middle Bathonian") #Barron et al. 2012

  brach[which(brach$formation=="Tazigzaout"),"late_interval"]<-c("Early Bathonian") #Ait Addi and Chafiki 2013

  brach[which(brach$formation=="Teradani"),"early_interval"]<-c("Late Pliensbachian")
  brach[which(brach$formation=="Teradani"),"late_interval"]<-c("Late Pliensbachian") #"Late Pliensbachian" in PBDB; see also Nakada et al. 2021

  brach[which(brach$formation=="Thaynes"&brach$early_interval=="Early Triassic"),"early_interval"]<-c("Olenekian")
  brach[which(brach$formation=="Thaynes"&brach$late_interval=="Early Triassic"),"late_interval"]<-c("Olenekian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Theoi Marls"),"early_interval"]<-c("Plicatilis")
  brach[which(brach$formation=="Theoi Marls"),"late_interval"]<-c("Plicatilis") #According to ammonoid zone

  brach[which(brach$formation=="Tiryakh"),"early_interval"]<-c("Early Capitanian")
  brach[which(brach$formation=="Tiryakh"),"late_interval"]<-c("Early Capitanian") #Roadian in PBDB, but these species were reported from the Capitanian according to Davydov et al. 2022

  brach[which(brach$formation=="Titan"&brach$early_interval=="Capitanian"),"early_interval"]<-c("Late Capitanian")
  brach[which(brach$formation=="Titan"&brach$late_interval=="Capitanian"),"late_interval"]<-c("Late Capitanian")
  brach[which(brach$formation=="Titan"&brach$early_interval=="Wuchiapingian"),"early_interval"]<-c("Early Wuchiapingian")
  brach[which(brach$formation=="Titan"&brach$late_interval=="Wuchiapingian"),"late_interval"]<-c("Early Wuchiapingian") #Biakov 2013

  brach[which(brach$formation=="Togotui"),"early_interval"]<-c("Late Capitanian")
  brach[which(brach$formation=="Togotui"),"late_interval"]<-c("Late Capitanian") #Biakov 2013

  brach[which(brach$formation=="Trigonia clavellata"),"early_interval"]<-c("Transversarium")
  brach[which(brach$formation=="Trigonia clavellata"),"late_interval"]<-c("Cautisnigrae") #According to ammonoid zone

  brach[which(brach$formation=="Trochitenkalk"),"early_interval"]<-c("Illyrian")
  brach[which(brach$formation=="Trochitenkalk"),"late_interval"]<-c("Illyrian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Trold Fiord"),"early_interval"]<-c("Wordian")
  brach[which(brach$formation=="Trold Fiord"),"late_interval"]<-c("Capitanian") #The ages are uncertain, may be Wordian-Wuchiapingian (Lee et al. 2019). Capitanian or Wuchiapingian in PBDB, but usually Wordian in systematic papers

  brach[which(brach$formation=="Tschermakfjellet"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Tschermakfjellet"),"late_interval"]<-c("Julian") #The lower member

  brach[which(brach$formation=="Tumul"&brach$early_interval=="Norian"),"early_interval"]<-c("Alaunian")
  brach[which(brach$formation=="Tumul"&brach$late_interval=="Norian"),"late_interval"]<-c("Alaunian") #According to bivalve zone

  brach[which(brach$formation=="Tunlonggongba"),"early_interval"]<-c("Kungurian")
  brach[which(brach$formation=="Tunlonggongba"),"late_interval"]<-c("Kungurian") #Following Shen et al. 2021; Xu et al. 2022

  brach[which(brach$formation=="Turmiel"&brach$early_interval=="Levesquei"),"early_interval"]<-c("Pseudoradiosa")
  brach[which(brach$formation=="Turmiel"&brach$late_interval=="Levesquei"),"late_interval"]<-c("Pseudoradiosa")
  brach[which(brach$formation=="Turmiel"&brach$early_interval=="Late Toarcian"),"early_interval"]<-c("Thouarsense")
  brach[which(brach$formation=="Turmiel"&brach$late_interval=="Late Toarcian"),"late_interval"]<-c("Thouarsense") #Fallaciosum subzone
  brach[which(brach$collection_no%in%c(211066:211068,211168:211177)),"early_interval"]<-c("Tenuicostatum")
  brach[which(brach$collection_no%in%c(211066:211068,211168:211177)),"late_interval"]<-c("Tenuicostatum")
  brach[which(brach$collection_no%in%c(211178:211180,211184:211211)),"early_interval"]<-c("Serpentinum")
  brach[which(brach$collection_no%in%c(211178:211180,211184:211211)),"late_interval"]<-c("Serpentinum")
  brach[which(brach$collection_no%in%c(211212:211216)),"early_interval"]<-c("Bifrons")
  brach[which(brach$collection_no%in%c(211212:211216)),"late_interval"]<-c("Bifrons")
  brach[which(brach$collection_no==51613),"early_interval"]<-c("Serpentinum")
  brach[which(brach$collection_no==51613),"late_interval"]<-c("Bifrons") #According to ammonoid zone

  brach[which(brach$formation=="Turuzov"&brach$early_interval=="Asselian"),"early_interval"]<-c("Kasimovian")
  brach[which(brach$formation=="Turuzov"&brach$late_interval=="Asselian"),"late_interval"]<-c("Kasimovian")
  brach[which(brach$formation=="Turuzov"&brach$early_interval=="Sakmarian"),"early_interval"]<-c("Gzhelian")
  brach[which(brach$formation=="Turuzov"&brach$late_interval=="Sakmarian"),"late_interval"]<-c("Gzhelian") #Asselian-Sakmarian in PBDB, but according to Budnikov et al. 2020, this formation should be Kasimovian-Gzhelian

  brach[which(brach$formation%in%c("Tuwaiq","Tuwaiq Mountain","Tuwaiq Mt.Lst.")&brach$early_interval=="Callovian"),"early_interval"]<-c("Middle Callovian")
  brach[which(brach$formation%in%c("Tuwaiq","Tuwaiq Mountain","Tuwaiq Mt.Lst.")&brach$late_interval=="Callovian"),"late_interval"]<-c("Late Callovian") #Alméras et al. 2010

  brach[which(brach$formation=="Tyaughton"&brach$early_interval=="Rhaetian"),"early_interval"]<-c("Early Rhaetian")
  brach[which(brach$formation=="Tyaughton"&brach$late_interval=="Rhaetian"),"late_interval"]<-c("Early Rhaetian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation%in%c("Ufimskiy","Ufim")),"early_interval"]<-c("Ufimian")
  brach[which(brach$formation%in%c("Ufimskiy","Ufim")),"late_interval"]<-c("Ufimian") #May be Ufimian

  brach[which(brach$collection_no%in%c(124544:124546)),"early_interval"]<-c("Aegean")
  brach[which(brach$collection_no%in%c(124544:124546)),"late_interval"]<-c("Aegean")
  brach[which(brach$collection_no%in%c(124547:124550,124574)),"early_interval"]<-c("Bithynian")
  brach[which(brach$collection_no%in%c(124547:124550,124574)),"late_interval"]<-c("Pelsonian")
  brach[which(brach$collection_no%in%c(124551:124553,124640:124652)),"early_interval"]<-c("Illyrian")
  brach[which(brach$collection_no%in%c(124551:124553,124640:124652)),"late_interval"]<-c("Illyrian")
  brach[which(brach$collection_no%in%c(124554:124556,124653:124655)),"early_interval"]<-c("Fassanian")
  brach[which(brach$collection_no%in%c(124554:124556,124653:124655)),"late_interval"]<-c("Fassanian")
  brach[which(brach$collection_no%in%c(124558:124564,124656:124661,124663:124669)),"early_interval"]<-c("Longobardian")
  brach[which(brach$collection_no%in%c(124558:124564,124656:124661,124663:124669)),"late_interval"]<-c("Longobardian")
  brach[which(brach$collection_no%in%c(124670:124675)),"early_interval"]<-c("Julian")
  brach[which(brach$collection_no%in%c(124670:124675)),"late_interval"]<-c("Julian") #According to the original reference

  brach[which(brach$formation=="Upper Calcareous Grit"),"early_interval"]<-c("Pumilus")
  brach[which(brach$formation=="Upper Calcareous Grit"),"late_interval"]<-c("Cautisnigrae") #According to ammonoid zone

  brach[which(brach$formation=="Upper Trigonia Grit"),"early_interval"]<-c("Late Bajocian")
  brach[which(brach$formation=="Upper Trigonia Grit"),"late_interval"]<-c("Late Bajocian") #Barron et al. 2012

  brach[which(brach$formation=="Urushten"),"early_interval"]<-c("Late Changhsingian")
  brach[which(brach$formation=="Urushten"),"late_interval"]<-c("Late Changhsingian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Ushimaru"),"early_interval"]<-c("Tithonian")
  brach[which(brach$formation=="Ushimaru"),"late_interval"]<-c("Tithonian") #Haggart and Matsukawa 2020

  brach[which(brach$formation=="Uzen"),"early_interval"]<-c("Spathian")
  brach[which(brach$formation=="Uzen"),"late_interval"]<-c("Spathian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Valtos Sandstone"),"early_interval"]<-c("Subcontractus")
  brach[which(brach$formation=="Valtos Sandstone"),"late_interval"]<-c("Subcontractus") #Barron et al. 2012

  brach[which(brach$formation=="van Hauen"),"early_interval"]<-c("Wordian")
  brach[which(brach$formation=="van Hauen"),"late_interval"]<-c("Capitanian") #Lee et al. 2019

  brach[which(brach$formation=="Vardekloft"),"early_interval"]<-c("Early Bathonian")
  brach[which(brach$formation=="Vardekloft"),"late_interval"]<-c("Early Bathonian") #According to ammonoid zone

  brach[which(brach$formation=="Vellerat"),"early_interval"]<-c("Late Oxfordian")
  brach[which(brach$formation=="Vellerat"),"late_interval"]<-c("Late Oxfordian") #Thuy et al. 2013

  brach[which(brach$formation=="Verkhnii Uslon"),"early_interval"]<-c("Late Roadian")
  brach[which(brach$formation=="Verkhnii Uslon"),"late_interval"]<-c("Late Roadian") #Upper Kazanian

  brach[which(brach$formation=="Vester"&brach$early_interval=="Carnian"),"early_interval"]<-c("Tuvalian")
  brach[which(brach$formation=="Vester"&brach$late_interval=="Carnian"),"late_interval"]<-c("Tuvalian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Veszpr茅m Marl"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Veszpr茅m Marl"),"late_interval"]<-c("Julian") #The formation name should be Veszprém Marl. Following Flannery-Sutherland et al. 2022

  brach[which(brach$collection_no==128375),"early_interval"]<-c("Late Pliensbachian")
  brach[which(brach$collection_no==128375),"late_interval"]<-c("Late Pliensbachian") #Domerian Substage

  brach[which(brach$collection_no==127112),"early_interval"]<-c("Gracilis")
  brach[which(brach$collection_no==127112),"late_interval"]<-c("Athleta") #According to ammonoid zone

  brach[which(brach$formation=="Vitiacua"),"early_interval"]<-c("Permian")
  brach[which(brach$formation=="Vitiacua"),"late_interval"]<-c("Permian") #The age of this formation is uncertain. It will be discarded

  brach[which(brach$formation=="Waluba"),"early_interval"]<-c("Alaunian")
  brach[which(brach$formation=="Waluba"),"late_interval"]<-c("Sevatian") #Tong et al. 2021

  brach[which(brach$formation=="Wangpanli"),"early_interval"]<-c("Early Changhsingian")
  brach[which(brach$formation=="Wangpanli"),"late_interval"]<-c("Early Changhsingian") #Shen et al. 2017

  brach[which(brach$formation=="Wargal"&brach$member=="Kalabagh"),"early_interval"]<-c("Late Wuchiapingian")
  brach[which(brach$formation=="Wargal"&brach$member=="Kalabagh"),"late_interval"]<-c("Late Wuchiapingian") #Waterhouse 2010

  brach[which(brach$formation=="Weishan"),"early_interval"]<-c("Lacian")
  brach[which(brach$formation=="Weishan"),"late_interval"]<-c("Lacian") #Tiechang Member, Sanhedong Formation

  brach[which(brach$formation=="Weiyuanjiang"),"early_interval"]<-c("Carnian")
  brach[which(brach$formation=="Weiyuanjiang"),"late_interval"]<-c("Carnian") #Tong et al. 2021

  brach[which(brach$formation=="Wellenkalk"&brach$member=="Gogoliner Schichten"),"early_interval"]<-c("Aegean")
  brach[which(brach$formation=="Wellenkalk"&brach$member=="Gogoliner Schichten"),"late_interval"]<-c("Aegean") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation%in%c("Wengen","Wengener Schichten")),"early_interval"]<-c("Longobardian")
  brach[which(brach$formation%in%c("Wengen","Wengener Schichten")),"late_interval"]<-c("Longobardian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Werfen"&brach$early_interval=="Changhsingian"),"early_interval"]<-c("Late Changhsingian")
  brach[which(brach$formation=="Werfen"&brach$late_interval=="Changhsingian"),"late_interval"]<-c("Late Changhsingian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Wetterstein"&brach$early_interval=="Anisian"),"early_interval"]<-c("Illyrian")
  brach[which(brach$formation=="Wetterstein"&brach$late_interval=="Anisian"),"late_interval"]<-c("Illyrian") #According to ammonoid zone
  brach[which(brach$formation=="Wetterstein Limestone"&brach$early_interval=="Carnian"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Wetterstein Limestone"&brach$late_interval=="Carnian"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022
  brach[which(brach$collection_no%in%c(202276:202280)),"early_interval"]<-c("Pelsonian")
  brach[which(brach$collection_no%in%c(202276:202280)),"late_interval"]<-c("Illyrian") #The brachiopods and ammonoids suggest late Anisian

  brach[which(brach$formation=="Whitby Mudstone"&brach$member=="Alum Shale"),"early_interval"]<-c("Bifrons")
  brach[which(brach$formation=="Whitby Mudstone"&brach$member=="Alum Shale"),"late_interval"]<-c("Bifrons")
  brach[which(brach$formation=="Whitby Mudstone"&brach$member=="Grey Shale"),"early_interval"]<-c("Tenuicostatum")
  brach[which(brach$formation=="Whitby Mudstone"&brach$member=="Grey Shale"),"late_interval"]<-c("Tenuicostatum")
  brach[which(brach$formation=="Whitby Mudstone"&brach$member=="Mulgrave Shale"),"early_interval"]<-c("Falciferum")
  brach[which(brach$formation=="Whitby Mudstone"&brach$member=="Mulgrave Shale"),"late_interval"]<-c("Falciferum")
  brach[which(brach$collection_no==219475),"early_interval"]<-c("Tenuicostatum")
  brach[which(brach$collection_no==219475),"late_interval"]<-c("Tenuicostatum")
  brach[which(brach$collection_no==219476),"early_interval"]<-c("Falciferum")
  brach[which(brach$collection_no==219476),"late_interval"]<-c("Falciferum") #According to ammonoid zone

  brach[which(brach$formation=="White Lias"&brach$early_interval=="Rhaetian"),"early_interval"]<-c("Late Rhaetian")
  brach[which(brach$formation=="White Lias"&brach$late_interval=="Rhaetian"),"late_interval"]<-c("Late Rhaetian")
  brach[which(brach$formation=="White Lias"&brach$early_interval=="Hettangian"),"early_interval"]<-c("Tilmani/spelae")
  brach[which(brach$formation=="White Lias"&brach$late_interval=="Hettangian"),"late_interval"]<-c("Tilmani/spelae") #Near T/J boundry

  brach[which(brach$formation=="White Limestone"&brach$early_interval=="Bathonian"),"early_interval"]<-c("Subcontractus")
  brach[which(brach$formation=="White Limestone"&brach$late_interval=="Bathonian"),"late_interval"]<-c("Retrocostatum") #Barron et al. 2012

  brach[which(brach$formation=="Whitehorse"),"early_interval"]<-c("Wordian")
  brach[which(brach$formation=="Whitehorse"),"late_interval"]<-c("Early Capitanian") #Capitanian in PBDB, mostly Wordian in Foster et al. 2014

  brach[which(brach$formation=="Winnemucca"),"early_interval"]<-c("Lacian")
  brach[which(brach$formation=="Winnemucca"),"late_interval"]<-c("Lacian") #"The species is known only from the early Norian"

  brach[which(brach$formation=="Wordie Creek"&brach$early_interval=="Early Triassic"),"early_interval"]<-c("Induan")
  brach[which(brach$formation=="Wordie Creek"&brach$late_interval=="Early Triassic"),"late_interval"]<-c("Induan") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Wutankule"),"early_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Wutankule"),"late_interval"]<-c("Late Artinskian") #Chen et al. 2003

  brach[which(brach$formation=="Xiaowa"),"early_interval"]<-c("Julian")
  brach[which(brach$formation=="Xiaowa"),"late_interval"]<-c("Julian") #Most species are from the lower part. Zhao et al. 2021

  brach[which(brach$formation=="Xintianmen"&brach$early_interval=="Hettangian"),"early_interval"]<-c("Late Hettangian")
  brach[which(brach$formation=="Xintianmen"&brach$late_interval=="Hettangian"),"late_interval"]<-c("Late Hettangian")
  brach[which(brach$formation=="Xintianmen"&brach$early_interval=="Sinemurian"),"early_interval"]<-c("Early Sinemurian")
  brach[which(brach$formation=="Xintianmen"&brach$late_interval=="Sinemurian"),"late_interval"]<-c("Early Sinemurian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Yakoun"),"early_interval"]<-c("Bajocian")
  brach[which(brach$formation=="Yakoun"),"late_interval"]<-c("Bajocian") #Kottachchi et al. 2002

  brach[which(brach$formation=="Yanjar"),"early_interval"]<-c("Olenekian")
  brach[which(brach$formation=="Yanjar"),"late_interval"]<-c("Olenekian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Yamamba Limestone"),"early_interval"]<-c("Capitanian")
  brach[which(brach$formation=="Yamamba Limestone"),"late_interval"]<-c("Capitanian") #Shen and Shi 2004

  brach[which(brach$formation=="Yenduyet"),"early_interval"]<-c("Early Changhsingian")
  brach[which(brach$formation=="Yenduyet"),"late_interval"]<-c("Early Changhsingian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Yinkeng"&brach$early_interval=="Changhsingian"),"early_interval"]<-c("Late Changhsingian")
  brach[which(brach$formation=="Yinkeng"&brach$late_interval=="Changhsingian"),"late_interval"]<-c("Late Changhsingian") #Near P/T boundry

  brach[which(brach$formation=="Ystanakh"),"early_interval"]<-c("Spathian")
  brach[which(brach$formation=="Ystanakh"),"late_interval"]<-c("Spathian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Yundoutan"),"early_interval"]<-c("Late Wuchiapingian")
  brach[which(brach$formation=="Yundoutan"),"late_interval"]<-c("Late Wuchiapingian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Zalgiris"),"early_interval"]<-c("Late Wuchiapingian")
  brach[which(brach$formation=="Zalgiris"),"late_interval"]<-c("Late Wuchiapingian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Zechstein"&brach$member=="Upper Werra Anhydrite"),"early_interval"]<-c("Early Wuchiapingian")
  brach[which(brach$formation=="Zechstein"&brach$member=="Upper Werra Anhydrite"),"late_interval"]<-c("Early Wuchiapingian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Zewan"&brach$member=="A"),"early_interval"]<-c("Early Wuchiapingian")
  brach[which(brach$formation=="Zewan"&brach$member=="A"),"late_interval"]<-c("Early Wuchiapingian")
  brach[which(brach$formation=="Zewan"&brach$member=="C"),"early_interval"]<-c("Late Wuchiapingian")
  brach[which(brach$formation=="Zewan"&brach$member=="C"),"late_interval"]<-c("Late Wuchiapingian")
  brach[which(brach$formation=="Zewan"&brach$member=="D"),"early_interval"]<-c("Early Changhsingian")
  brach[which(brach$formation=="Zewan"&brach$member=="D"),"late_interval"]<-c("Early Changhsingian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Zhuganpo"),"early_interval"]<-c("Longobardian")
  brach[which(brach$formation=="Zhuganpo"),"late_interval"]<-c("Julian") #Benton et al. 2013

  brach[which(brach$formation=="Zlambach"),"early_interval"]<-c("Rhaetian")
  brach[which(brach$formation=="Zlambach"),"late_interval"]<-c("Rhaetian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$formation=="Zor Hauran"),"early_interval"]<-c("Rhaetian")
  brach[which(brach$formation=="Zor Hauran"),"late_interval"]<-c("Rhaetian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$ref_author=="Guo et al."&brach$ref_pubyr=="2022"&brach$state=="Qinghai"),"early_interval"]<-c("Bithynian")
  brach[which(brach$ref_author=="Guo et al."&brach$ref_pubyr=="2022"&brach$state=="Qinghai"),"late_interval"]<-c("Illyrian") # Guo et al. 2022

  brach[which(brach$genus=="Sinucostella"),"early_interval"]<-c("Aegean")
  brach[which(brach$genus=="Sinucostella"),"late_interval"]<-c("Pelsonian") # Guo et al. 2022

  brach[which(brach$member=="Carditaschichten"),"early_interval"]<-c("Julian")
  brach[which(brach$member=="Carditaschichten"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$member=="Nerinea Bed"),"early_interval"]<-c("Kimmeridgian")
  brach[which(brach$member=="Nerinea Bed"),"late_interval"]<-c("Kimmeridgian") #Kimmeridgian-Tithonian in PBDB. Schrank 2010

  brach[which(brach$member=="Middle Saurian Bed"),"early_interval"]<-c("Late Kimmeridgian")
  brach[which(brach$member=="Middle Saurian Bed"),"late_interval"]<-c("Late Kimmeridgian") #Schrank 2010

  brach[which(brach$member=="Trigonia Smeei Bed"),"early_interval"]<-c("Early Tithonian")
  brach[which(brach$member=="Trigonia Smeei Bed"),"late_interval"]<-c("Early Tithonian")
  brach[which(brach$member=="Trigonia smeei Bed"),"early_interval"]<-c("Early Tithonian")
  brach[which(brach$member=="Trigonia smeei Bed"),"late_interval"]<-c("Early Tithonian") #Aberhan 2002; Schrank 2010

  brach[which(brach$member=="Napeng"),"early_interval"]<-c("Rhaetian")
  brach[which(brach$member=="Napeng"),"late_interval"]<-c("Rhaetian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$member=="Halobia Limestone"&brach$early_interval=="Norian"),"early_interval"]<-c("Lacian")
  brach[which(brach$member=="Halobia Limestone"&brach$late_interval=="Norian"),"late_interval"]<-c("Lacian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$collection_no==85014),"early_interval"]<-c("Otamitan")
  brach[which(brach$collection_no==85014),"late_interval"]<-c("Otamitan") #"Otamitan stage equivalent" in PBDB

  brach[which(brach$stratgroup=="Yangshiping"&brach$early_interval=="Oxfordian"),"early_interval"]<-c("Callovian")
  brach[which(brach$stratgroup=="Yangshiping"&brach$late_interval=="Oxfordian"),"late_interval"]<-c("Callovian") #Sun et al. 2017

  brach[which(brach$collection_no==155211),"early_interval"]<-c("Aalensis")
  brach[which(brach$collection_no==155211),"late_interval"]<-c("Aalensis") #According to ammonoid zone

  brach[which(brach$collection_no==38487),"early_interval"]<-c("Middle Callovian")
  brach[which(brach$collection_no==38487),"late_interval"]<-c("Middle Callovian") #Barron et al. 2012

  brach[which(brach$stratgroup=="Yakuno"&brach$early_interval=="Induan"),"early_interval"]<-c("Olenekian") #Following Flannery-Sutherland et al. 2022

  brach[which(brach$stratgroup=="Fukumoto"&brach$early_interval=="Induan"),"early_interval"]<-c("Olenekian") #The Kusano Formation is Olenekian, and the Induan-Olenekian records may be Olenekian as well

  brach[which(brach$collection_no==61741),"early_interval"]<-c("Longobardian")
  brach[which(brach$collection_no==61741),"late_interval"]<-c("Julian") #According to the original reference

  brach[which(brach$stratgroup=="Yakoun"),"early_interval"]<-c("Bajocian")
  brach[which(brach$stratgroup=="Yakoun"),"late_interval"]<-c("Bajocian") #Kottachchi et al. 2002





  #####In addition to the records mentioned above, these records without the formation name are also revised

  brach[which(brach$early_interval=="Levesquei"),"early_interval"]<-c("Pseudoradiosa")
  brach[which(brach$late_interval=="Levesquei"),"late_interval"]<-c("Pseudoradiosa") #Levesquei should be a Late Toarcian zone, but the absolute age is Bajocian in fossilbrush

  brach[which(brach$early_interval=="Norian"&brach$late_interval=="Rhaetian"&brach$ref_author=="Dagys"),"early_interval"]<-c("Rhaetian") #See comments in PBDB collection 128039: Listed as "Norian-Rhaetian" but only because there was debate at the time whether the Rhaetian was a separate stage. These "Norian-Rhaetian" taxa were also regarded as Rhaetian ones by Ruban (2010)



  ######These revisions are about the Triassic and Jurassic collections from New Zealand
  #The New Zealand stages were transfered to the international stages, because the absolute ages of the international ages have been revised according to GTS2020
  #However, the absolute ages of the New Zealand stages were based on faunal correlation

  #Triassic:
  brach[which(brach$cc=="NZ"&brach$early_interval=="Kaihikuan"),"early_interval"]<-c("Carnian")
  brach[which(brach$cc=="NZ"&brach$late_interval=="Kaihikuan"),"late_interval"]<-c("Carnian")
  brach[which(brach$cc=="NZ"&brach$early_interval=="Oretian"),"early_interval"]<-c("Lacian")
  brach[which(brach$cc=="NZ"&brach$late_interval=="Oretian"),"late_interval"]<-c("Lacian")
  brach[which(brach$cc=="NZ"&brach$early_interval=="Otamitan"),"early_interval"]<-c("Alaunian")
  brach[which(brach$cc=="NZ"&brach$late_interval=="Otamitan"),"late_interval"]<-c("Alaunian")
  brach[which(brach$cc=="NZ"&brach$early_interval=="Warepan"),"early_interval"]<-c("Sevatian")
  brach[which(brach$cc=="NZ"&brach$late_interval=="Warepan"),"late_interval"]<-c("Sevatian")
  brach[which(brach$cc=="NZ"&brach$early_interval=="Otapiran"),"early_interval"]<-c("Rhaetian")
  brach[which(brach$cc=="NZ"&brach$late_interval=="Otapiran"),"late_interval"]<-c("Rhaetian") #See Raine et al. 2015

  brach[which(brach$collection_no%in%c(44467,44472,44473,44484:44490)),"early_interval"]<-c("Lacian")
  brach[which(brach$collection_no%in%c(44467,44472,44473,44484:44490)),"late_interval"]<-c("Lacian") #Carnian-Norian in PBDB. But the presence of Halobia austriaca and other Halobia species suggest an Early Norian age according to Campbell 2019

  brach[which(brach$collection_no==101407),"early_interval"]<-c("Julian")
  brach[which(brach$collection_no==101407),"late_interval"]<-c("Julian") #"Lower Carnian" in PBDB

  brach[which(brach$collection_no==201498),"early_interval"]<-c("Alaunian")
  brach[which(brach$collection_no==201498),"late_interval"]<-c("Sevatian") #"Otamitan-Warepan" in PBDB

  brach[which(brach$collection_no%in%c(201930,201932,201934,201935,201937,201939,201943)),"early_interval"]<-c("Norian")
  brach[which(brach$collection_no%in%c(201930,201932,201934,201935,201937,201939,201943)),"late_interval"]<-c("Norian") #Carnian in PBDB, but the presence of Halobia possibly indicates Norian

  brach[which(brach$collection_no%in%c(50759)),"early_interval"]<-c("Early Rhaetian")
  brach[which(brach$collection_no%in%c(50759)),"late_interval"]<-c("Early Rhaetian") #"Early Otapirian"

  brach[which(brach$collection_no%in%c(50760)),"early_interval"]<-c("Late Rhaetian")
  brach[which(brach$collection_no%in%c(50760)),"late_interval"]<-c("Late Rhaetian") #"Late Otapirian"

  brach[which(brach$collection_no%in%c(203926)),"early_interval"]<-c("Sevatian")
  brach[which(brach$collection_no%in%c(203926)),"late_interval"]<-c("Sevatian") #"Warepan"

  brach[which(brach$collection_no%in%c(203932)),"early_interval"]<-c("Alaunian")
  brach[which(brach$collection_no%in%c(203932)),"late_interval"]<-c("Sevatian") #"Oretian-Warepan"

  brach[which(brach$collection_no%in%c(134423,134432,134433,134459,134465,134471,134473,134476,134479,134483,134486,134487,134496,134497,134524:134593,134595:134598,134625
                                       ,134628:134633,134771,134784:134786,134789,134790,134792:134799,134815,134817,134822:134824,134827:134834,134839,134841,134852:134859
  									 ,134870:134873,134880,134881,134883:134887,134889:134892,134896,134907,134910,134921,134922,134960,134961,134963,134971:134976,134979
  									 ,134984:134986,134989:134994,134996:134999,135006,135020,135021,135085,135137:135139,135141,135144:135150,135156,135165:135167,135170
  									 ,135171,135191:135193,135201:135206,135208:135210,135213:135216,135218:135221,135233,135234,135239,135241,135244,135246:135248,135250
  									 ,135251,135252,135254,135255,135260:135265,135275,135277,135280,135283,135284,135285,135288:135292,135295:135300,135307:135311,137175:137178
  									 ,137182,137183,137868:137871,197505:197508,201925,201926,201945,44459:44466,44468:44471,44478,60997,134429,134430,134472,134800)),"early_interval"]<-c("Carnian")
  brach[which(brach$collection_no%in%c(134423,134432,134433,134459,134465,134471,134473,134476,134479,134483,134486,134487,134496,134497,134524:134593,134595:134598,134625
                                       ,134628:134633,134771,134784:134786,134789,134790,134792:134799,134815,134817,134822:134824,134827:134834,134839,134841,134852:134859
  									 ,134870:134873,134880,134881,134883:134887,134889:134892,134896,134907,134910,134921,134922,134960,134961,134963,134971:134976,134979
  									 ,134984:134986,134989:134994,134996:134999,135006,135020,135021,135085,135137:135139,135141,135144:135150,135156,135165:135167,135170
  									 ,135171,135191:135193,135201:135206,135208:135210,135213:135216,135218:135221,135233,135234,135239,135241,135244,135246:135248,135250
  									 ,135251,135252,135254,135255,135260:135265,135275,135277,135280,135283,135284,135285,135288:135292,135295:135300,135307:135311,137175:137178
  									 ,137182,137183,137868:137871,197505:197508,201925,201926,201945,44459:44466,44468:44471,44478,60997,134429,134430,134472,134800)),"late_interval"]<-c("Carnian") #Most of these collections are Ladinian in PBDB. These collections are Kaihikuan in age.
  #These taxa were regarded as Ladinian ones by Treatise and these records were re-assigned to the Longobardian by Flannery-Sutherland et al. 2022.
  #Mundil et al. (2010) report a provisional CA-TIMS Pb-U zircon age of 237 Ma for a tuff within upper Etalian strata (See Raine et al. 2015).
  #This age is consistent with the age of the Ladinian/Carnian boundry. Although the Kaihikuan may extend to the uppermost Ladinian, here it is treated to be equivalent to Carnian

  brach[which(brach$collection_no%in%c(134434:134440,134482,134484,134485,134488,134489,134490,134494,134626,134778,134782,134783,134791,134818,134820,134821,134825,134826
                                       ,134845,134848,134869,134882,134926,134927,134964,134965,134968:134970,134995,135110,135140,135194:135197,135235,135238,135259,137495
  									 ,137809:137817,44453:44455,44457,44458,44474,44476,44477,44479,44480:44482,60990)),"early_interval"]<-c("Ladinian")
  brach[which(brach$collection_no%in%c(134434:134440,134482,134484,134485,134488,134489,134490,134494,134626,134778,134782,134783,134791,134818,134820,134821,134825,134826
                                       ,134845,134848,134869,134882,134926,134927,134964,134965,134968:134970,134995,135110,135140,135194:135197,135235,135238,135259,137495
  									 ,137809:137817,44453:44455,44457,44458,44474,44476,44477,44479,44480:44482,60990)),"late_interval"]<-c("Ladinian") #Late Etalian according to the brachiopod and bivalve range

  brach[which(brach$collection_no%in%c(134620,134627,134781,134843,134851,134928,135119,135198,135304)),"early_interval"]<-c("Anisian")
  brach[which(brach$collection_no%in%c(134620,134627,134781,134843,134851,134928,135119,135198,135304)),"late_interval"]<-c("Anisian") #Early Etalian according to the brachiopod range

  brach[which(brach$collection_no%in%c(134780,134787,134788,134819,134846,134849,134850,134888,134893:134895,134905,134912:134914,134918:134920,44483)),"early_interval"]<-c("Pelsonian")
  brach[which(brach$collection_no%in%c(134780,134787,134788,134819,134846,134849,134850,134888,134893:134895,134905,134912:134914,134918:134920,44483)),"late_interval"]<-c("Fassanian") #Middle Etalian according to the brachiopod range

  brach[which(brach$collection_no%in%c(134877,134878,134879,134897,134899:134903,134911,134915:134917)),"early_interval"]<-c("Spathian")
  brach[which(brach$collection_no%in%c(134877,134878,134879,134897,134899:134903,134911,134915:134917)),"late_interval"]<-c("Spathian") #"Malakovian"

  brach[which(brach$collection_no%in%c(41666:41678,41680,41682,41684,41685,41688)),"early_interval"]<-c("Rhaetian")
  brach[which(brach$collection_no%in%c(41666:41678,41680,41682,41684,41685,41688)),"late_interval"]<-c("Rhaetian") #"Warepan" in the original reference

  #Jurassic:
  brach[which(brach$cc=="NZ"&brach$early_interval=="Puaroan"),"early_interval"]<-c("Semiforme")
  brach[which(brach$cc=="NZ"&brach$late_interval=="Puaroan"),"late_interval"]<-c("Late Tithonian")
  brach[which(brach$cc=="NZ"&brach$early_interval=="Ohauan"),"early_interval"]<-c("Late Kimmeridgian")
  brach[which(brach$cc=="NZ"&brach$late_interval=="Ohauan"),"late_interval"]<-c("Darwini")
  brach[which(brach$cc=="NZ"&brach$early_interval=="Heterian"),"early_interval"]<-c("Coronatum")
  brach[which(brach$cc=="NZ"&brach$late_interval=="Heterian"),"late_interval"]<-c("Early Kimmeridgian")
  brach[which(brach$cc=="NZ"&brach$early_interval=="Temaikan"),"early_interval"]<-c("Late Toarcian")
  brach[which(brach$cc=="NZ"&brach$late_interval=="Temaikan"),"late_interval"]<-c("Anceps")
  brach[which(brach$cc=="NZ"&brach$early_interval=="Ururoan"),"early_interval"]<-c("Ibex")
  brach[which(brach$cc=="NZ"&brach$late_interval=="Ururoan"),"late_interval"]<-c("Variabilis")
  brach[which(brach$cc=="NZ"&brach$early_interval=="Aratauran"),"early_interval"]<-c("Hettangian")
  brach[which(brach$cc=="NZ"&brach$late_interval=="Aratauran"),"late_interval"]<-c("Jamesoni") #According to Raine et al. 2015

  brach[which(brach$collection_no%in%c(63755,63859)),"early_interval"]<-c("Semiforme")
  brach[which(brach$collection_no%in%c(63755,63859)),"late_interval"]<-c("Pallasioides") #"Mangaoran"

  brach[which(brach$collection_no%in%c(182779,63741,63765,63766,63758)),"early_interval"]<-c("Rotunda")
  brach[which(brach$collection_no%in%c(182779,63741,63765,63766,63758)),"late_interval"]<-c("Late Tithonian") #"Upper Puaroan or Waikatoan". "An upper Middle Tithonian (upper Fallauxi Zone) correlation is adopted for the base of the Waikatoan". So Rotunda is selected to represent the "upper Fallauxi Zone"

  brach[which(brach$collection_no%in%c(134137:134141)),"early_interval"]<-c("Hybonotum")
  brach[which(brach$collection_no%in%c(134137:134141)),"late_interval"]<-c("Darwini") #"Late Ohauan"

  brach[which(brach$collection_no%in%c(182791,38962,63869)),"early_interval"]<-c("Semiforme")
  brach[which(brach$collection_no%in%c(182791,38962,63869)),"late_interval"]<-c("Late Tithonian") #"Puaroan"

  brach[which(brach$collection_no%in%c(63829,63836,63837,137847)),"early_interval"]<-c("Late Kimmeridgian")
  brach[which(brach$collection_no%in%c(63829,63836,63837,137847)),"late_interval"]<-c("Darwini") #"Ohauan"

  brach[which(brach$collection_no%in%c(134142,134143,63742,63856:63858,63866,63875,63876,63882)),"early_interval"]<-c("Late Kimmeridgian")
  brach[which(brach$collection_no%in%c(134142,134143,63742,63856:63858,63866,63875,63876,63882)),"late_interval"]<-c("Late Kimmeridgian") #"Early Ohauan"

  brach[which(brach$collection_no%in%c(134010,51934,56229,56230,63745:63747,63749,63751,63880,63881)),"early_interval"]<-c("Coronatum")
  brach[which(brach$collection_no%in%c(134010,51934,56229,56230,63745:63747,63749,63751,63880,63881)),"late_interval"]<-c("Cautisnigrae") #"Early Heterian"

  brach[which(brach$collection_no%in%c(133865,134009,35723,56289,56292,56296,133848,133855,133856,133858:133863,133956:133963,133971,134086:134090,134101:134104,134110,134127,134135,134160,134162,182717
                                       ,182720,182728:182737,182739:182741,182757:182760,182773,182775,182776,182784,182785,56231,56290,63763,63764)),"early_interval"]<-c("Pseudocordata")
  brach[which(brach$collection_no%in%c(133865,134009,35723,56289,56292,56296,133848,133855,133856,133858:133863,133956:133963,133971,134086:134090,134101:134104,134110,134127,134135,134160,134162,182717
                                       ,182720,182728:182737,182739:182741,182757:182760,182773,182775,182776,182784,182785,56231,56290,63763,63764)),"late_interval"]<-c("Pseudocordata") #"Middle Heterian"

  brach[which(brach$collection_no%in%c(133910,133972,134091,134167,182718,182719,182738)),"early_interval"]<-c("Pseudocordata")
  brach[which(brach$collection_no%in%c(133910,133972,134091,134167,182718,182719,182738)),"late_interval"]<-c("Early Kimmeridgian") #"Middle-late Heterian"

  brach[which(brach$collection_no%in%c(133973,133974,63855)),"early_interval"]<-c("Early Kimmeridgian")
  brach[which(brach$collection_no%in%c(133973,133974,63855)),"late_interval"]<-c("Early Kimmeridgian") #"Late Heterian"

  brach[which(brach$collection_no%in%c(133914,133925)),"early_interval"]<-c("Early Kimmeridgian")
  brach[which(brach$collection_no%in%c(133914,133925)),"late_interval"]<-c("Late Kimmeridgian") #"Late Heterian-early Ohauan". The base of the Middle Ohauan is correlated within the upper Kimmeridgian (within the last Kimmeridgian ammonoid zone, beckeri zone?)

  brach[which(brach$member=="Captain Kings Shellbed"),"early_interval"]<-c("Pseudocordata")
  brach[which(brach$member=="Captain Kings Shellbed"),"late_interval"]<-c("Pseudocordata") #This member is Middle Heterian

  brach[which(brach$collection_no%in%c(38961)),"early_interval"]<-c("Coronatum")
  brach[which(brach$collection_no%in%c(38961)),"late_interval"]<-c("Early Kimmeridgian") #"Heterian"

  brach[which(brach$collection_no%in%c(133850,133899,133936,133937,133866,133867:133880)),"early_interval"]<-c("Middle Bathonian")
  brach[which(brach$collection_no%in%c(133850,133899,133936,133937,133866,133867:133880)),"late_interval"]<-c("Anceps") #Late late Temaikan, treated as the middle Bathonian to Callovian part of late Temaikan

  brach[which(brach$collection_no%in%c(133921,133923,133924,56241,56245,56249,56250,56241,56283,56285,56241,56273,56241,56285,134288,134403,182714:182716,182722:182725,182726,182755,182774,182780,182781,182783,182792
                                       ,63736,63737,63774,182782,133922)),"early_interval"]<-c("Late Bajocian")
  brach[which(brach$collection_no%in%c(133921,133923,133924,56241,56245,56249,56250,56241,56283,56285,56241,56273,56241,56285,134288,134403,182714:182716,182722:182725,182726,182755,182774,182780,182781,182783,182792
                                       ,63736,63737,63774,182782,133922)),"late_interval"]<-c("Anceps") #"Late Temaikan"

  brach[which(brach$collection_no%in%c(134361,56252,133898,133901,133915,134402,134404,134405,4878)),"early_interval"]<-c("Late Aalenian")
  brach[which(brach$collection_no%in%c(134361,56252,133898,133901,133915,134402,134404,134405,4878)),"late_interval"]<-c("Anceps") #"Middle-late Temaikan"

  brach[which(brach$collection_no%in%c(134228,134229,182710:182713,182756,182787,56237:56240,56242:56244,56264)),"early_interval"]<-c("Late Aalenian")
  brach[which(brach$collection_no%in%c(134228,134229,182710:182713,182756,182787,56237:56240,56242:56244,56264)),"late_interval"]<-c("Early Bajocian") #"Middle Temaikan"

  brach[which(brach$collection_no%in%c(134291,134355,182727)),"early_interval"]<-c("Early Bajocian")
  brach[which(brach$collection_no%in%c(134291,134355,182727)),"late_interval"]<-c("Early Bajocian") #"Upper middle Temaikan"

  brach[which(brach$collection_no%in%c(134370,182786)),"early_interval"]<-c("Late Bajocian")
  brach[which(brach$collection_no%in%c(134370,182786)),"late_interval"]<-c("Late Bajocian") #"Base of upper Temaikan"

  brach[which(brach$collection_no%in%c(135072,133916:133918,38958,38963,38964,56279:56281,56398,63729,63732,63734,63738,63753,63775,63817:63820)),"early_interval"]<-c("Late Toarcian")
  brach[which(brach$collection_no%in%c(135072,133916:133918,38958,38963,38964,56279:56281,56398,63729,63732,63734,63738,63753,63775,63817:63820)),"late_interval"]<-c("Middle Aalenian") #"Early Temaikan"

  brach[which(brach$collection_no%in%c(135103,135211,135271,135312:135314)),"early_interval"]<-c("Late Toarcian")
  brach[which(brach$collection_no%in%c(135103,135211,135271,135312:135314)),"late_interval"]<-c("Late Toarcian") #"Lowest Temaikan"

  brach[which(brach$collection_no%in%c(51932)),"early_interval"]<-c("Late Toarcian")
  brach[which(brach$collection_no%in%c(51932)),"late_interval"]<-c("Anceps") #"Temaikan"

  brach[which(brach$collection_no%in%c(133926,133935,133939,133941:133955,133966,133967,133969,133970,133975:133979,133993,133994,133996:134002,134014,134015,134033,134034
                                       ,134072:134080,134082,134083,134145,134147,134148,134205:134207,134214,134278,134283,134285:134287,134354,134356,134359,134366,134374
  									 ,134375,134377:134380,134391:134398,135121,135122,135126,160386,38957,133938,133940,134146)),"early_interval"]<-c("Bifrons")
  brach[which(brach$collection_no%in%c(133926,133935,133939,133941:133955,133966,133967,133969,133970,133975:133979,133993,133994,133996:134002,134014,134015,134033,134034
                                       ,134072:134080,134082,134083,134145,134147,134148,134205:134207,134214,134278,134283,134285:134287,134354,134356,134359,134366,134374
  									 ,134375,134377:134380,134391:134398,135121,135122,135126,160386,38957,133938,133940,134146)),"late_interval"]<-c("Variabilis") #"Late Ururoan"

  brach[which(brach$collection_no%in%c(63809,63814,63831,137930,38931:38933)),"early_interval"]<-c("Ibex")
  brach[which(brach$collection_no%in%c(63809,63814,63831,137930,38931:38933)),"late_interval"]<-c("Variabilis") #"Ururoan"

  brach[which(brach$collection_no%in%c(134003,134181,135007,135008,135015,135098,135113:135115,135127,135224,135226,135228,135230:135232,135258,135266:135267,135270,135274,137848
                                       ,204591,50763)),"early_interval"]<-c("Ibex")
  brach[which(brach$collection_no%in%c(134003,134181,135007,135008,135015,135098,135113:135115,135127,135224,135226,135228,135230:135232,135258,135266:135267,135270,135274,137848
                                       ,204591,50763)),"late_interval"]<-c("Serpentinum") #"Early Ururoan"

  brach[which(brach$collection_no%in%c(133928,133934,134005,134011,134012,134016,134017,134131,134158,134209,134210,134218,134381,134383:134387,135010:135012,135014,135045
                                       ,135046,135073,135075:135079,135086:135090,135099,135102,135104:135107,135111,135112,135120,135123,135173,135175,135178,135185,135222
  									 ,135223,135257,38929)),"early_interval"]<-c("Sinemurian")
  brach[which(brach$collection_no%in%c(133928,133934,134005,134011,134012,134016,134017,134131,134158,134209,134210,134218,134381,134383:134387,135010:135012,135014,135045
                                       ,135046,135073,135075:135079,135086:135090,135099,135102,135104:135107,135111,135112,135120,135123,135173,135175,135178,135185,135222
  									 ,135223,135257,38929)),"late_interval"]<-c("Jamesoni") #"Late Aratauran"

  brach[which(brach$collection_no%in%c(38928,38934,38955,51864,51868)),"early_interval"]<-c("Hettangian")
  brach[which(brach$collection_no%in%c(38928,38934,38955,51864,51868)),"late_interval"]<-c("Jamesoni") #"Aratauran"


  ######These revisions are about the Permian collections from New Zealand. Ages following Waterhouse 2002, 2021

  brach[which(brach$collection_no%in%c(41775,41791,52787,62063)),"early_interval"]<-c("Early Artinskian")
  brach[which(brach$collection_no%in%c(41775,41791,52787,62063)),"late_interval"]<-c("Early Artinskian") #Notostrophia zealandicus, Notostrophia homeri,Terrakea dickinsi zones

  brach[which(brach$collection_no%in%c(41790,137851)),"early_interval"]<-c("Late Artinskian")
  brach[which(brach$collection_no%in%c(41790,137851)),"late_interval"]<-c("Late Artinskian") #Echinalosia prideri (now Echinalosia conata) brachiopod zone

  brach[which(brach$collection_no%in%c(52696,39926,39927,39928,39929,39930,39931,41768,41771,52763,52766,52782,52798,52896,41780,52832,120440,73851,120639,41769,120441,120423,120426,120430:120433
                                       ,120436:120439,121735,41774,41786,41765)),"early_interval"]<-c("Early Changhsingian")
  brach[which(brach$collection_no%in%c(52696,39926,39927,39928,39929,39930,39931,41768,41771,52763,52766,52782,52798,52896,41780,52832,120440,73851,120639,41769,120441,120423,120426,120430:120433
                                       ,120436:120439,121735,41774,41786,41765)),"late_interval"]<-c("Early Changhsingian") #Plekonella multicostata, Spinomartinia spinosa zones

  brach[which(brach$collection_no%in%c(7276,120641,52751,52752,52753,52755,52757,52759,52760,52767,52777:52780,52783:52786,52788,52793,52796,7204,98149,52244,52245,52774,52775,52776,52781,52794)),"early_interval"]<-c("Late Changhsingian")
  brach[which(brach$collection_no%in%c(7276,120641,52751,52752,52753,52755,52757,52759,52760,52767,52777:52780,52783:52786,52788,52793,52796,7204,98149,52244,52245,52774,52775,52776,52781,52794)),"late_interval"]<-c("Late Changhsingian") #Wairakiella rostrata, Marginalosia planata zones

  brach[which(brach$collection_no%in%c(39923:39925,41772,120434,120435,41794,41770)),"early_interval"]<-c("Late Wuchiapingian")
  brach[which(brach$collection_no%in%c(39923:39925,41772,120434,120435,41794,41770)),"late_interval"]<-c("Late Wuchiapingian") #Martiniopsis woodi Zone

  brach[which(brach$formation=="Flowers"),"early_interval"]<-c("Early Capitanian")
  brach[which(brach$formation=="Flowers"),"late_interval"]<-c("Early Capitanian") #Wordian in PBDB. Waterhouse 2002

  brach[which(brach$collection_no%in%c(213211)),"early_interval"]<-c("Anisian")
  brach[which(brach$collection_no%in%c(213211)),"late_interval"]<-c("Anisian") #Tongue Point Member (Greville Formation), not Permian, but Triassic. See Waterhouse 2002

  brach[which(brach$formation=="Letham"),"early_interval"]<-c("Late Kungurian")
  brach[which(brach$formation=="Letham"),"late_interval"]<-c("Roadian") #Waterhouse 2002

  brach[which(brach$collection_no%in%c(120427,41767,41777,41778,41779,41784,41785,41788,41789,52188,52189,52773,52795,52833:52835)),"early_interval"]<-c("Capitanian")
  brach[which(brach$collection_no%in%c(120427,41767,41777,41778,41779,41784,41785,41788,41789,52188,52189,52773,52795,52833:52835)),"late_interval"]<-c("Capitanian") #Waterhouse 2002

  brach[which(brach$collection_no%in%c(41796)),"early_interval"]<-c("Late Kungurian")
  brach[which(brach$collection_no%in%c(41796)),"late_interval"]<-c("Roadian")  #Echinalosia discinia Zone. Wordian in PBDB. Waterhouse 2002.

  brach[which(brach$collection_no%in%c(41798)),"early_interval"]<-c("Late Artinskian")
  brach[which(brach$collection_no%in%c(41798)),"late_interval"]<-c("Early Kungurian") #The species was discovered from the Kungurian and Artinskian



  ######These revisions are about the Permian collections from Australia. See Waterhouse 2015, 2021

  #These collections were treated to be Guadalupian by Waterhouse according to brachiopod zones. But they were dated as Wuchiapingian by geochronologic methods. Here we follow the absolute ages
  #Thus, the Guadalupian records will be much fewer than in PBDB
  #brach[which(brach$formation=="Barfield"),"early_interval"]<-c("Wordian")
  #brach[which(brach$formation=="Barfield"),"late_interval"]<-c("Wordian") #Wuchiapingian in PBDB. Pseudostrophalosia blakei Zone (Wordian) in Waterhouse 2015
  #brach[which(brach$formation=="Blenheim"),"early_interval"]<-c("Wordian")
  #brach[which(brach$formation=="Blenheim"),"late_interval"]<-c("Capitanian")
  #brach[which(brach$formation=="Blenheim"&brach$member=="Moonlight Sandstone"),"early_interval"]<-c("Wordian")
  #brach[which(brach$formation=="Blenheim"&brach$member=="Moonlight Sandstone"),"late_interval"]<-c("Wordian") #Mostly Wuchiapingian in PBDB. According to brachiopod zone. See Waterhouse 2008
  #brach[which(brach$formation=="Flat Top"),"early_interval"]<-c("Wordian")
  #brach[which(brach$formation=="Flat Top"),"late_interval"]<-c("Wordian") ##Wuchiapingian in PBDB. Wordian in Waterhouse 2015
  #brach[which(brach$formation=="Catherine"),"early_interval"]<-c("Capitanian")
  #brach[which(brach$formation=="Catherine"),"late_interval"]<-c("Capitanian") #Wuchiapingian in PBDB. Echinalosia ovalis Zone (Wordian or Capitanian) in Waterhouse 2015
  #brach[which(brach$formation=="Freitag"),"early_interval"]<-c("Kungurian")
  #brach[which(brach$formation=="Freitag"),"late_interval"]<-c("Kungurian") #Wuchiapingian in PBDB. Wyndhamia typica Zone in Waterhouse 2015
  #brach[which(brach$formation=="Ingelara"),"early_interval"]<-c("Roadian")
  #brach[which(brach$formation=="Ingelara"),"late_interval"]<-c("Wordian") #Wuchiapingian in PBDB. Roadian-Wordian in Waterhouse 2015
  #brach[which(brach$formation=="Oxtrack"),"early_interval"]<-c("Roadian")
  #brach[which(brach$formation=="Oxtrack"),"late_interval"]<-c("Roadian") #Wordian in PBDB. Roadian in Waterhouse 2015. This formation is below the Barfield Formation and may be Wuchiapingian as well???
  #brach[which(brach$formation=="Peawaddy"),"early_interval"]<-c("Wordian")
  #brach[which(brach$formation=="Peawaddy"),"late_interval"]<-c("Wordian") #Wuchiapingian in PBDB. Wordian in Waterhouse 2015

  brach[which(brach$formation=="Abels Bay"),"early_interval"]<-c("Capitanian")
  brach[which(brach$formation=="Abels Bay"),"late_interval"]<-c("Wuchiapingian") #Wuchiapingian in PBDB. Middle Permian in Waterhouse 2015, but Birchsella was regarded as a Cisuralian genus by Waterhouse (2016). Capitanian in Mii et al. 2012

  brach[which(brach$formation=="Aldebaran"),"early_interval"]<-c("Artinskian")
  brach[which(brach$formation=="Aldebaran"),"late_interval"]<-c("Kungurian") #Roadian in PBDB. The Aldebaran Formation of the Springsure region contains late Early Permian brachiopods, but may range into older Permian. Waterhouse 2015

  brach[which(brach$formation=="Allandale"),"early_interval"]<-c("Late Asselian")
  brach[which(brach$formation=="Allandale"),"late_interval"]<-c("Early Sakmarian") #Sakmarian in PBDB. The Allandale formation correlates to the Crassispinosella subcircularis Zone by Waterhouse (2015),
  #which is thought to be middle Asselian by Waterhouse (2015). The Alum Rock correlates to subcircularis Zone or possibly concentrica Zone below it. The two U–Pb zircon SHRIMP ages from the Alum Rock Conglomerate are
  #approximately 294 Ma and 296 Ma, and they together may support a late Asselian to earliest Sakmarian age (See Shi et al. 2022). Therefore, here the collections belong to middle Asselian biozones of Waterhouse
  #and Sakmarian in PBDB are regarded as late Asselian-early Sakmarian

  brach[which(brach$formation=="Alum Rock Beds"),"early_interval"]<-c("Late Asselian")
  brach[which(brach$formation=="Alum Rock Beds"),"late_interval"]<-c("Early Sakmarian") #Sakmarian in PBDB. Shi et al. 2022

  brach[which(brach$formation=="Baker"),"early_interval"]<-c("Early Roadian")
  brach[which(brach$formation=="Baker"),"late_interval"]<-c("Early Roadian") #Kungurian in PBDB. Haig 2018

  brach[which(brach$formation=="Beckers"),"early_interval"]<-c("Late Asselian")
  brach[which(brach$formation=="Beckers"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  brach[which(brach$formation=="Belford"),"early_interval"]<-c("Roadian")
  brach[which(brach$formation=="Belford"),"late_interval"]<-c("Roadian") #Roadian-Wordian in PBDB. Waterhouse 2015

  brach[which(brach$formation=="Berriedale"),"early_interval"]<-c("Early Artinskian")
  brach[which(brach$formation=="Berriedale"),"late_interval"]<-c("Early Artinskian") #Kungurian in PBDB. Taeniothaerus subquadratus Zone. Late Sakmarian in Waterhouse 2015, bur this zone was regarded as early Artinskian by Shi et al. 2020

  brach[which(brach$formation=="Berry"),"early_interval"]<-c("Early Wordian")
  brach[which(brach$formation=="Berry"),"late_interval"]<-c("Early Wordian") #Wordian in PBDB. Shi et al. 2022

  brach[which(brach$formation=="Berserker"),"early_interval"]<-c("Early Artinskian")
  brach[which(brach$formation=="Berserker"),"late_interval"]<-c("Early Artinskian") #Kungurian in PBDB. Taeniothaerus subquadratus Zone. Lakes Creek beds in Waterhouse 2015

  brach[which(brach$formation=="Billidee"),"early_interval"]<-c("Early Artinskian")
  brach[which(brach$formation=="Billidee"),"late_interval"]<-c("Early Artinskian") #jimbaensis zone. Waterhouse 2015

  brach[which(brach$formation=="Billop"),"early_interval"]<-c("Late Asselian")
  brach[which(brach$formation=="Billop"),"late_interval"]<-c("Early Sakmarian") #Billpo Formation. Sakmarian in PBDB, Asselian in Waterhouse 2015

  brach[which(brach$formation=="Brae"),"early_interval"]<-c("Late Kungurian")
  brach[which(brach$formation=="Brae"),"late_interval"]<-c("Late Kungurian") #Roadian in PBDB. Echinalosia discinia Zone. Latest Kungurian according to Waterhouse 2015

  brach[which(brach$formation=="Branxton"),"early_interval"]<-c("Late Kungurian")
  brach[which(brach$formation=="Branxton"),"late_interval"]<-c("Roadian") #Kungurian-Roadian in PBDB. Wyndhamia typica-Echinalosia discinia zones. Late Kungurian in Waterhouse 2015. But the the lower Branxton Formation 12m above the topmost Greta Coal seam was dated as 272.2 ± 3.2 Ma (see Waterhouse 2002), which is Roadian now

  brach[which(brach$formation=="Buffel"),"early_interval"]<-c("Sakmarian")
  brach[which(brach$formation=="Buffel"),"late_interval"]<-c("Early Artinskian")
  brach[which(brach$formation=="Buffel"&brach$member=="Dresden"),"early_interval"]<-c("Sakmarian")
  brach[which(brach$formation=="Buffel"&brach$member=="Dresden"),"late_interval"]<-c("Sakmarian")
  brach[which(brach$formation=="Buffel"&brach$member=="Fairyland"),"early_interval"]<-c("Sakmarian")
  brach[which(brach$formation=="Buffel"&brach$member=="Fairyland"),"late_interval"]<-c("Sakmarian")
  brach[which(brach$formation=="Buffel"&brach$member=="Elvinia"),"early_interval"]<-c("Late Sakmarian")
  brach[which(brach$formation=="Buffel"&brach$member=="Elvinia"),"late_interval"]<-c("Late Sakmarian")
  brach[which(brach$formation=="Buffel"&brach$member=="Rose's Pride"),"early_interval"]<-c("Artinskian")
  brach[which(brach$formation=="Buffel"&brach$member=="Rose's Pride"),"late_interval"]<-c("Artinskian") #Artinskian-Kungurian in PBDB. Waterhouse 2015

  brach[which(brach$formation=="Bulgadoo"),"early_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Bulgadoo"),"late_interval"]<-c("Late Artinskian") #Kungurian in PBDB. Haig et al. 2017

  brach[which(brach$formation=="Bundella"),"early_interval"]<-c("Late Asselian")
  brach[which(brach$formation=="Bundella"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  brach[which(brach$formation=="Burnett"),"early_interval"]<-c("Late Asselian")
  brach[which(brach$formation=="Burnett"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  brach[which(brach$formation=="Callytharra"&brach$early_interval=="Artinskian"),"early_interval"]<-c("Early Artinskian")
  brach[which(brach$formation=="Callytharra"&brach$late_interval=="Artinskian"),"late_interval"]<-c("Early Artinskian") #Sakmarian-Artinskian in PBDB. "Coronalosia irwinensis" Zone. Waterhouse 2015

  brach[which(brach$formation=="Calytrix"),"early_interval"]<-c("Late Asselian")
  brach[which(brach$formation=="Calytrix"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  brach[which(brach$formation=="Camboon"),"early_interval"]<-c("Sakmarian")
  brach[which(brach$formation=="Camboon"),"late_interval"]<-c("Sakmarian") #Kungurian in PBDB. Possibly Sakmarian in Waterhouse 2015

  brach[which(brach$formation=="Carmila"),"early_interval"]<-c("Sakmarian")
  brach[which(brach$formation=="Carmila"),"late_interval"]<-c("Sakmarian") #Artinskian in PBDB. But the species possibly suggest Sakmarian

  brach[which(brach$formation=="Carolyn"),"early_interval"]<-c("Late Asselian")
  brach[which(brach$formation=="Carolyn"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  brach[which(brach$formation=="Cattle Creek"),"early_interval"]<-c("Artinskian")
  brach[which(brach$formation=="Cattle Creek"),"late_interval"]<-c("Artinskian") #Artinskian-Kungurian in PBDB. Waterhouse 2015

  brach[which(brach$formation=="Collinsville Coal Measures"),"early_interval"]<-c("Kungurian")
  brach[which(brach$formation=="Collinsville Coal Measures"),"late_interval"]<-c("Kungurian") #Glendoo Sandstone thought to be lateral equivalent of the middle part of the Gebbie Formation,
  #most likely belonging to the upper Echinalosia maxwelli, E. davidi, or lower E. discinia brachiopod zones of Briggs (1998). Possibly Kungurian according to Shi et al. 2022. Kungurian in Waterhouse 2015

  brach[which(brach$formation=="Colraine"),"early_interval"]<-c("Sakmarian")
  brach[which(brach$formation=="Colraine"),"late_interval"]<-c("Sakmarian")  #Vanderlaan and Ebach 2015

  brach[which(brach$formation=="Coyrie"),"early_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Coyrie"),"late_interval"]<-c("Late Artinskian") #Artinskian in PBDB. Haig et al. 2017

  brach[which(brach$formation=="Cundlego"),"early_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Cundlego"),"late_interval"]<-c("Early Kungurian") #Artinskian-Kungurian in PBDB. #Haig et al. 2017

  brach[which(brach$formation=="Darlington"),"early_interval"]<-c("Late Asselian")
  brach[which(brach$formation=="Darlington"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  brach[which(brach$formation=="Eight Mile Creek"),"early_interval"]<-c("Kungurian")
  brach[which(brach$formation=="Eight Mile Creek"),"late_interval"]<-c("Kungurian") #Roadian in PBDB. Echinalosia maxwelli of Briggs. Possibly Kungurian according to Shi et al. 2022

  brach[which(brach$formation=="Elderslie"),"early_interval"]<-c("Kungurian")
  brach[which(brach$formation=="Elderslie"),"late_interval"]<-c("Kungurian") #Roadian in PBDB. Echinalosia maxwelli of Briggs. Possibly Kungurian according to Shi et al. 2022

  brach[which(brach$formation=="Eurydesma Beds"),"early_interval"]<-c("Sakmarian")
  brach[which(brach$formation=="Eurydesma Beds"),"late_interval"]<-c("Sakmarian") #Artinskian in PBDB. Bookeria pollex Zone. Sakmarian in Waterhouse 2015

  brach[which(brach$formation=="Exmoor"),"early_interval"]<-c("Wuchiapingian")
  brach[which(brach$formation=="Exmoor"),"late_interval"]<-c("Wuchiapingian") #The species is Wuchiapingian in age

  brach[which(brach$formation=="Farley"),"early_interval"]<-c("Late Sakmarian")
  brach[which(brach$formation=="Farley"),"late_interval"]<-c("Artinskian") #Artinskian in PBDB. Sakmarian in Waterhouse 2015

  brach[which(brach$formation=="Gebbie"),"early_interval"]<-c("Kungurian")
  brach[which(brach$formation=="Gebbie"),"late_interval"]<-c("Kungurian") #Roadian in PBDB. Kungurian in Waterhouse 2015

  brach[which(brach$formation=="Glencoe"),"early_interval"]<-c("Late Asselian")
  brach[which(brach$formation=="Glencoe"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  brach[which(brach$formation=="Golden Valley"),"early_interval"]<-c("Late Asselian")
  brach[which(brach$formation=="Golden Valley"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  brach[which(brach$formation=="Gray Siltstone"),"early_interval"]<-c("Late Sakmarian")
  brach[which(brach$formation=="Gray Siltstone"),"late_interval"]<-c("Late Sakmarian") #Artinskian in PBDB. Upper Gray Formation or basal Berriedale Limestone-Enstone Park limestone correlate

  brach[which(brach$formation=="High Cliff Sandstone"),"early_interval"]<-c("Late Sakmarian")
  brach[which(brach$formation=="High Cliff Sandstone"),"late_interval"]<-c("Early Artinskian") #Artinskian in PBDB. Dillinger et al. 2018

  brach[which(brach$formation=="Hickman"),"early_interval"]<-c("Late Asselian")
  brach[which(brach$formation=="Hickman"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  brach[which(brach$formation=="Inglis"),"early_interval"]<-c("Late Asselian")
  brach[which(brach$formation=="Inglis"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  brach[which(brach$formation=="Jimba Jimba"),"early_interval"]<-c("Early Artinskian")
  brach[which(brach$formation=="Jimba Jimba"),"late_interval"]<-c("Early Artinskian") #Artinskian in PBDB. Waterhouse 2015

  brach[which(brach$formation=="Kansas Creek"),"early_interval"]<-c("Late Asselian")
  brach[which(brach$formation=="Kansas Creek"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  brach[which(brach$formation=="Kensington"),"early_interval"]<-c("Late Asselian")
  brach[which(brach$formation=="Kensington"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  brach[which(brach$formation=="Lochinvar"),"early_interval"]<-c("Late Asselian")
  brach[which(brach$formation=="Lochinvar"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  brach[which(brach$formation=="Madeline"),"early_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Madeline"),"late_interval"]<-c("Late Artinskian") #Artinskian-Kungurian in PBDB. Haig et al. 2017

  brach[which(brach$formation=="Malbina"&brach$early_interval=="Roadian"),"early_interval"]<-c("Kungurian")
  brach[which(brach$formation=="Malbina"&brach$late_interval=="Roadian"),"late_interval"]<-c("Kungurian")
  brach[which(brach$formation=="Malbina"&brach$early_interval=="Wordian"),"early_interval"]<-c("Roadian")
  brach[which(brach$formation=="Malbina"&brach$late_interval=="Wordian"),"late_interval"]<-c("Roadian") #According to the brachiopod zone and the ages shown in Shi et al. 2022

  brach[which(brach$formation=="Mallens"),"early_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Mallens"),"late_interval"]<-c("Late Artinskian") #Artinskian in PBDB. Haig et al. 2017

  brach[which(brach$formation=="Mingenew"),"early_interval"]<-c("Artinskian")
  brach[which(brach$formation=="Mingenew"),"late_interval"]<-c("Artinskian") #Artinskian-Kungurian in PBDB. Artinskian in Waterhouse 2015

  brach[which(brach$formation=="Nalbia"),"early_interval"]<-c("Late Kungurian")
  brach[which(brach$formation=="Nalbia"),"late_interval"]<-c("Late Kungurian") #Kungurian in PBDB. Haig et al. 2017

  brach[which(brach$formation=="Noonkanbah"&brach$early_interval=="Artinskian"),"early_interval"]<-c("Late Artinskian")
  brach[which(brach$formation=="Noonkanbah"&brach$late_interval=="Artinskian"),"late_interval"]<-c("Late Artinskian") #Haig et al. 2017

  brach[which(brach$formation=="One Gum"),"early_interval"]<-c("Early Artinskian")
  brach[which(brach$formation=="One Gum"),"late_interval"]<-c("Early Artinskian") #Artinskian in PBDB. Strophalosia jimbaensis zone

  brach[which(brach$formation=="Poole"&brach$early_interval=="Sakmarian"),"early_interval"]<-c("Late Sakmarian")
  brach[which(brach$formation=="Poole"&brach$late_interval=="Sakmarian"),"late_interval"]<-c("Late Sakmarian")
  brach[which(brach$formation=="Poole"&brach$early_interval=="Artinskian"),"early_interval"]<-c("Early Artinskian")
  brach[which(brach$formation=="Poole"&brach$late_interval=="Artinskian"),"late_interval"]<-c("Early Artinskian") #Haig et al. 2017

  brach[which(brach$formation=="Quamby"),"early_interval"]<-c("Late Asselian")
  brach[which(brach$formation=="Quamby"),"late_interval"]<-c("Early Sakmarian") #Asselian-Samkarian in PBDB; Asselian in Waterhouse 2015

  brach[which(brach$formation=="Quinnanie"),"early_interval"]<-c("Early Kungurian")
  brach[which(brach$formation=="Quinnanie"),"late_interval"]<-c("Early Kungurian") #Artinskian in PBDB. Haig et al. 2017

  brach[which(brach$formation=="Rammutt"),"early_interval"]<-c("Late Asselian")
  brach[which(brach$formation=="Rammutt"),"late_interval"]<-c("Sakmarian") #Artinskian in PBDB. Asselian in Waterhouse 2015

  brach[which(brach$formation=="Rutherford"),"early_interval"]<-c("Sakmarian")
  brach[which(brach$formation=="Rutherford"),"late_interval"]<-c("Sakmarian") #Sakmarian-Artinskian in PBDB. Sakmarian in Waterhouse 2015

  brach[which(brach$formation=="Silver Spur"),"early_interval"]<-c("Sakmarian")
  brach[which(brach$formation=="Silver Spur"),"late_interval"]<-c("Sakmarian") #Sakmarian-Artinskian in PBDB. Asselian in Waterhouse 2015. Bandoproductus Zone. The Artinskian records in PBDB include some species possible of the Bookeria Zone

  brach[which(brach$formation=="Snapper Point"),"early_interval"]<-c("Early Kungurian")
  brach[which(brach$formation=="Snapper Point"),"late_interval"]<-c("Early Kungurian") #Roadian in PBDB. Shi et al. 2022

  brach[which(brach$formation=="South Curra"),"early_interval"]<-c("Changhsingian")
  brach[which(brach$formation=="South Curra"),"late_interval"]<-c("Changhsingian") #Artinskian-Wuchiapingian in PBDB. Changhsingian in Waterhouse 2015

  brach[which(brach$formation=="Spreyton"),"early_interval"]<-c("Late Asselian")
  brach[which(brach$formation=="Spreyton"),"late_interval"]<-c("Early Sakmarian") #Sakmarian in PBDB. May be Strophalosiaria concentrica Zone

  brach[which(brach$formation=="Stanleigh"),"early_interval"]<-c("Sakmarian")
  brach[which(brach$formation=="Stanleigh"),"late_interval"]<-c("Sakmarian") #Artinskian in PBDB. The species Terrakea pollex possibly belongs to the Bookeria Zone

  brach[which(brach$formation=="Swifts Jetty Sandstone"),"early_interval"]<-c("Late Asselian")
  brach[which(brach$formation=="Swifts Jetty Sandstone"),"late_interval"]<-c("Early Sakmarian")
  brach[which(brach$stratgroup=="Masseys Creek"),"early_interval"]<-c("Late Asselian")
  brach[which(brach$stratgroup=="Masseys Creek"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  brach[which(brach$formation=="Tiverton"),"early_interval"]<-c("Late Sakmarian")
  brach[which(brach$formation=="Tiverton"),"late_interval"]<-c("Artinskian") #Artinskian in PBDB. Waterhouse 2015

  brach[which(brach$formation=="Tomago"),"early_interval"]<-c("Wuchiapingian")
  brach[which(brach$formation=="Tomago"),"late_interval"]<-c("Wuchiapingian") #Melehan et al. 2021. Wordian in PBDB

  brach[which(brach$formation=="Wallaby"),"early_interval"]<-c("Sakmarian")
  brach[which(brach$formation=="Wallaby"),"late_interval"]<-c("Sakmarian") #Artinskian in PBDB. Sakmarian in Waterhouse 2015

  brach[which(brach$formation=="Wandrawandian"),"early_interval"]<-c("Late Kungurian")
  brach[which(brach$formation=="Wandrawandian"),"late_interval"]<-c("Roadian") #Roadian-Wordian in PBDB. Shi et al. 2022

  brach[which(brach$formation=="Wasp Head"),"early_interval"]<-c("Late Asselian")
  brach[which(brach$formation=="Wasp Head"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  brach[which(brach$formation=="Watermark"),"early_interval"]<-c("Capitanian")
  brach[which(brach$formation=="Watermark"),"late_interval"]<-c("Capitanian") #Wordian or Wuchiapingian in PBDB. Laurie et al. 2016

  brach[which(brach$formation=="Woody Island"),"early_interval"]<-c("Late Asselian")
  brach[which(brach$formation=="Woody Island"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  brach[which(brach$formation=="Yarrol"),"early_interval"]<-c("Late Sakmarian")
  brach[which(brach$formation=="Yarrol"),"late_interval"]<-c("Artinskian") #Artinskian in PBDB. Sakmarian in Waterhouse 2015

  brach[which(brach$stratgroup=="Lyons"&brach$early_interval=="Sakmarian"),"early_interval"]<-c("Early Sakmarian")
  brach[which(brach$stratgroup=="Lyons"&brach$late_interval=="Sakmarian"),"late_interval"]<-c("Early Sakmarian") #See Waterhouse 2015

  brach[which(brach$stratgroup=="Back Creek"&brach$early_interval=="Kungurian"),"early_interval"]<-c("Artinskian")
  brach[which(brach$stratgroup=="Back Creek"&brach$late_interval=="Kungurian"),"late_interval"]<-c("Artinskian") #Echinalosia preovalis Zone of Briggs 1998

  return(brach)
}
