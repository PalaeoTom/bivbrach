apply_Guo2023_bivalves <- function(bival, GTS2020){
  bival[which(bival$genus=="Acesta (Acesta)"),"genus"]<-c("Acesta")
  bival[which(bival$genus=="Acharax (Nacrosolemya)"),"genus"]<-c("Acharax")
  bival[which(bival$genus=="Acila (Truncacila)"),"genus"]<-c("Acila")
  bival[which(bival$genus=="Amphidonte (Amphidonte)"),"genus"]<-c("Amphidonte")
  bival[which(bival$genus=="Amusium (Pseudamussium)"),"genus"]<-c("Pseudamussium")
  bival[which(bival$genus=="Anadara (Cunearca)"),"genus"]<-c("Anadara")
  bival[which(bival$genus=="Anisocardia (Anisocardia)"),"genus"]<-c("Anisocardia")
  bival[which(bival$genus=="Anisocardia (Antiquicyprina)"),"genus"]<-c("Antiquicyprina")
  bival[which(bival$genus=="Anomia (Anomia)"),"genus"]<-c("Anomia")
  bival[which(bival$genus=="Antiquilima (Antiquilima)"),"genus"]<-c("Antiquilima")
  bival[which(bival$genus=="Aphrodina (Tikia)"),"genus"]<-c("Aphrodina")
  bival[which(bival$genus=="Apiotrigonia (Apiotrigonia)"),"genus"]<-c("Apiotrigonia")
  bival[which(bival$genus=="Arca (Arca)"),"genus"]<-c("Arca")
  bival[which(bival$genus=="Astarte (Astarte)"),"genus"]<-c("Astarte")
  bival[which(bival$genus=="Astarte (Eriphylopsis)"),"genus"]<-c("Eriphylopsis")
  bival[which(bival$genus=="Astarte (Neocrassina)"),"genus"]<-c("Neocrassina")
  bival[which(bival$genus=="Astarte (Trautscholdia)"),"genus"]<-c("Nicaniella") #Following Treatise
  bival[which(bival$genus=="Avicula (Oxytoma)"),"genus"]<-c("Oxytoma")
  bival[which(bival$genus=="Aviculopecten (Oxypteria)"),"genus"]<-c("Oxypteria")
  bival[which(bival$genus=="Bakevellia (Bakevellia)"),"genus"]<-c("Bakevellia")
  bival[which(bival$genus=="Bakevellia (Bakevelloides)"),"genus"]<-c("Bakevellia")
  bival[which(bival$genus=="Bakevellia (Boreiobakevellia)"),"genus"]<-c("Bakevellia")
  bival[which(bival$genus=="Bakevellia (Costibakevellia)"),"genus"]<-c("Bakevellia")
  bival[which(bival$genus=="Bakevellia (Integribakevellia)"),"genus"]<-c("Bakevellia")
  bival[which(bival$genus=="Bakevellia (Maizuria)"),"genus"]<-c("Bakevellia")
  bival[which(bival$genus=="Bakevellia (Neobakevellia)"),"genus"]<-c("Bakevellia")
  bival[which(bival$genus=="Barbatia (Barbatia)"),"genus"]<-c("Barbatia")
  bival[which(bival$genus=="Barbatia (Cucullaearca)"),"genus"]<-c("Barbatia")
  bival[which(bival$genus=="Barnea (Anchomasa)"),"genus"]<-c("Barnea")
  bival[which(bival$genus=="Brachidontes (Aeidimytilus)"),"genus"]<-c("Aeidimytilus")
  bival[which(bival$genus=="Brachidontes (Arcomytilus)"),"genus"]<-c("Arcomytilus")
  bival[which(bival$genus=="Brachidontes (Brachidontes)"),"genus"]<-c("Brachidontes")
  bival[which(bival$genus=="Caestocorbula (Caestocorbula)"),"genus"]<-c("Caestocorbula")
  bival[which(bival$genus=="Callista (Microcallista)"),"genus"]<-c("Callista")
  bival[which(bival$genus=="Calva (Calva)"),"genus"]<-c("Calva")
  bival[which(bival$genus=="Calva (Egelicalva)"),"genus"]<-c("Calva")
  bival[which(bival$genus=="Calva (Microcalva)"),"genus"]<-c("Calva")
  bival[which(bival$genus=="Calva (Penecallista)"),"genus"]<-c("Calva")
  bival[which(bival$genus=="Camptonectes (Annulinectes)"),"genus"]<-c("Camptonectes")
  bival[which(bival$genus=="Camptonectes (Boreionectes)"),"genus"]<-c("Camptonectes")
  bival[which(bival$genus=="Camptonectes (Camptochlamys)"),"genus"]<-c("Camptochlamys") #See Ros et al. 2014
  bival[which(bival$genus=="Camptonectes (Camptonectes)"),"genus"]<-c("Camptonectes")
  bival[which(bival$genus=="Camptonectes (Costicamptonectes)"),"genus"]<-c("Camptonectes")
  bival[which(bival$genus=="Camptonectes (Hunanonectes)"),"genus"]<-c("Radulonectites") #Following Ros et al. 2014
  bival[which(bival$genus=="Camptonectes (Maclearnia)"),"genus"]<-c("Camptonectes")
  bival[which(bival$genus=="Cardium (Tulongocardium)"),"genus"]<-c("Tulongocardium")
  bival[which(bival$genus=="Catella (Oceanopieris)"),"genus"]<-c("Catella")
  bival[which(bival$genus=="Cavilucina (Mesomiltha)"),"genus"]<-c("Mesomiltha")
  bival[which(bival$genus=="Cercomya (Capillimya)"),"genus"]<-c("Cercomya")
  bival[which(bival$genus=="Ceromya (Ceromyopsis)"),"genus"]<-c("Ceratomyopsis")
  bival[which(bival$genus=="Chlamys (Aequipecten)"),"genus"]<-c("Aequipecten")
  bival[which(bival$genus=="Chlamys (Camptochlamys)"),"genus"]<-c("Camptochlamys")
  bival[which(bival$genus=="Chlamys (Chlamys)"),"genus"]<-c("Chlamys")
  bival[which(bival$genus=="Chlamys (Lyriochlamys)"),"genus"]<-c("Lyriochlamys") #Costamussium Yin and Nie 1990 is probably is a junior synonym of the subgenus Chlamys (Lyriochlamys) according to Fang 2009. The latter is regarded as a genus in PBDB
  bival[which(bival$genus=="Chlamys (Mimachlamys)"),"genus"]<-c("Mimachlamys")
  bival[which(bival$genus=="Chlamys (Praechlamys)"),"genus"]<-c("Chlamys") #Following Ros et al. 2014
  bival[which(bival$genus=="Chlamys (Radulopecten)"),"genus"]<-c("Radulopecten")
  bival[which(bival$genus=="Cingentolium (Colpentolium)"),"genus"]<-c("Entolium") #Waller 2016 said Colpentolium has nothing to to distinguish it from Cingentolium; Cingentolium and many other genera are regarded as synonyms of Entolium by Ros et al. 2014
  bival[which(bival$genus=="Claraia (Pteroclaraia)"),"genus"]<-c("Claraia")
  bival[which(bival$genus=="Clisocolus (Clisocolus)"),"genus"]<-c("Clisocolus")
  bival[which(bival$genus=="Coelopis (Coelopis)"),"genus"]<-c("Coelopis")
  bival[which(bival$genus=="Corbula (Corbula)"),"genus"]<-c("Corbula")
  bival[which(bival$genus=="Corbula (Varicorbula)"),"genus"]<-c("Varicorbula")
  bival[which(bival$genus=="Cosmomya (Palaeocosmomya)"),"genus"]<-c("Cosmomya")
  bival[which(bival$genus=="Crassatella (Landinia)"),"genus"]<-c("Crassatella")
  bival[which(bival$genus=="Crassatella (Pachythaerus)"),"genus"]<-c("Pachythaerus") #Regarded as a genus following Komatsu 2013
  bival[which(bival$genus=="Cucullaea (Cucullastis)"),"genus"]<-c("Cucullaea")
  bival[which(bival$genus=="Cucullaea (Cyphoxis)"),"genus"]<-c("Cucullaea")
  bival[which(bival$genus=="Cucullaea (Idonearca)"),"genus"]<-c("Cucullaea")
  bival[which(bival$genus=="Cuspidaria (Cuspidaria)"),"genus"]<-c("Cuspidaria")
  bival[which(bival$genus=="Cuspidaria (Dianocuspidaria)"),"genus"]<-c("Cuspidaria")
  bival[which(bival$genus=="Daonella (Arzelella)"),"genus"]<-c("Daonella")
  bival[which(bival$genus=="Daonella (Longidaonella)"),"genus"]<-c("Daonella")
  bival[which(bival$genus=="Daonella (Pichlerella)"),"genus"]<-c("Daonella")
  bival[which(bival$genus=="Deltoideum (Boreiodeltoideum)"),"genus"]<-c("Deltoideum")
  bival[which(bival$genus=="Deltoideum (Deltoideum)"),"genus"]<-c("Deltoideum")
  bival[which(bival$genus=="Divalinga (Stchepinskya)"),"genus"]<-c("Divalinga")
  bival[which(bival$genus=="Entolium (Costentolium)"),"genus"]<-c("Entolium")
  bival[which(bival$genus=="Entolium (Entolium)"),"genus"]<-c("Entolium")
  bival[which(bival$genus=="Eocallista (Eocallista)"),"genus"]<-c("Eocallista")
  bival[which(bival$genus=="Eriphyla (Eriphyla)"),"genus"]<-c("Eriphyla")
  bival[which(bival$genus=="Exogyra (Aetostreon)"),"genus"]<-c("Aetostreon")
  bival[which(bival$genus=="Exogyra (Exogyra)"),"genus"]<-c("Exogyra")
  bival[which(bival$genus=="Fatina (Avia)"),"genus"]<-c("Fatina") #This genus may be a synonym of Sokolowia; it is a Cretaceous genus and will be discarded
  bival[which(bival$genus=="Gari (Psammobia)"),"genus"]<-c("Gari")
  bival[which(bival$genus=="Gastrochaena (Spengleria)"),"genus"]<-c("Spengleria")
  bival[which(bival$genus=="Gervillaria (Platygervillaria)"),"genus"]<-c("Gervillaria")
  bival[which(bival$genus=="Gervilleia (Angustella)"),"genus"]<-c("Gervillia")
  bival[which(bival$genus=="Gervilleioperna (Gervilleignoma)"),"genus"]<-c("Gervilleioperna") #"Gervilleignoma" (misspelling) should be "Gervilleiognoma"
  bival[which(bival$genus=="Gervillia (Angustella)"),"genus"]<-c("Gervillia")
  bival[which(bival$genus=="Gervillia (Cultriopsis)"),"genus"]<-c("Gervillia")
  bival[which(bival$genus=="Gervillia (Gervillia)"),"genus"]<-c("Gervillia")
  bival[which(bival$genus=="Gervillia (Odontoperna)"),"genus"]<-c("Bakevellia") #Following Treatise
  bival[which(bival$genus=="Glans (Centrocardita)"),"genus"]<-c("Centrocardita")
  bival[which(bival$genus=="Glycymeris (Glycymeris)"),"genus"]<-c("Glycymeris")
  bival[which(bival$genus=="Glycymeris (Hanaia)"),"genus"]<-c("Glycymeris")
  bival[which(bival$genus=="Glycymeris (Pseudoveletuceta)"),"genus"]<-c("Glycymeris")
  bival[which(bival$genus=="Glyptoactis (Fasciculicardia)"),"genus"]<-c("Fasciculicardia")
  bival[which(bival$genus=="Glyptoleda (Menucula)"),"genus"]<-c("Veteranella") #Following Fang 2009
  bival[which(bival$genus=="Goniomya (Goniomya)"),"genus"]<-c("Goniomya")
  bival[which(bival$genus=="Grammatodon (Cosmetodon)"),"genus"]<-c("Grammatodon") #Following Ros et al. 2014
  bival[which(bival$genus=="Grammatodon (Grammatodon)"),"genus"]<-c("Grammatodon")
  bival[which(bival$genus=="Grammatodon (Indogrammatodon)"),"genus"]<-c("Grammatodon")
  bival[which(bival$genus=="Grammatodon (Nordenskjoldia)"),"genus"]<-c("Nordenskjoldia")
  bival[which(bival$genus=="Gryphaea (Bilobissa)"),"genus"]<-c("Gryphaea")
  bival[which(bival$genus=="Gryphaea (Catinula)"),"genus"]<-c("Gryphaea")
  bival[which(bival$genus=="Gryphaea (Gryphaea)"),"genus"]<-c("Gryphaea")
  bival[which(bival$genus=="Gyrostrea (Vatonnei)"),"genus"]<-c("Gyrostrea")
  bival[which(bival$genus=="Halobia (Enormihalobia)"),"genus"]<-c("Antijanira") #Following Fang 2009
  bival[which(bival$genus=="Halobia (Parahalobia)"),"genus"]<-c("Halobia")
  bival[which(bival$genus=="Halobia (Zittelihalobia)"),"genus"]<-c("Halobia")
  bival[which(bival$genus=="Hoernesia (Strophopteria)"),"genus"]<-c("Hoernesia")
  bival[which(bival$genus=="Inoceramus (Cordiceramus)"),"genus"]<-c("Cordiceramus")
  bival[which(bival$genus=="Inoceramus (Endocostea)"),"genus"]<-c("Endocostea")
  bival[which(bival$genus=="Inoceramus (Heroceramus)"),"genus"]<-c("Heroceramus")
  bival[which(bival$genus=="Inoceramus (Inoceramus)"),"genus"]<-c("Inoceramus")
  bival[which(bival$genus=="Inoceramus (Platyceramus)"),"genus"]<-c("Platyceramus")
  bival[which(bival$genus=="Inoceramus (Sphenoceramus)"),"genus"]<-c("Sphenoceramus")
  bival[which(bival$genus=="Inoceramus (Volviceramus)"),"genus"]<-c("Volviceramus")
  bival[which(bival$genus=="Inoperna (Inoperna)"),"genus"]<-c("Inoperna")
  bival[which(bival$genus=="Inoperna (Triasoperna)"),"genus"]<-c("Inoperna")
  bival[which(bival$genus=="Isocyprina (Eotrapezium)"),"genus"]<-c("Isocyprina")
  bival[which(bival$genus=="Isocyprina (Isocyprina)"),"genus"]<-c("Isocyprina")
  bival[which(bival$genus=="Isognomon (Iranognomon)"),"genus"]<-c("Isognomon")
  bival[which(bival$genus=="Isognomon (Isognomon)"),"genus"]<-c("Isognomon")
  bival[which(bival$genus=="Isognomon (Mulletia)"),"genus"]<-c("Mulletia")
  bival[which(bival$genus=="Isognomon (Mytiloperna)"),"genus"]<-c("Mytiloperna") #Mytiloperna is treated as an independent genus following recent papers
  bival[which(bival$genus=="Isognomon (Semignomon)"),"genus"]<-c("Isognomon")
  bival[which(bival$genus=="Kaibabella (Flattopia)"),"genus"]<-c("Kaibabella")
  bival[which(bival$genus=="Kellyella (Lutetia)"),"genus"]<-c("Lutetia")
  bival[which(bival$genus=="Laevitrigonia (Malagasitrigonia)"),"genus"]<-c("Malagasitrigonia")
  bival[which(bival$genus=="Lamprotula (Eolamprotula)"),"genus"]<-c("Eolamprotula")
  bival[which(bival$genus=="Leda (Lembulus)"),"genus"]<-c("Lembulus")
  bival[which(bival$genus=="Leptodesma (Leiopteria)"),"genus"]<-c("Leptodesma")
  bival[which(bival$genus=="Leptodesma (Leptodesma)"),"genus"]<-c("Leptodesma")
  bival[which(bival$genus=="Leptodesma (Springeria)"),"genus"]<-c("Leptodesma")
  bival[which(bival$genus=="Lima (Lima)"),"genus"]<-c("Lima")
  bival[which(bival$genus=="Lima (Mantellum)"),"genus"]<-c("Lima")
  bival[which(bival$genus=="Lima (Regalilima)"),"genus"]<-c("Regalilima") #Following Treatise
  bival[which(bival$genus=="Lima (Tirolidia)"),"genus"]<-c("Tirolidia")
  bival[which(bival$genus=="Limea (Eolimea)"),"genus"]<-c("Limea")
  bival[which(bival$genus=="Limopsis (Limopsis)"),"genus"]<-c("Limopsis")
  bival[which(bival$genus=="Linotrigonia (Oistotrigonia)"),"genus"]<-c("Oistotrigonia")
  bival[which(bival$genus=="Liopistha (Psilomya)"),"genus"]<-c("Liopistha")
  bival[which(bival$genus=="Lithophaga (Leiosolenus)"),"genus"]<-c("Lithophaga") #The Triassic record should be Lithophaga
  bival[which(bival$genus=="Lithophaga (Lithophaga)"),"genus"]<-c("Lithophaga")
  bival[which(bival$genus=="Lopha (Actinostreon)"),"genus"]<-c("Actinostreon")
  bival[which(bival$genus=="Lopha (Arctostrea)"),"genus"]<-c("Arctostrea")
  bival[which(bival$genus=="Lopha (Lopha)"),"genus"]<-c("Lopha")
  bival[which(bival$genus=="Loripes (Loripes)"),"genus"]<-c("Loripes")
  bival[which(bival$genus=="Meekia (Meekia)"),"genus"]<-c("Meekia")
  bival[which(bival$genus=="Meekia (Mygallia)"),"genus"]<-c("Meekia")
  bival[which(bival$genus=="Megadesmus (Cleobis)"),"genus"]<-c("Megadesmus")
  bival[which(bival$genus=="Megatrigonia (Apiotrigonia)"),"genus"]<-c("Apiotrigonia")
  bival[which(bival$genus=="Megatrigonia (Iotrigonia)"),"genus"]<-c("Iotrigonia")
  bival[which(bival$genus=="Megayoldia (Portlandella)"),"genus"]<-c("Portlandia") #Following Treatise, but this genus will be discarded
  bival[which(bival$genus=="Mesocallista (Mesocallista)"),"genus"]<-c("Mesocallista")
  bival[which(bival$genus=="Modiola (Lithophagus)"),"genus"]<-c("Lithophaga") #Following Treatise
  bival[which(bival$genus=="Modiola (Septiola)"),"genus"]<-c("Septiola")
  bival[which(bival$genus=="Modiolus (Cyranus)"),"genus"]<-c("Modiolus")
  bival[which(bival$genus=="Modiolus (Inoperna)"),"genus"]<-c("Inoperna")
  bival[which(bival$genus=="Modiolus (Modiolus)"),"genus"]<-c("Modiolus")
  bival[which(bival$genus=="Monotis (Entomonotis)"),"genus"]<-c("Monotis")
  bival[which(bival$genus=="Monotis (Eomonotis)"),"genus"]<-c("Monotis") #Following Ros et al. 2014
  bival[which(bival$genus=="Monotis (Inflatomonotis)"),"genus"]<-c("Monotis")
  bival[which(bival$genus=="Monotis (Maorimonotis)"),"genus"]<-c("Monotis")
  bival[which(bival$genus=="Monotis (Monotis)"),"genus"]<-c("Monotis")
  bival[which(bival$genus=="Monotis (Pacimonotis)"),"genus"]<-c("Monotis")
  bival[which(bival$genus=="Musculus (Musculus)"),"genus"]<-c("Musculus")
  bival[which(bival$genus=="Myalina (Myalina)"),"genus"]<-c("Myalina")
  bival[which(bival$genus=="Myalina (Myalinella)"),"genus"]<-c("Myalinella")
  bival[which(bival$genus=="Myofossa (Myofossa)"),"genus"]<-c("Myofossa")
  bival[which(bival$genus=="Myofossa (Ragozinia)"),"genus"]<-c("Myofossa")
  bival[which(bival$genus=="Myonia (Myonia)"),"genus"]<-c("Myonia")
  bival[which(bival$genus=="Myonia (Pachymyonia)"),"genus"]<-c("Pachymyonia")
  bival[which(bival$genus=="Myophorella (Clavitrigonia)"),"genus"]<-c("Myophorella")
  bival[which(bival$genus=="Myophorella (Clavotrigonia)"),"genus"]<-c("Myophorella")
  bival[which(bival$genus=="Myophorella (Haidaia)"),"genus"]<-c("Myophorella")
  bival[which(bival$genus=="Myophorella (Myophorella)"),"genus"]<-c("Myophorella")
  bival[which(bival$genus=="Myophorella (Orthotrigonia)"),"genus"]<-c("Orthotrigonia") #Following Yanin and Bogdanova 2017
  bival[which(bival$genus=="Myophorella (Scaphogonia)"),"genus"]<-c("Myophorella")
  bival[which(bival$genus=="Myophorella (Scaphotrigonia)"),"genus"]<-c("Scaphotrigonia")
  bival[which(bival$genus=="Myophoria (Leviconcha)"),"genus"]<-c("Neoschizodus") #Leviconcha is regarded as a subgenus of Neoschizodus following Ros et al. 2014
  bival[which(bival$genus=="Myrtea (Myrtea)"),"genus"]<-c("Myrtea")
  bival[which(bival$genus=="Mysidioptera (Pseudacesta)"),"genus"]<-c("Mysidioptera")
  bival[which(bival$genus=="Mytilaster (Persiaster)"),"genus"]<-c("Mytilaster")
  bival[which(bival$genus=="Mytilus (Chloromya)"),"genus"]<-c("Perna") #Following Treatise
  bival[which(bival$genus=="Mytilus (Falcimytilus)"),"genus"]<-c("Falcimytilus")
  bival[which(bival$genus=="Mytilus (Modiola)"),"genus"]<-c("Modiolus")
  bival[which(bival$genus=="Mytilus (Mytilus)"),"genus"]<-c("Mytilus")
  bival[which(bival$genus=="Mytilus (Pharomytilus)"),"genus"]<-c("Inoperna") #Following Treatise
  bival[which(bival$genus=="Nanogyra (Nanogyra)"),"genus"]<-c("Nanogyra")
  bival[which(bival$genus=="Nanogyra (Palaeogyra)"),"genus"]<-c("Nanogyra")
  bival[which(bival$genus=="Neithea (Neithea)"),"genus"]<-c("Neithea")
  bival[which(bival$genus=="Neithea (Neithella)"),"genus"]<-c("Neithea")
  bival[which(bival$genus=="Nemocardium (Brevicardium)"),"genus"]<-c("Brevicardium")
  bival[which(bival$genus=="Nemocardium (Nemocardium)"),"genus"]<-c("Nemocardium")
  bival[which(bival$genus=="Nemocardium (Pratulum)"),"genus"]<-c("Nemocardium")
  bival[which(bival$genus=="Nemodon (Nemodon)"),"genus"]<-c("Nemodon")
  bival[which(bival$genus=="Neocrassina (Coelastarte)"),"genus"]<-c("Coelastarte")
  bival[which(bival$genus=="Neomegalodon (Gemmellarodus)"),"genus"]<-c("Neomegalodon")
  bival[which(bival$genus=="Neomegalodon (Neomegalodon)"),"genus"]<-c("Neomegalodon")
  bival[which(bival$genus=="Neomegalodon (Rossiodus)"),"genus"]<-c("Neomegalodon")
  bival[which(bival$genus=="Neoschizodus (Okunominetania)"),"genus"]<-c("Neoschizodus")
  bival[which(bival$genus=="Nevenulora (Jagonoma)"),"genus"]<-c("Jagonoma")
  bival[which(bival$genus=="Newaagia (Latinewaagia)"),"genus"]<-c("Newaagia")
  bival[which(bival$genus=="Nicaniella (Nicaniella)"),"genus"]<-c("Nicaniella")
  bival[which(bival$genus=="Notodonax (Aliodonax)"),"genus"]<-c("Notodonax")
  bival[which(bival$genus=="Nucula (Lamellinucula)"),"genus"]<-c("Nucula")
  bival[which(bival$genus=="Nucula (Nucula)"),"genus"]<-c("Nucula")
  bival[which(bival$genus=="Nuculana (Dacryomya)"),"genus"]<-c("Dacryomya")
  bival[which(bival$genus=="Nuculana (Nuculana)"),"genus"]<-c("Nuculana")
  bival[which(bival$genus=="Nuculana (Praesaccella)"),"genus"]<-c("Praesaccella")
  bival[which(bival$genus=="Nuculana (Thestyleda)"),"genus"]<-c("Nuculana")
  bival[which(bival$genus=="Nuculites (Cleidophorus)"),"genus"]<-c("Nuculites")
  bival[which(bival$genus=="Nuculopsis (Nuculopsis)"),"genus"]<-c("Nuculopsis")
  bival[which(bival$genus=="Opis (Coelopis)"),"genus"]<-c("Coelopis")
  bival[which(bival$genus=="Opis (Hesperopis)"),"genus"]<-c("Opis")
  bival[which(bival$genus=="Opis (Trigonopis)"),"genus"]<-c("Opis")
  bival[which(bival$genus=="Osteomya (Yunnanomya)"),"genus"]<-c("Homomya") #Following Fang 2009
  bival[which(bival$genus=="Ostrea (Alectryonia)"),"genus"]<-c("Lopha") #Alectryonia = Lopha (see Treatise)
  bival[which(bival$genus=="Ostrea (Catinula)"),"genus"]<-c("Catinula")
  bival[which(bival$genus=="Ostrea (Liostrea)"),"genus"]<-c("Liostrea")
  bival[which(bival$genus=="Ostrea (Ostrea)"),"genus"]<-c("Ostrea")
  bival[which(bival$genus=="Oxytoma (Oxytoma)"),"genus"]<-c("Oxytoma")
  bival[which(bival$genus=="Oxytoma (Palmoxytoma)"),"genus"]<-c("Palmoxytoma")
  bival[which(bival$genus=="Pachymya (Pachymya)"),"genus"]<-c("Pachymya")
  bival[which(bival$genus=="Pachyrisma (Durga)"),"genus"]<-c("Pachyrisma")
  bival[which(bival$genus=="Pachyrisma (Pachymegalodon)"),"genus"]<-c("Pachyrisma")
  bival[which(bival$genus=="Palaeopharus (Minepharus)"),"genus"]<-c("Minepharus")
  bival[which(bival$genus=="Panopea (Myopsis)"),"genus"]<-c("Panopea")
  bival[which(bival$genus=="Panopea (Panopea)"),"genus"]<-c("Panopea")
  bival[which(bival$genus=="Parallelodon (Gilbertwhitea)"),"genus"]<-c("Nemodon") #Following Treatise
  bival[which(bival$genus=="Pecten (Janira)"),"genus"]<-c("Pecten")
  bival[which(bival$genus=="Pecten (Pecten)"),"genus"]<-c("Pecten")
  bival[which(bival$genus=="Pecten (Plesiopecten)"),"genus"]<-c("Spondylopecten") #Following Treatise
  bival[which(bival$genus=="Pecten (Propeamusium)"),"genus"]<-c("Propeamusium")
  bival[which(bival$genus=="Pecten (Syncyclonema)"),"genus"]<-c("Syncyclonema")
  bival[which(bival$genus=="Pecten (Vola)"),"genus"]<-c("Pecten")
  bival[which(bival$genus=="Periploma (Periploma)"),"genus"]<-c("Periploma")
  bival[which(bival$genus=="Perna (Perna)"),"genus"]<-c("Perna")
  bival[which(bival$genus=="Perugonia (Albitrigonia)"),"genus"]<-c("Perugonia")
  bival[which(bival$genus=="Pholadomya (Bucardiomya)"),"genus"]<-c("Pholadomya")
  bival[which(bival$genus=="Pholadomya (Pholadomya)"),"genus"]<-c("Pholadomya")
  bival[which(bival$genus=="Pinna (Pinna)"),"genus"]<-c("Pinna")
  bival[which(bival$genus=="Pinna (Sulcatopinna)"),"genus"]<-c("Pinna")
  bival[which(bival$genus=="Pitar (Lamelliconcha)"),"genus"]<-c("Pitar")
  bival[which(bival$genus=="Pleuriocardia (Dochmocardia)"),"genus"]<-c("Pleuriocardia")
  bival[which(bival$genus=="Pleuriocardia (Incacardium)"),"genus"]<-c("Pleuriocardia")
  bival[which(bival$genus=="Plicatula (Eoplicatula)"),"genus"]<-c("Eoplicatula")
  bival[which(bival$genus=="Plicatula (Harpax)"),"genus"]<-c("Harpax")
  bival[which(bival$genus=="Plicatula (Plicatula)"),"genus"]<-c("Plicatula")
  bival[which(bival$genus=="Plicatula (Pseudoplacunopsis)"),"genus"]<-c("Pseudoplacunopsis")
  bival[which(bival$genus=="Propeamussium (Parvamussium)"),"genus"]<-c("Parvamussium")
  bival[which(bival$genus=="Propeamussium (Propeamussium)"),"genus"]<-c("Propeamussium")
  bival[which(bival$genus=="Propeamussium (Striatoamussium)"),"genus"]<-c("Striatoamussium")
  bival[which(bival$genus=="Prothyris (Amphikoilum)"),"genus"]<-c("Prothyris")
  bival[which(bival$genus=="Prothyris (Lophoprothyris)"),"genus"]<-c("Prothyris")
  bival[which(bival$genus=="Protocardia (Grypocardia)"),"genus"]<-c("Protocardia")
  bival[which(bival$genus=="Protocardia (Leptocardia)"),"genus"]<-c("Protocardia")
  bival[which(bival$genus=="Protocardia (Pachycardium)"),"genus"]<-c("Protocardia")
  bival[which(bival$genus=="Protocardia (Protocardia)"),"genus"]<-c("Protocardia")
  bival[which(bival$genus=="Pseudomonotis (Aviculomonotis)"),"genus"]<-c("Pseudomonotis")
  bival[which(bival$genus=="Pseudomonotis (Prospondylus)"),"genus"]<-c("Prospondylus")
  bival[which(bival$genus=="Pseudomonotis (Pseudomonotis)"),"genus"]<-c("Pseudomonotis")
  bival[which(bival$genus=="Pseudomonotis (Trematiconcha)"),"genus"]<-c("Pseudomonotis")
  bival[which(bival$genus=="Pseudopecten (Pseudopecten)"),"genus"]<-c("Pseudopecten")
  bival[which(bival$genus=="Pterotrigonia (Acanthotrigonia)"),"genus"]<-c("Acanthotrigonia")
  bival[which(bival$genus=="Pterotrigonia (Pterotrigonia)"),"genus"]<-c("Pterotrigonia")
  bival[which(bival$genus=="Pterotrigonia (Ptilotrigonia)"),"genus"]<-c("Ptilotrigonia")
  bival[which(bival$genus=="Pterotrigonia (Scabrotrigonia)"),"genus"]<-c("Scabrotrigonia")
  bival[which(bival$genus=="Pycnodonte (Phygraea)"),"genus"]<-c("Phygraea")
  bival[which(bival$genus=="Pycnodonte (Pycnodonte)"),"genus"]<-c("Pycnodonte")
  bival[which(bival$genus=="Rastellum (Arctostrea)"),"genus"]<-c("Arctostrea")
  bival[which(bival$genus=="Saikraconcha (Dereconcha)"),"genus"]<-c("Saikraconcha")
  bival[which(bival$genus=="Sanoarca (Sanoarca)"),"genus"]<-c("Sanoarca")
  bival[which(bival$genus=="Seebachia (Eoseebachia)"),"genus"]<-c("Seebachia")
  bival[which(bival$genus=="Septifer (Septifer)"),"genus"]<-c("Septifer")
  bival[which(bival$genus=="Shikamaia (Alatoconcha)"),"genus"]<-c("Shikamaia")
  bival[which(bival$genus=="Solemya (Janeia)"),"genus"]<-c("Solemya")
  bival[which(bival$genus=="Solemya (Petrasma)"),"genus"]<-c("Solemya")
  bival[which(bival$genus=="Spondylus (Spondylus)"),"genus"]<-c("Spondylus")
  bival[which(bival$genus=="Tancredia (Tancredia)"),"genus"]<-c("Tancredia")
  bival[which(bival$genus=="Tellina (Tellinella)"),"genus"]<-c("Tellinella")
  bival[which(bival$genus=="Thracia (Thracia)"),"genus"]<-c("Thracia")
  bival[which(bival$genus=="Trachycardium (Trachycardium)"),"genus"]<-c("Trachycardium")
  bival[which(bival$genus=="Trigonia (Kumatrigonia)"),"genus"]<-c("Frenguelliella") #Following Ros et al. 2014
  bival[which(bival$genus=="Trigonia (Indotrigonia)"),"genus"]<-c("Indotrigonia")
  bival[which(bival$genus=="Trigonia (Pleurotrigonia)"),"genus"]<-c("Pleurotrigonia")
  bival[which(bival$genus=="Trigonia (Protrigonia)"),"genus"]<-c("Trigonia")
  bival[which(bival$genus=="Trigonia (Trigonia)"),"genus"]<-c("Trigonia")
  bival[which(bival$genus=="Trigonioides (Wakinoa)"),"genus"]<-c("Trigonioides")
  bival[which(bival$genus=="Trigonucula (Gonionucula)"),"genus"]<-c("Trigonucula")
  bival[which(bival$genus=="Tucetona (Bellaxinaea)"),"genus"]<-c("Tucetona")
  bival[which(bival$genus=="Vaugonia (Hijitrigonia)"),"genus"]<-c("Vaugonia") #Following Ros et al. 2014
  bival[which(bival$genus=="Vaugonia (Vaugonia)"),"genus"]<-c("Vaugonia")
  bival[which(bival$genus=="Venericardia (Pacificor)"),"genus"]<-c("Venericardia")
  bival[which(bival$genus=="Venericardia (Venericor)"),"genus"]<-c("Venericor")
  bival[which(bival$genus=="Venus (Venus)"),"genus"]<-c("Venus")
  bival[which(bival$genus=="Vesicomya (Calyptogena)"),"genus"]<-c("Calyptogena")
  bival[which(bival$genus=="Veteranella (Glyptoleda)"),"genus"]<-c("Veteranella") #Following Ros et al. 2014
  bival[which(bival$genus=="Veteranella (Ledoides)"),"genus"]<-c("Veteranella") #Following Ros et al. 2014
  bival[which(bival$genus=="Veteranella (Nucundata)"),"genus"]<-c("Veteranella") #Following Ros et al. 2014
  bival[which(bival$genus=="Weyla (Lywea)"),"genus"]<-c("Weyla")
  bival[which(bival$genus=="Weyla (Weyla)"),"genus"]<-c("Weyla")
  bival[which(bival$genus=="Willimactra (Petromactra)"),"genus"]<-c("Willimactra")
  bival[which(bival$genus=="Willimactra (Willimactra)"),"genus"]<-c("Willimactra")
  bival[which(bival$genus=="Allorisma"),"genus"]<-c("Edmondia") #Following Treatise
  bival[which(bival$genus=="Anatina"),"genus"]<-c("Laternula") #Following Ros et al. 2014
  bival[which(bival$primary_name=="Cercomya"),"genus"]<-c("Cercomya") #Some species of Cercomya were re-assigned to Anatina in PBDB
  bival[which(bival$genus=="Anodontophora"),"genus"]<-c("Unionites") #Following Treatise
  bival[which(bival$genus=="Anthraconeilo"),"genus"]<-c("Palaeoneilo") #Following Treatise
  bival[which(bival$genus=="Arcomya"),"genus"]<-c("Pachymya") #Arcomya is regarded as a subgenus of Pachymya following Ros et al. 2014
  bival[which(bival$genus=="Astartopis"),"genus"]<-c("Myophoriopis") #Following Ros et al. 2014
  bival[which(bival$occurrence_no=="1450039"),"genus"]<-c("Botulopsis") #See Ros et al. 2014
  bival[which(bival$genus=="Brachiodontes"),"genus"]<-c("Brachidontes")
  bival[which(bival$genus=="Bupecten"),"genus"]<-c("Entolium") #Following Fang et al. 2009
  bival[which(bival$genus=="Catinula"),"genus"]<-c("Gryphaea") #A subgenus of Gryphaea
  bival[which(bival$genus=="Chuluaria"),"genus"]<-c("Claraia") #Following Ros et al. 2014
  bival[which(bival$genus=="Cingentolium"),"genus"]<-c("Entolium") #Following Ros et al. 2014
  bival[which(bival$genus=="Claraioides"),"genus"]<-c("Claraia") #Following Ros et al. 2014
  bival[which(bival$genus=="Corbicella"),"genus"]<-c("Quenstedtia") #Following Treatise
  bival[which(bival$genus=="Costentolium"),"genus"]<-c("Entolium") #Following Ros et al. 2014
  bival[which(bival$genus=="Calvaentolium"),"genus"]<-c("Entolium") #Following Ros et al. 2014
  bival[which(bival$genus=="Ctenolima"),"genus"]<-c("Antiquilima") #A subgenus of Antiquilima
  bival[which(bival$genus=="Cultriopsis"),"genus"]<-c("Gervillia") #Following Ros et al. 2014
  bival[which(bival$genus=="Culunana"),"genus"]<-c("Phestia") #Following Treatise
  bival[which(bival$genus=="Cypricardia"),"genus"]<-c("Trapezium") #Following Treatise
  bival[which(bival$genus=="Cyrena"),"genus"]<-c("Corbicula") #Following Treatise
  bival[which(bival$genus=="Dacromya"),"genus"]<-c("Dacryomya")
  bival[which(bival$genus=="Dianchora"),"genus"]<-c("Spondylus") #Following Treatise
  bival[which(bival$genus=="Dimyodon"),"genus"]<-c("Atreta") #Following Ros et al. 2014
  bival[which(bival$genus=="Dipleurites"),"genus"]<-c("Daonella") #Following Ros et al. 2014
  bival[which(bival$genus=="Ensio"),"genus"]<-c("Gonilia") #Following Ros et al. 2014
  bival[which(bival$genus=="Eodiceras"),"genus"]<-c("Epidiceras") #Skelton et al. 2013
  bival[which(bival$genus=="Eomonotis"),"genus"]<-c("Monotis") #Following Ros et al. 2014
  bival[which(bival$genus=="Enosolen"),"genus"]<-c("Tulongella") #Following Ros et al. 2014
  bival[which(bival$genus=="Epiclaraia"),"genus"]<-c("Claraia") #Following Ros et al. 2014
  bival[which(bival$primary_name=="Neomorphotis"),"genus"]<-c("Neomorphotis") #This genus is regarded as a synonym of Eumorphotis in PBDB; it is treated as an independent genus following Ros et al. 2014
  bival[which(bival$genus=="Filamussium"),"genus"]<-c("Parvamussium") #Following Ros et al. 2014
  bival[which(bival$primary_name=="Filopecten"),"genus"]<-c("Filopecten") #Following Ros et al. 2014
  bival[which(bival$genus=="Fogiella"),"genus"]<-c("Pleuromya") #Following Treatise
  bival[which(bival$genus=="Gemmelarodus"),"genus"]<-c("Neomegalodon") #The name should be "Gemmellarodus", a subgenus or synonym of Neomegalodon
  bival[which(bival$genus=="Girtyana"),"genus"]<-c("Phestia") #Following Treatise
  bival[which(bival$genus=="Globicarina"),"genus"]<-c("Megadesmus") #Runnegar 1969
  bival[which(bival$primary_name=="Granulochlamys"),"genus"]<-c("Chlamys") #Following Ros et al. 2014
  bival[which(bival$genus=="Guangdongella"),"genus"]<-c("Jiangxiella") #Following Fang 2009
  bival[which(bival$genus=="Guichiella"),"genus"]<-c("Claraia") #Following Ros et al. 2014
  bival[which(bival$genus=="Gythemon"),"genus"]<-c("Pronoella") #Gythemon is regarded as a subgenus of Pronoella following Treatise
  bival[which(bival$genus=="Heligmus"),"genus"]<-c("Eligmus") #Following Treatise
  bival[which(bival$genus=="Pseudosaxicava"),"genus"]<-c("Hiatella") #Pseudosaxicava is regarded as a subgenus of Hiatella following Schneider 2012
  bival[which(bival$genus=="Hemimenia"),"genus"]<-c("Hemipelex") #The latter is the new name of the former because the former is pre-occupied
  bival[which(bival$genus=="Hinganodon"),"genus"]<-c("Palaeomutela") #Following Fang 2009; Palaeomutela is a nonmarine genus
  bival[which(bival$genus=="Homoya"),"genus"]<-c("Homomya")
  bival[which(bival$genus=="Hunanonectes"),"genus"]<-c("Radulonectites") #Following Ros et al. 2014
  bival[which(bival$genus=="Hypotrema"),"genus"]<-c("Pulvinites") #Hypotrema is regarded as a subgenus of the latter
  bival[which(bival$genus=="Megadiceras"),"genus"]<-c("Epidiceras") #Skelton et al. 2013
  bival[which(bival$genus=="Mytiloceramus"),"genus"]<-c("Inoceramus") #Following Treatise
  bival[which(bival$genus=="Isodonta"),"genus"]<-c("Sowerbya") #Following Treatise
  bival[which(bival$genus=="Janeia"),"genus"]<-c("Solemya")
  bival[which(bival$primary_name=="Eolimea"),"genus"]<-c("Eolimea") #Eolimea is regarded as an independent genus following recent papers
  bival[which(bival$primary_name=="Pseudolimea"),"genus"]<-c("Pseudolimea")  #Pseudolimea is regarded as an independent genus following recent papers
  bival[which(bival$genus=="Linearis"),"genus"]<-c("Linearia")
  bival[which(bival$genus=="Lupherella"),"genus"]<-c("Otapiria") #Lupherella is regarded as a subgenus of Otapiria; see Ros et al. 2014
  bival[which(bival$genus=="Lyapinella"),"genus"]<-c("Eriphyla") #Lyapinella is regarded as a subgenus of Eriphyla as originally suggested by Zakharov
  bival[which(bival$genus=="Macrodiceras"),"genus"]<-c("Diceras") #Following Macrodiceras 2015
  bival[which(bival$genus=="Magnolobia"),"genus"]<-c("Daonella") #Following Ros et al. 2014
  bival[which(bival$genus=="Mclearnia"),"genus"]<-c("Camptonectes") #Following Ros et al. 2014
  bival[which(bival$genus=="Megalodus"),"genus"]<-c("Megalodon")
  bival[which(bival$genus=="Middalya"),"genus"]<-c("Neoschizodus") #This genus was treated as a synonym of Neoschizodus. But the name in Treatise "Middalaya" was incorrect
  bival[which(bival$genus=="Modesticoncha"),"genus"]<-c("Trigonia") #Regarded as a subgenus following Ros et al. 2014
  bival[which(bival$genus=="Neoentolium"),"genus"]<-c("Entolium") #Following Ros et al. 2014
  bival[which(bival$genus=="Notomya"),"genus"]<-c("Pyramus") #Following Treatise
  bival[which(bival$genus=="Nuculanella"),"genus"]<-c("Nuculopsis") #Following Treatise
  bival[which(bival$genus=="Orthotrigonia"),"genus"]<-c("Vaugonia") #Following Ros et al. 2014, Orthotrigonia is regarded as a subgenus of Vaugonia
  bival[which(bival$genus=="Oxyloma"),"genus"]<-c("Oxyeurax") #The latter is the new name of the former because the former is pre-occupied
  bival[which(bival$genus=="Praeotapiria"),"genus"]<-c("Otapiria") #Following Ros et al. 2014
  bival[which(bival$genus=="Pachydomus"),"genus"]<-c("Megadesmus") #Following Treatise
  bival[which(bival$genus=="Paradiceras"),"genus"]<-c("Epidiceras") #Skelton et al. 2013
  bival[which(bival$genus=="Parahalobia"),"genus"]<-c("Halobia") #Following Ros et al. 2014
  bival[which(bival$genus=="Peribositra"),"genus"]<-c("Claraia") #Following Ros et al. 2014
  bival[which(bival$genus=="Permoperna"),"genus"]<-c("Waagenoperna") #Following Ros et al. 2014
  bival[which(bival$genus=="Polidevcia"),"genus"]<-c("Phestia") #Following Ros et al. 2014
  bival[which(bival$genus=="Platymya"),"genus"]<-c("Platymyoidea")
  bival[which(bival$genus=="Praechlamys"),"genus"]<-c("Chlamys") #Following Ros et al. 2014
  bival[which(bival$genus=="Praeotapiria"),"genus"]<-c("Otapiria") #Following Ros et al. 2014
  bival[which(bival$genus=="Propeamusium"),"genus"]<-c("Propeamussium")
  bival[which(bival$genus=="Pseudomyoconcha"),"genus"]<-c("Myoconcha") #Following Kaim and Schneider 2012
  bival[which(bival$genus=="Pteroclaraia"),"genus"]<-c("Claraia") #Following Ros et al. 2014
  bival[which(bival$genus=="Pterohalobia"),"genus"]<-c("Daonella") #Following Fang 2009
  bival[which(bival$genus=="Pteroperna"),"genus"]<-c("Pteria") #Following Ros et al. 2014
  bival[which(bival$genus=="Pteropiria"),"genus"]<-c("Otapiria") #Following Ros et al. 2014
  bival[which(bival$genus=="Rhynchopterus"),"genus"]<-c("Pteria") #Following Ros et al. 2014
  bival[which(bival$genus=="Rostroperna"),"genus"]<-c("Isognomon") #A subgenus
  bival[which(bival$genus=="Rugiclaraia"),"genus"]<-c("Claraia") #Following Ros et al. 2014
  bival[which(bival$genus=="Sichuantrigonia"),"genus"]<-c("Acanomyphoria") #Following Fang 2009
  bival[which(bival$genus=="Solonodon"),"genus"]<-c("Palaeomutela") #Following Fang 2009
  bival[which(bival$genus=="Streblopterinella"),"genus"]<-c("Streblopteria") #Streblopterinella is a nomen nudum; the species assigned to it were included in Streblopteria by Ros et al. 2014
  bival[which(bival$genus=="Thetironia"),"genus"]<-c("Thetis") #Following Treatise
  bival[which(bival$genus=="Trigonopis"),"genus"]<-c("Opis") #Following Ros et al. 2014
  bival[which(bival$genus=="Venericyprina"),"genus"]<-c("Isocyprina") #Venericyprina is a subgenus of Isocyprina
  bival[which(bival$genus=="Verchanogrammysia"),"genus"]<-c("Verchojanogrammysia") #The name in PBDB is wrong
  bival[which(bival$genus=="Volsella"),"genus"]<-c("Modiolus") #Treatise
  bival[which(bival$genus=="Yokoyamaina"),"genus"]<-c("Integricardium") #Following Ros et al. 2014
  bival[which(bival$genus=="Xinanopecten"),"genus"]<-c("Streblochondria") #See Ros et al. 2014


  bival[which(bival$genus=="Posidonia"&bival$max_ma<=GTS2020[which(GTS2020$Interval=="Permian"),"LAD"]),"genus"]<-c("Bositra") #The Triassic and Jurassic records of Posidonia are re-assigned to Bositra
  bival[which(bival$genus=="Propeamussium"&bival$min_ma>=GTS2020[which(GTS2020$Interval=="Cretaceous"),"FAD"]),"genus"]<-c("Parvamussium") #Following Ros et al. 2014



  #####Changing the generic assignment of some species
  bival[which(bival$accepted_name=="Camptonectes (Camptochlamys) zitteli"),"genus"]<-c("Entolioides") #This genus is the type species of Entolioides, but was reassigned to Camptonectes (Camptochlamys) in PBDB
  bival[which(bival$identified_name=="Enantiostreon flabellum"),"genus"]<-c("Pseudoplacunopsis") #See Ros et al. 2014
  bival[which(bival$identified_name=="Enantiostreon spondyloides"),"genus"]<-c("Umbrostrea") #See Ros et al. 2014
  bival[which(bival$identified_name=="Enantiostreon sponyloides"),"genus"]<-c("Umbrostrea") #See Ros et al. 2014
  bival[which(bival$identified_name=="Enantiostreon cristadifforme"),"genus"]<-c("Umbrostrea") #See Ros et al. 2014
  bival[which(bival$identified_name=="Pecten schlosseri"),"genus"]<-c("Filopecten") #See Ros et al. 2014
  bival[which(bival$identified_name=="Pecten n. sp. azzarolae"),"genus"]<-c("Filopecten") #See Ros et al. 2014
  bival[which(bival$identified_name=="Pecten filosus"),"genus"]<-c("Filopecten") #See Ros et al. 2014
  bival[which(bival$accepted_name=="Inoceramus ussuriensis"),"genus"]<-c("Otapiria") #According to recent papers
  bival[which(bival$accepted_name=="Protopis timorensis"),"genus"]<-c("Joannina") #See Ros et al. 2014
  bival[which(bival$accepted_name=="Nuculana (Jupiteria) asiatica"),"genus"]<-c("Nuculana") #According to Dagys 1996
  bival[which(bival$identified_name=="Parainoceramus subtilis"),"genus"]<-c("Parainoceramya") #Following Ros et al. 2015
  bival[which(bival$identified_name=="Parainoceramus nitescens"),"genus"]<-c("Parainoceramya") #Following Ros et al. 2015
  bival[which(bival$identified_name=="Parainoceramus apollo"),"genus"]<-c("Parainoceramya") #Following Ros et al. 2015
  bival[which(bival$identified_name=="Parainoceramus substriatus"),"genus"]<-c("Parainoceramya") #Following Ros et al. 2015
  bival[which(bival$identified_name=="Laevitrigonia regina"),"genus"]<-c("Eselaevitrigonia") #According to recent papers
  bival[which(bival$identified_name=="Mysidioptera (Latemaria) inflata"),"genus"]<-c("Mysidioptera") #Mysidioptera inflata
  bival[which(bival$identified_name=="Lima (Pseudolimea) duplicata"),"genus"]<-c("Pseudolimea") #Pseudolimea is regarded as a subgenus of Limea by Ros et al. 2014, but many researchers regarded it as an independent genus
  bival[which(bival$identified_name=="Lima (Pseudolimea) boonei"),"genus"]<-c("Limatula") #Following Fleming 1978
  bival[which(bival$identified_name=="Loripes (Discoloripes) n. sp. gerasimovi"),"genus"]<-c("Discoloripes")
  bival[which(bival$identified_name=="Lyriomyophoria inflata"),"genus"]<-c("Elegantinia") #This species belongs to Elegantinia
  bival[which(bival$identified_name=="Mytilus eduliformis"),"genus"]<-c("Promysidiella")
  bival[which(bival$identified_name=="Mytilus eduliformis praecursor"),"genus"]<-c("Promysidiella")
  bival[which(bival$identified_name=="Astarte (Nicaniella) semicostata"),"genus"]<-c("Nicaniella")
  bival[which(bival$identified_name=="Astarte (Nicaniella) minor"),"genus"]<-c("Nicaniella")
  bival[which(bival$identified_name=="Astarte (Nicaniella) higoensis"),"genus"]<-c("Nicaniella")
  bival[which(bival$identified_name=="Astarte (Nicaniella) supraextensa"),"genus"]<-c("Nicaniella")
  bival[which(bival$identified_name=="Astarte (Nicaniella) sp."),"genus"]<-c("Nicaniella")
  bival[which(bival$identified_name=="Astarte (Nicaniella) extensa"),"genus"]<-c("Nicaniella")
  bival[which(bival$accepted_name=="Opis affinis"),"genus"]<-c("Coelopis")
  bival[which(bival$accepted_name=="Opis hoeninghausii"),"genus"]<-c("Coelopis")
  bival[which(bival$identified_name=="Nuculana sulcellata"),"genus"]<-c("Phestia") #See Ros et al. 2014
  bival[which(bival$identified_name=="Nucula n. sp. sulcellata"),"genus"]<-c("Phestia")
  bival[which(bival$identified_name=="Pecten kachhensis"),"genus"]<-c("Indoweyla") #Fürsich et al. 2019
  bival[which(bival$identified_name=="Phestia (Polidevcia) subperlonga"),"genus"]<-c("Nuculana") #This species was re-assigned to Nuculana by Komatsu 2010
  bival[which(bival$identified_name=="Pichleria polyglypha"),"genus"]<-c("Pichleria")
  bival[which(bival$accepted_name=="Placunopsis fissistriata"),"genus"]<-c("Pseudoplacunopsis")
  bival[which(bival$accepted_name=="Placunopsis plana"),"genus"]<-c("Pseudoplacunopsis")
  bival[which(bival$identified_name=="Placunopsis teruelensis"),"genus"]<-c("Pseudoplacunopsis")
  bival[which(bival$accepted_name=="Burmesia posteroradiata"),"genus"]<-c("Pseudoburmesia") #See Ros et al. 2014
  bival[which(bival$accepted_name=="Burmesia qinghaiensis"),"genus"]<-c("Pseudoburmesia") #See Ros et al. 2014
  bival[which(bival$accepted_name=="Pecten (Pecten) dentatus"),"genus"]<-c("Pseudopecten")
  bival[which(bival$accepted_name=="Quadratojaworskiella acarinata"),"genus"]<-c("Prosogyrotrigonia") #Following Echevarría et al. 2021
  bival[which(bival$accepted_name=="Nuculana doris"),"genus"]<-c("Ryderia")
  bival[which(bival$accepted_name=="Sphaera madagascariensis"),"genus"]<-c("Eosphaera")
  bival[which(bival$accepted_name=="Botula cassiana"),"genus"]<-c("Botulopsis") #see Ros et al. 2014

  #####Discarding some doubtful occurrences

  #Making a list of occurrences that will be discarded
  discard<-c()
  discard<-c(discard,which(bival$genus=="Amiodon")) #This is a nonmarine genus according to Fang 2009
  discard<-c(discard,which(bival$ref_author=="Wu"&bival$ref_pubyr=="1989")) #These taxa are mentioned but not illustrated by Wu (1989); ages of many taxa are inconsistent with those in other references
  discard<-c(discard,which(bival$genus=="Anadontella")) #This is a nonmarine genus according to Silantiev 2018
  discard<-c(discard,which(bival$occurrence_no=="771684"))  #Yang and Gao 2000 Aviculopecten cf. janus; True Aviculopecten janus was reassigned to Annuliconcha
  discard<-c(discard,which(bival$genus=="Botulopsis"&bival$ref_author=="Stiller")) #The Anisian records from China are problematic according to Ros et al. 2014
  discard<-c(discard,which(bival$genus=="Concinella")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Cowperesia")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Cuneopsis")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Eolamprotula")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Eosion")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Ferrazia")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Fengjiachongia")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Hunanella")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Indosinion")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Jiangxiella")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Liaoningia")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Lilingella")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Liopotomida")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Margaritifera")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Margaritifera (Palaeomargaritifera")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Margaritifera (Pseudunio)")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Mengyinaia")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Nippononaia")) #This is a nonmarine genus
  discard<-c(discard,which(bival$occurrence_no=="884227")) #Yao et al. 1980 Procrassatella ? cf. plana. True "plana" was re-assigned to Oriocrassatella
  discard<-c(discard,which(bival$genus=="Palaeomutela")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Peregrinoconcha")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Potomida (Palaeopotomida)")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Prilukiella")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Psilunio")) #This is a nonmarine genus
  discard<-c(discard,which(bival$occurrence_no=="545708")) #Only tentatively assigned to Regalilima
  discard<-c(discard,which(bival$occurrence_no=="600661")) #Astarte ? cf. sowerbyana. This record is only tentatively assigned to this species; true sowerbyana belongs to Seebachia
  discard<-c(discard,which(bival$genus=="Shifangella")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Solenoides")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Undulatula")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Unio")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Xinyuella")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Yananoconcha")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Yunnanophorus")) #This is a nonmarine genus
  discard<-c(discard,which(bival$genus=="Zhifangia")) #This is a nonmarine genus
  discard<-c(discard,which(bival$ref_author=="Lu"&bival$ref_pubyr=="1986")) #Ages of these collections (from the Bayinhe and Nuoyinhe groups) are highly uncertain.
  discard<-c(discard,which(bival$ref_author=="Buddington and Chapin"&bival$ref_pubyr=="1929")) #The taxa reported are poorly identified
  discard<-c(discard,which(bival$ref_author=="Xiao et al."&bival$ref_pubyr=="2022")) #The bivalves are from olistostromes
  bival<-bival[-discard,]



  #####Changing the ages of collections and formations

  bival[which(bival$formation=="Abbotsbury Ironstone"),"early_interval"]<-c("Cymodoce")
  bival[which(bival$formation=="Abbotsbury Ironstone"),"late_interval"]<-c("Cymodoce") #According to ammonoid zone

  bival[which(bival$formation=="Admiral"),"early_interval"]<-c("Early Artinskian")
  bival[which(bival$formation=="Admiral"),"late_interval"]<-c("Early Artinskian") #Lucas 2018

  bival[which(bival$formation=="Agoudim"&bival$late_interval=="Bathonian"),"late_interval"]<-c("Bajocian") #Ait Addi and Chafiki 2013

  bival[which(bival$formation=="Aiduna"),"early_interval"]<-c("Early Artinskian")
  bival[which(bival$formation=="Aiduna"),"late_interval"]<-c("Early Artinskian") #"Early Artinskian" in PBDB

  bival[which(bival$formation=="Akhura"&bival$early_interval=="Wuchiapingian"),"early_interval"]<-c("Late Wuchiapingian")
  bival[which(bival$formation=="Akhura"&bival$late_interval=="Wuchiapingian"),"late_interval"]<-c("Late Wuchiapingian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Alcobaca"),"early_interval"]<-c("Kimmeridgian")
  bival[which(bival$formation=="Alcobaca"),"late_interval"]<-c("Kimmeridgian")
  bival[which(bival$formation=="Alcoba莽a"),"early_interval"]<-c("Kimmeridgian")
  bival[which(bival$formation=="Alcoba莽a"),"late_interval"]<-c("Kimmeridgian") #Fürsich et al. 2022

  bival[which(bival$formation=="Almstrom Creek"&bival$early_interval=="Sinemurian"),"early_interval"]<-c("Late Sinemurian")
  bival[which(bival$formation=="Almstrom Creek"&bival$late_interval=="Sinemurian"),"late_interval"]<-c("Late Sinemurian") #According to the original reference

  bival[bival$collection_no%in%c(160644,160645,161031,161032,161107,161109,161110,160205,160647,160874,160875),"early_interval"]<-c("Aalensis")
  bival[bival$collection_no%in%c(160644,160645,161031,161032,161107,161109,161110,160205,160647,160874,160875),"late_interval"]<-c("Aalensis")
  bival[bival$collection_no%in%c(160808,160809),"early_interval"]<-c("Pseudoradiosa")
  bival[bival$collection_no%in%c(160808,160809),"late_interval"]<-c("Pseudoradiosa") #According to ammonoid Zone

  bival[bival$collection_no%in%c(224862,225391:225397,225401,225405,225406),"early_interval"]<-c("Margaritatus")
  bival[bival$collection_no%in%c(224862,225391:225397,225401,225405,225406),"late_interval"]<-c("Margaritatus")
  bival[bival$collection_no%in%c(225390,225398:225400,225402:225404),"early_interval"]<-c("Spinatum")
  bival[bival$collection_no%in%c(225390,225398:225400,225402:225404),"late_interval"]<-c("Spinatum") #According to ammonoid zone

  bival[which(bival$formation=="Amaral"),"early_interval"]<-c("Late Kimmeridgian")
  bival[which(bival$formation=="Amaral"),"late_interval"]<-c("Late Kimmeridgian") #Fürsich et al. 2022

  bival[which(bival$formation=="Ampthill Clay"),"early_interval"]<-c("Late Oxfordian")
  bival[which(bival$formation=="Ampthill Clay"),"late_interval"]<-c("Late Oxfordian") #Wright and Powell 2008

  bival[which(bival$formation=="Anyuan"),"early_interval"]<-c("Carnian")
  bival[which(bival$formation=="Anyuan"),"late_interval"]<-c("Norian")

  bival[which(bival$formation=="Argile de Villerville"),"early_interval"]<-c("Cautisnigrae")
  bival[which(bival$formation=="Argile de Villerville"),"late_interval"]<-c("Cautisnigrae") #According to ammonoid zone

  bival[which(bival$formation=="Argiles a Lopha gregarea"),"early_interval"]<-c("Plicatilis")
  bival[which(bival$formation=="Argiles a Lopha gregarea"),"late_interval"]<-c("Plicatilis") #According to ammonoid zone

  bival[bival$collection_no==198598,"early_interval"]<-c("Bifurcatus")
  bival[bival$collection_no==198598,"late_interval"]<-c("Bifurcatus")
  bival[bival$collection_no==198599,"early_interval"]<-c("Plicatilis")
  bival[bival$collection_no==198599,"late_interval"]<-c("Plicatilis") #According to ammonoid zone

  bival[bival$collection_no==47420,"early_interval"]<-c("Mutabilis")
  bival[bival$collection_no==47420,"late_interval"]<-c("Mutabilis") #According to ammonoid zone

  bival[which(bival$formation=="Asfal"),"early_interval"]<-c("Sevatian")
  bival[which(bival$formation=="Asfal"),"late_interval"]<-c("Sevatian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Aso"),"early_interval"]<-c("Tuvalian")
  bival[which(bival$formation=="Aso"),"late_interval"]<-c("Lacian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Assise a Gryphees"),"early_interval"]<-c("Aalensis")
  bival[which(bival$formation=="Assise a Gryphees"),"late_interval"]<-c("Aalensis") #According to ammonoid zone

  bival[which(bival$formation=="Assistance"),"early_interval"]<-c("Roadian")
  bival[which(bival$formation=="Assistance"),"late_interval"]<-c("Roadian") #Davydov et al. 2018

  bival[which(bival$formation=="Aston Limestone"),"early_interval"]<-c("Early Bajocian")
  bival[which(bival$formation=="Aston Limestone"),"late_interval"]<-c("Early Bajocian") #Barron et al. 1997

  bival[which(bival$formation=="Atkan"),"early_interval"]<-c("Early Capitanian")
  bival[which(bival$formation=="Atkan"),"late_interval"]<-c("Early Capitanian") #Biakov 2013

  bival[which(bival$formation=="Augusta Mountain"&bival$member=="lower"),"early_interval"]<-c("Longobardian")
  bival[which(bival$formation=="Augusta Mountain"&bival$member=="lower"),"late_interval"]<-c("Longobardian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Ayach'yaga"),"early_interval"]<-c("Late Kungurian")
  bival[which(bival$formation=="Ayach'yaga"),"late_interval"]<-c("Late Kungurian") #Kotlyar et al. 2018

  bival[which(bival$formation=="Baikur"),"early_interval"]<-c("Ufimian")
  bival[which(bival$formation=="Baikur"),"late_interval"]<-c("Roadian") #Wordian-Capitanian in PBDB, but Ufimian-Kazanian in Shishlova and Dubkova 2021

  bival[which(bival$formation=="Baitugan"),"early_interval"]<-c("Early Roadian")
  bival[which(bival$formation=="Baitugan"),"late_interval"]<-c("Early Roadian") #Lower Kazanian

  bival[which(bival$formation=="Baliqliq"&bival$early_interval=="Artinskian"),"early_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Baliqliq"&bival$late_interval=="Artinskian"),"late_interval"]<-c("Late Artinskian") #Chen and Shi 2003

  bival[which(bival$formation=="Banan"),"early_interval"]<-c("Tuvalian")
  bival[which(bival$formation=="Banan"),"late_interval"]<-c("Lacian") #Tong et al. 2021. Carnian in old references, but Norian in Tong et al. 2019, 2021

  bival[which(bival$formation=="Bap"),"early_interval"]<-c("Early Sakmarian")
  bival[which(bival$formation=="Bap"),"late_interval"]<-c("Early Sakmarian") #"Early Sakmarian" in PBDB

  bival[which(bival$formation=="Barabash"&bival$early_interval=="Wordian"),"early_interval"]<-c("Late Wordian")
  bival[which(bival$formation=="Barabash"&bival$late_interval=="Wordian"),"late_interval"]<-c("Late Wordian") #According to foram zone. Shi et al. 2022

  bival[bival$collection_no%in%c(210184,211278),"early_interval"]<-c("Margaritatus")
  bival[bival$collection_no%in%c(210184,211278),"late_interval"]<-c("Margaritatus")
  bival[bival$collection_no%in%c(211279,211285,211871),"early_interval"]<-c("Spinatum")
  bival[bival$collection_no%in%c(211279,211285,211871),"late_interval"]<-c("Spinatum")
  bival[bival$collection_no%in%c(211290,211292,211293,211295:211297,211872:211876,212055,212055:212069),"early_interval"]<-c("Tenuicostatum")
  bival[bival$collection_no%in%c(211290,211292,211293,211295:211297,211872:211876,212055,212055:212069),"late_interval"]<-c("Tenuicostatum")
  bival[bival$collection_no%in%c(211856:211869,212070,212071,212188:212191,212193,212197,212199,212200,212202,212203,212205,212206,212208,212212,212214,212216,212217),"early_interval"]<-c("Serpentinum")
  bival[bival$collection_no%in%c(211856:211869,212070,212071,212188:212191,212193,212197,212199,212200,212202,212203,212205,212206,212208,212212,212214,212216,212217),"late_interval"]<-c("Serpentinum")
  bival[bival$collection_no%in%c(211870,212218:212230),"early_interval"]<-c("Bifrons")
  bival[bival$collection_no%in%c(211870,212218:212230),"late_interval"]<-c("Bifrons") #According to ammonoid zone

  bival[which(bival$formation=="Barneston"),"early_interval"]<-c("Early Artinskian")
  bival[which(bival$formation=="Barneston"),"late_interval"]<-c("Early Artinskian") #According to Menning et al. 2006

  bival[which(bival$formation=="Bath Oolite"),"early_interval"]<-c("Retrocostatum")
  bival[which(bival$formation=="Bath Oolite"),"late_interval"]<-c("Retrocostatum") #Barron et al. 2012

  bival[which(bival$formation%in%c("Beattie","Beattie Limestone","Beattie Ls")),"early_interval"]<-c("Late Asselian")
  bival[which(bival$formation%in%c("Beattie","Beattie Limestone","Beattie Ls")),"late_interval"]<-c("Late Asselian") #Wahlman and West 2010

  bival[which(bival$formation=="Bechateur"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Bechateur"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Belebey"),"early_interval"]<-c("Roadian")
  bival[which(bival$formation=="Belebey"),"late_interval"]<-c("Roadian") #Kazanian

  bival[which(bival$formation=="Bell Canyon"&bival$member=="Lamar"),"early_interval"]<-c("Late Capitanian")
  bival[which(bival$formation=="Bell Canyon"&bival$member=="Lamar"),"late_interval"]<-c("Late Capitanian")
  bival[which(bival$formation=="Bell Canyon"&bival$member=="Rader"),"early_interval"]<-c("Early Capitanian")
  bival[which(bival$formation=="Bell Canyon"&bival$member=="Rader"),"late_interval"]<-c("Early Capitanian")
  bival[which(bival$formation=="Bell Canyon"&bival$member=="Hegler"),"early_interval"]<-c("Late Wordian")
  bival[which(bival$formation=="Bell Canyon"&bival$member=="Hegler"),"late_interval"]<-c("Late Wordian")
  bival[which(bival$formation=="Bell Canyon"&bival$member=="Pinery"&bival$early_interval=="Wordian"),"early_interval"]<-c("Late Wordian")
  bival[which(bival$formation=="Bell Canyon"&bival$member=="Pinery"&bival$early_interval=="Wordian"),"late_interval"]<-c("Late Wordian")
  bival[which(bival$formation=="Bell Canyon"&bival$member=="Pinery"&bival$early_interval=="Capitanian"),"early_interval"]<-c("Early Capitanian")
  bival[which(bival$formation=="Bell Canyon"&bival$member=="Pinery"&bival$early_interval=="Capitanian"),"late_interval"]<-c("Early Capitanian")
  bival[which(bival$formation=="Bell Canyon"&bival$early_interval=="Wordian"),"early_interval"]<-c("Late Wordian")
  bival[which(bival$formation=="Bell Canyon"&bival$late_interval=="Wordian"),"late_interval"]<-c("Late Wordian") #GTS2020

  bival[which(bival$formation=="Berry"),"early_interval"]<-c("Early Wordian")
  bival[which(bival$formation=="Berry"),"late_interval"]<-c("Early Wordian") #The age is early Wordian according to Shi et al. 2022

  bival[which(bival$formation=="Berkshire Oolite"),"early_interval"]<-c("Plicatilis")
  bival[which(bival$formation=="Berkshire Oolite"),"late_interval"]<-c("Plicatilis") #According to ammonoid zone

  bival[which(bival$formation=="Bhuj"&bival$member=="Umia"),"early_interval"]<-c("Late Tithonian")
  bival[which(bival$formation=="Bhuj"&bival$member=="Umia"),"late_interval"]<-c("Late Tithonian") #The other collection of the same horizon was given a late Tithonian age

  bival[which(bival$formation=="Birdlip Limestone"&bival$member=="Crickley"),"early_interval"]<-c("Murchisonae")
  bival[which(bival$formation=="Birdlip Limestone"&bival$member=="Crickley"),"late_interval"]<-c("Murchisonae")
  bival[which(bival$formation=="Birdlip Limestone"&bival$member=="Scottsquar"),"early_interval"]<-c("Bradfordensis")
  bival[which(bival$formation=="Birdlip Limestone"&bival$member=="Scottsquar"),"late_interval"]<-c("Bradfordensis") #According to ammonoid zone

  bival[which(bival$formation=="Birmensdorf Beds"),"early_interval"]<-c("Cordatum")
  bival[which(bival$formation=="Birmensdorf Beds"),"late_interval"]<-c("Transversarium") ##According to ammonoid zone

  bival[which(bival$formation=="Blaine"),"early_interval"]<-c("Roadian")
  bival[which(bival$formation=="Blaine"),"late_interval"]<-c("Early Wordian") #Kungurian in PBDB; Laurin and Hook 2022

  bival[which(bival$formation=="Blisworth Clay"),"early_interval"]<-c("Late Bathonian")
  bival[which(bival$formation=="Blisworth Clay"),"late_interval"]<-c("Late Bathonian") #Barron et al. 2012

  bival[which(bival$formation=="Blisworth Limestone"),"early_interval"]<-c("Subcontractus")
  bival[which(bival$formation=="Blisworth Limestone"),"late_interval"]<-c("Retrocostatum") #Barron et al. 2012

  bival[which(bival$formation=="Blue Anchor"),"early_interval"]<-c("Early Rhaetian")
  bival[which(bival$formation=="Blue Anchor"),"late_interval"]<-c("Early Rhaetian") #Following Flannery-Sutherland et al. 2022

  bival[bival$collection_no%in%c(29227:29230),"early_interval"]<-c("Planorbis")
  bival[bival$collection_no%in%c(29227:29230),"late_interval"]<-c("Planorbis")
  bival[bival$collection_no%in%c(29235:29240),"early_interval"]<-c("Bucklandi")
  bival[bival$collection_no%in%c(29235:29240),"late_interval"]<-c("Bucklandi")
  bival[bival$collection_no%in%c(29241:29242),"early_interval"]<-c("Semicostatum")
  bival[bival$collection_no%in%c(29241:29242),"late_interval"]<-c("Semicostatum")
  bival[bival$collection_no==176060,"early_interval"]<-c("Tilmani/spelae")
  bival[bival$collection_no==176060,"late_interval"]<-c("Tilmani/spelae")
  bival[bival$collection_no==176124,"early_interval"]<-c("Hettangian")
  bival[bival$collection_no==176124,"late_interval"]<-c("Early Sinemurian") #According to ammonoid zone

  bival[which(bival$formation=="Bortepa"),"early_interval"]<-c("Early Rhaetian")
  bival[which(bival$formation=="Bortepa"),"late_interval"]<-c("Early Rhaetian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Brisbois"),"early_interval"]<-c("Tuvalian")
  bival[which(bival$formation=="Brisbois"),"late_interval"]<-c("Tuvalian") #According to the original reference

  bival[which(bival$formation=="Broadford Beds"),"early_interval"]<-c("Early Sinemurian")
  bival[which(bival$formation=="Broadford Beds"),"late_interval"]<-c("Early Sinemurian") #Sinemurian in PBDB. This formation is Hettangian-early Sinemurian

  bival[which(bival$formation=="Broccatello"),"early_interval"]<-c("Early Sinemurian")
  bival[which(bival$formation=="Broccatello"),"late_interval"]<-c("Early Sinemurian") #Following Flannery-Sutherland et al. 2022

  bival[bival$collection_no%in%137003:137006,"early_interval"]<-c("Pelsonian")
  bival[bival$collection_no%in%137003:137006,"late_interval"]<-c("Illyrian") #Buchenstein Formation: Pelsonian-Ladinian; these collection were given an Anisian age

  bival[which(bival$formation=="Buko Limestone"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Buko Limestone"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Bulunkan"),"early_interval"]<-c("Late Rhaetian")
  bival[which(bival$formation=="Bulunkan"),"late_interval"]<-c("Late Rhaetian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Byrranga"),"early_interval"]<-c("Asselian")
  bival[which(bival$formation=="Byrranga"),"late_interval"]<-c("Early Artinskian") #Artinskian-Kungurian in PBDB, but Asselian-Artinskian in Shishlova and Dubkova 2021

  bival[which(bival$formation=="Cabacos"),"early_interval"]<-c("Middle Oxfordian")
  bival[which(bival$formation=="Cabacos"),"late_interval"]<-c("Middle Oxfordian") #Fürsich et al. 2022

  bival[bival$collection_no%in%c(29180:29184,29278,29280,29281,29283:29291,29293,29295:29302,29304,29305,29307,29309,29310:29321,29323:29333),"early_interval"]<-c("Semicostatum")
  bival[bival$collection_no%in%c(29180:29184,29278,29280,29281,29283:29291,29293,29295:29302,29304,29305,29307,29309,29310:29321,29323:29333),"late_interval"]<-c("Semicostatum")
  bival[bival$collection_no%in%c(29178:29179,29264:29277),"early_interval"]<-c("Bucklandi")
  bival[bival$collection_no%in%c(29178:29179,29264:29277),"late_interval"]<-c("Bucklandi")
  bival[bival$collection_no%in%c(29253:29254),"early_interval"]<-c("Planorbis")
  bival[bival$collection_no%in%c(29253:29254),"late_interval"]<-c("Planorbis") #According to ammonoid zone

  bival[which(bival$formation=="Calcare di Dosso dei Morti"),"early_interval"]<-c("Pelsonian")
  bival[which(bival$formation=="Calcare di Dosso dei Morti"),"late_interval"]<-c("Illyrian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Calcare di Prezzo"),"early_interval"]<-c("Pelsonian")
  bival[which(bival$formation=="Calcare di Prezzo"),"late_interval"]<-c("Illyrian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Capitan"&bival$early_interval=="Wordian"),"early_interval"]<-c("Late Wordian")
  bival[which(bival$formation=="Capitan"&bival$late_interval=="Wordian"),"late_interval"]<-c("Late Wordian") #Dunbar 1960

  bival[which(bival$formation=="Carditaschichten"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Carditaschichten"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Cassian"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Cassian"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Chambara"&bival$early_interval=="Norian"),"early_interval"]<-c("Alaunian")
  bival[which(bival$formation=="Chambara"&bival$late_interval=="Norian"),"late_interval"]<-c("Sevatian")
  bival[which(bival$formation=="Chambara"&bival$early_interval=="Rhaetian"),"early_interval"]<-c("Early Rhaetian")
  bival[which(bival$formation=="Chambara"&bival$late_interval=="Rhaetian"),"late_interval"]<-c("Early Rhaetian")
  bival[which(bival$formation=="Chambara"&bival$early_interval=="Late Triassic"),"early_interval"]<-c("Alaunian")
  bival[which(bival$formation=="Chambara"&bival$late_interval=="Late Triassic"),"late_interval"]<-c("Early Rhaetian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Chandalaz"&bival$early_interval=="Wordian"),"early_interval"]<-c("Late Wordian")
  bival[which(bival$formation=="Chandalaz"&bival$late_interval=="Wordian"),"late_interval"]<-c("Early Capitanian") #According to foram zone

  bival[which(bival$formation=="Cherry Canyon"&bival$early_interval=="Roadian"),"early_interval"]<-c("Late Roadian")
  bival[which(bival$formation=="Cherry Canyon"&bival$late_interval=="Roadian"),"late_interval"]<-c("Late Roadian")
  bival[which(bival$formation=="Cherry Canyon"&bival$early_interval=="Wordian"),"early_interval"]<-c("Early Wordian")
  bival[which(bival$formation=="Cherry Canyon"&bival$late_interval=="Wordian"),"late_interval"]<-c("Early Wordian") #GTS2020

  bival[bival$collection_no%in%156846:156857,"early_interval"]<-c("Wuchiapingian")
  bival[bival$collection_no%in%156846:156857,"late_interval"]<-c("Wuchiapingian") #Zeng 1995. "Changxing Formation" of these collections should be "Longtan Formation" according to the original reference

  bival[which(bival$formation=="Chapin Peak"),"early_interval"]<-c("Sevatian")
  bival[which(bival$formation=="Chapin Peak"),"late_interval"]<-c("Sevatian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Charmouth Mudstone"&bival$member=="Black Ven Marl"),"early_interval"]<-c("Late Sinemurian")
  bival[which(bival$formation=="Charmouth Mudstone"&bival$member=="Black Ven Marl"),"late_interval"]<-c("Late Sinemurian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Chavoley Beds"),"early_interval"]<-c("Early Oxfordian")
  bival[which(bival$formation=="Chavoley Beds"),"late_interval"]<-c("Middle Oxfordian") #According to the ammonoid zone

  bival[which(bival$formation=="Chhidrun"&bival$early_interval=="Wuchiapingian"),"early_interval"]<-c("Late Wuchiapingian")
  bival[which(bival$formation=="Chhidru"&bival$late_interval=="Wuchiapingian"),"late_interval"]<-c("Late Wuchiapingian") #Waterhouse 2010

  bival[which(bival$formation=="Cleveland Ironstone"&bival$member=="Penny Nab"),"early_interval"]<-c("Margaritatus")
  bival[which(bival$formation=="Cleveland Ironstone"&bival$member=="Penny Nab"),"late_interval"]<-c("Margaritatus") #According to the ammonoid zone

  bival[which(bival$formation=="Cleveland Ironstone"&bival$member=="Kettleness"),"early_interval"]<-c("Spinatum")
  bival[which(bival$formation=="Cleveland Ironstone"&bival$member=="Kettleness"),"late_interval"]<-c("Spinatum") #According to the ammonoid zone

  bival[which(bival$formation=="Clyde"&bival$early_interval=="Artinskian"),"early_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Clyde"&bival$late_interval=="Artinskian"),"late_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Clyde"&bival$early_interval=="Kungurian"),"early_interval"]<-c("Early Kungurian")
  bival[which(bival$formation=="Clyde"&bival$late_interval=="Kungurian"),"late_interval"]<-c("Early Kungurian") #Lucas 2018

  bival[which(bival$formation=="Clypeus Grit"&bival$early_interval=="Bajocian"),"early_interval"]<-c("Late Bajocian")
  bival[which(bival$formation=="Clypeus Grit"&bival$late_interval=="Bajocian"),"late_interval"]<-c("Late Bajocian")
  bival[which(bival$formation=="Clypeus Grit"&bival$early_interval=="Middle Jurassic"),"early_interval"]<-c("Late Bajocian")
  bival[which(bival$formation=="Clypeus Grit"&bival$late_interval=="Middle Jurassic"),"late_interval"]<-c("Bathonian") #Barron et al. 2012

  bival[which(bival$formation=="Cold Fish Volcanics"&bival$early_interval=="Pliensbachian"),"early_interval"]<-c("Early Pliensbachian")
  bival[which(bival$formation=="Cold Fish Volcanics"&bival$late_interval=="Pliensbachian"),"late_interval"]<-c("Early Pliensbachian") #Based on ages of other collections from the same formation

  bival[which(bival$formation=="Coleman Junction"),"early_interval"]<-c("Early Artinskian")
  bival[which(bival$formation=="Coleman Junction"),"late_interval"]<-c("Early Artinskian") #Lucas 2018

  bival[which(bival$formation=="Conchodon"),"early_interval"]<-c("Late Rhaetian")
  bival[which(bival$formation=="Conchodon"),"late_interval"]<-c("Late Rhaetian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Coralline Oolite"&bival$member=="Malton Oolite"),"early_interval"]<-c("Middle Oxfordian")
  bival[which(bival$formation=="Coralline Oolite"&bival$member=="Malton Oolite"),"late_interval"]<-c("Middle Oxfordian") #According to the ammonoid zone

  bival[which(bival$formation=="Cordevol"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Cordevol"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Cornwallis"),"early_interval"]<-c("Lacian")
  bival[which(bival$formation=="Cornwallis"),"late_interval"]<-c("Lacian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Coryphyllia Beds"),"early_interval"]<-c("Longobardian")
  bival[which(bival$formation=="Coryphyllia Beds"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Cotham"),"early_interval"]<-c("Late Rhaetian")
  bival[which(bival$formation=="Cotham"),"late_interval"]<-c("Late Rhaetian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Cs枚var"),"early_interval"]<-c("Tuvalian") #The formation name should be Csövar
  bival[which(bival$formation=="Cs枚var"),"late_interval"]<-c("Rhaetian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Cutoff"&bival$early_interval=="Kungurian"),"early_interval"]<-c("Late Kungurian")
  bival[which(bival$formation=="Cutoff"&bival$late_interval=="Kungurian"),"late_interval"]<-c("Late Kungurian")
  bival[which(bival$formation=="Cutoff"&bival$early_interval=="Roadian"),"early_interval"]<-c("Early Roadian")
  bival[which(bival$formation=="Cutoff"&bival$late_interval=="Roadian"),"late_interval"]<-c("Early Roadian") #GTS2020

  bival[which(bival$formation=="Dashizhai"),"early_interval"]<-c("Roadian")
  bival[which(bival$formation=="Dashizhai"),"late_interval"]<-c("Roadian") #Shen et al. 2021

  bival[bival$collection_no%in%187722:187732,"early_interval"]<-c("Griesbachian")
  bival[bival$collection_no%in%187722:187732,"late_interval"]<-c("Griesbachian") #Wang et al. 2017. Jianzishan section Daye Formation. Age of the brachiopod assemblage should be late Griesbachian according to Dai et al. 2018

  bival[which(bival$formation=="De Geerdalen"&bival$early_interval=="Norian"),"early_interval"]<-c("Lacian")
  bival[which(bival$formation=="De Geerdalen"&bival$late_interval=="Norian"),"late_interval"]<-c("Lacian") #Following Flannery-Sutherland et al. 2022
  bival[which(bival$formation=="De Geerdalen"&bival$early_interval=="Rhaetian"),"early_interval"]<-c("Lacian")
  bival[which(bival$formation=="De Geerdalen"&bival$late_interval=="Rhaetian"),"late_interval"]<-c("Lacian") #The Rhaetian records seem doubtful; they may be Lacian in age

  bival[which(bival$formation=="Degerb枚ls"),"early_interval"]<-c("Capitanian") #The formation name should be Degerböls
  bival[which(bival$formation=="Degerb枚ls"),"late_interval"]<-c("Capitanian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Derirong"),"early_interval"]<-c("Sevatian")
  bival[which(bival$formation=="Derirong"),"late_interval"]<-c("Sevatian") #This formation is Sevatian-Rhaetian in age; marine bivalves were reported from the lower part of this formation

  bival[which(bival$formation=="Dinwoody"&bival$early_interval=="Early Triassic"),"early_interval"]<-c("Induan")
  bival[which(bival$formation=="Dinwoody"&bival$late_interval=="Early Triassic"),"late_interval"]<-c("Induan") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Dog Creek"),"early_interval"]<-c("Late Roadian")
  bival[which(bival$formation=="Dog Creek"),"late_interval"]<-c("Early Wordian") #Kungurian in PBDB; Laurin and Hook 2022

  bival[which(bival$formation=="Doi Long"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Doi Long"),"late_interval"]<-c("Julian") #Lower part of the Carnian according to Feng et al. 2005

  bival[which(bival$formation=="Doi Yot"),"early_interval"]<-c("Aalenian")
  bival[which(bival$formation=="Doi Yot"),"late_interval"]<-c("Aalenian") #According to Kozai et al. 2011

  bival[which(bival$formation=="Dolomia Principale"),"early_interval"]<-c("Lacian")
  bival[which(bival$formation=="Dolomia Principale"),"late_interval"]<-c("Alaunian") #Dolomia Principale Formation: Tuvalian-Alaunian; only the Norian records are in the database and they are assigned to Lacian-Alaunian

  bival[which(bival$formation=="Dolomia stratificata"&bival$early_interval=="Carnian"),"early_interval"]<-c("Tuvalian")
  bival[which(bival$formation=="Dolomia stratificata"&bival$late_interval=="Carnian"),"late_interval"]<-c("Tuvalian")
  bival[which(bival$formation=="Dolomia stratificata"&bival$early_interval=="Norian"),"early_interval"]<-c("Lacian")
  bival[which(bival$formation=="Dolomia stratificata"&bival$late_interval=="Norian"),"late_interval"]<-c("Alaunian") #Dolomia stratificata Formation: Tuvalian-Alaunian

  bival[which(bival$formation=="Dolomite"),"early_interval"]<-c("Longobardian")
  bival[which(bival$formation=="Dolomite"),"late_interval"]<-c("Longobardian") #"Upper Ladinian" in PBDB

  bival[which(bival$formation=="Douling"),"early_interval"]<-c("Early Wuchiapingian")
  bival[which(bival$formation=="Douling"),"late_interval"]<-c("Early Wuchiapingian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Dun Glen"&bival$early_interval=="Norian"),"early_interval"]<-c("Lacian")
  bival[which(bival$formation=="Dun Glen"&bival$late_interval=="Norian"),"late_interval"]<-c("Lacian") #The species of this collection was reported from Lacian according to the original description

  bival[which(bival$formation=="Ebitiem"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Ebitiem"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Echii"&bival$early_interval=="Sakmarian"),"early_interval"]<-c("Late Sakmarian")
  bival[which(bival$formation=="Echii"&bival$late_interval=="Sakmarian"),"late_interval"]<-c("Late Sakmarian")
  bival[which(bival$formation=="Echiiskaya"&bival$early_interval=="Sakmarian"),"early_interval"]<-c("Late Sakmarian")
  bival[which(bival$formation=="Echiiskaya"&bival$late_interval=="Sakmarian"),"late_interval"]<-c("Late Sakmarian") #Upper Sakmarian in PBDB

  bival[which(bival$formation=="Echiiskaya"&bival$early_interval=="Artinskian"),"early_interval"]<-c("Early Artinskian")
  bival[which(bival$formation=="Echiiskaya"&bival$late_interval=="Artinskian"),"late_interval"]<-c("Early Artinskian") #Lower Artinskian in PBDB

  bival[which(bival$formation=="Echiy"&bival$early_interval=="Sakmarian"),"early_interval"]<-c("Late Sakmarian")
  bival[which(bival$formation=="Echiy"&bival$late_interval=="Sakmarian"),"late_interval"]<-c("Late Sakmarian") #Kutygin 2006

  bival[which(bival$formation=="Efremov"),"early_interval"]<-c("Asselian")
  bival[which(bival$formation=="Efremov"),"late_interval"]<-c("Early Artinskian") #Roadian in PBDB. According to Shishlova and Dubkova 2021

  bival[which(bival$formation=="Endybal-Echiy"&bival$early_interval=="Artinskian"),"early_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Endybal-Echiy"&bival$late_interval=="Artinskian"),"late_interval"]<-c("Late Artinskian") #"Upper part of the Artinskian stage" in PBDB

  bival[which(bival$formation=="Erfurt"),"early_interval"]<-c("Longobardian")
  bival[which(bival$formation=="Erfurt"),"late_interval"]<-c("Longobardian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Excelsior"),"early_interval"]<-c("Longobardian")
  bival[which(bival$formation=="Excelsior"),"late_interval"]<-c("Longobardian") #Following Flannery-Sutherland et al. 2022

  bival[bival$collection_no==83128,"early_interval"]<-c("Late Roadian")
  bival[bival$collection_no==83128,"late_interval"]<-c("Late Roadian") #Upper Antiya Formation, Upper Kazanian

  bival[bival$collection_no==102158,"early_interval"]<-c("Early Roadian")
  bival[bival$collection_no==102158,"late_interval"]<-c("Early Roadian") #Lower Antiya Formation, Lower Kazanian

  bival[which(bival$formation=="Falang"),"early_interval"]<-c("Longobardian")
  bival[which(bival$formation=="Falang"),"late_interval"]<-c("Carnian") #Zhao et al. 2021

  bival[which(bival$formation=="Fassan"),"early_interval"]<-c("Fassanian")
  bival[which(bival$formation=="Fassan"),"late_interval"]<-c("Fassanian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Fatra"),"early_interval"]<-c("Late Rhaetian")
  bival[which(bival$formation=="Fatra"),"late_interval"]<-c("Late Rhaetian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Feixianguan"&bival$early_interval=="Changhsingian"),"early_interval"]<-c("Late Changhsingian")
  bival[which(bival$formation=="Feixianguan"&bival$late_interval=="Changhsingian"),"late_interval"]<-c("Late Changhsingian") #P/T beds
  bival[which(bival$formation=="Feixianguan"&bival$early_interval=="Early Triassic"),"early_interval"]<-c("Induan")
  bival[which(bival$formation=="Feixianguan"&bival$late_interval=="Early Triassic"),"late_interval"]<-c("Induan") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Fdolomit (Main Dolomite)"&bival$early_interval=="Carnian"),"early_interval"]<-c("Tuvalian")
  bival[which(bival$formation=="Fdolomit (Main Dolomite)"&bival$late_interval=="Carnian"),"late_interval"]<-c("Tuvalian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Fels玫枚rs Limestone"),"early_interval"]<-c("Pelsonian")
  bival[which(bival$formation=="Fels玫枚rs Limestone"),"late_interval"]<-c("Pelsonian") #The formation name is Felsõörs Limestone; Anisian in PBDB

  bival[which(bival$formation=="Fels枚rs"),"early_interval"]<-c("Pelsonian")
  bival[which(bival$formation=="Fels枚rs"),"late_interval"]<-c("Pelsonian") #The formation name is Felsörs; Anisian in PBDB

  bival[which(bival$formation=="Fernie"&bival$early_interval=="Rhaetian"),"early_interval"]<-c("Late Rhaetian")
  bival[which(bival$formation=="Fernie"&bival$late_interval=="Rhaetian"),"late_interval"]<-c("Late Rhaetian") #The Fernie Formation is a Jurassic formation, but some bivalves from the basal part were reported to be of latest Rhaetian?

  bival[which(bival$formation=="Ferrugineus-Schicht"),"early_interval"]<-c("Middle Bathonian")
  bival[which(bival$formation=="Ferrugineus-Schicht"),"late_interval"]<-c("Late Bathonian") #The early and late intervals were reversed in PBDB

  bival[which(bival$formation=="Fort Riley"),"early_interval"]<-c("Early Artinskian")
  bival[which(bival$formation=="Fort Riley"),"late_interval"]<-c("Early Artinskian") #Menning et al. 2006

  bival[which(bival$formation=="Gabbs"&bival$member=="Lower"),"early_interval"]<-c("Sevatian")
  bival[which(bival$formation=="Gabbs"&bival$member=="Lower"),"late_interval"]<-c("Early Rhaetian")
  bival[which(bival$formation=="Gabbs"&bival$member%in%c("Middle","middle")),"early_interval"]<-c("Late Rhaetian")
  bival[which(bival$formation=="Gabbs"&bival$member%in%c("Middle","middle")),"late_interval"]<-c("Late Rhaetian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Gabbs"&bival$member=="Nun Mine"),"early_interval"]<-c("Sevatian")
  bival[which(bival$formation=="Gabbs"&bival$member=="Nun Mine"),"late_interval"]<-c("Early Rhaetian")
  bival[which(bival$formation=="Gabbs"&bival$member%in%c("Mount Hyatt","Muller Canyon")),"early_interval"]<-c("Late Rhaetian")
  bival[which(bival$formation=="Gabbs"&bival$member%in%c("Mount Hyatt","Muller Canyon")),"late_interval"]<-c("Late Rhaetian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Gansingen Dolomite"),"early_interval"]<-c("Tuvalian")
  bival[which(bival$formation=="Gansingen Dolomite"),"late_interval"]<-c("Tuvalian") #Following Flannery-Sutherland et al. 2022

  bival[bival$collection_no==171314,"early_interval"]<-c("Late Pliensbachian")
  bival[bival$collection_no==171314,"late_interval"]<-c("Late Pliensbachian") #According to the original reference

  bival[which(bival$formation=="Gerennavar"&bival$early_interval=="Changhsingian"),"early_interval"]<-c("Late Changhsingian")
  bival[which(bival$formation=="Gerennavar"&bival$late_interval=="Changhsingian"),"late_interval"]<-c("Late Changhsingian") #"Boundary shale", Latest Changhsingian

  bival[which(bival$formation=="Gerennavar Limestone"&bival$early_interval=="Changhsingian"),"early_interval"]<-c("Late Changhsingian")
  bival[which(bival$formation=="Gerennavar Limestone"&bival$late_interval=="Changhsingian"),"late_interval"]<-c("Late Changhsingian") #"Boundary shale", Latest Changhsingian

  bival[which(bival$formation=="Germig"&bival$early_interval=="Rhaetian"),"early_interval"]<-c("Late Rhaetian")
  bival[which(bival$formation=="Germig"&bival$late_interval=="Rhaetian"),"late_interval"]<-c("Late Rhaetian") #According to the original reference

  bival[which(bival$formation=="Gevanim"&bival$early_interval=="Anisian"),"early_interval"]<-c("Pelsonian")
  bival[which(bival$formation=="Gevanim"&bival$late_interval=="Anisian"),"late_interval"]<-c("Illyrian") #"Upper Anisian" in PBDB

  bival[which(bival$formation=="Ghalilah"),"early_interval"]<-c("Sevatian")
  bival[which(bival$formation=="Ghalilah"),"late_interval"]<-c("Rhaetian") #The Triassic records are given a Sevatian-Rhaetian age

  bival[which(bival$formation=="Gijon"&bival$early_interval=="Rhaetian"),"early_interval"]<-c("Late Rhaetian")
  bival[which(bival$formation=="Gijon"&bival$late_interval=="Rhaetian"),"late_interval"]<-c("Late Rhaetian")
  bival[which(bival$formation=="Gij贸n"&bival$early_interval=="Rhaetian"),"early_interval"]<-c("Late Rhaetian")
  bival[which(bival$formation=="Gij贸n"&bival$late_interval=="Rhaetian"),"late_interval"]<-c("Late Rhaetian")
  bival[which(bival$formation=="Gij贸n"&bival$early_interval=="Hettangian"),"early_interval"]<-c("Planorbis")
  bival[which(bival$formation=="Gij贸n"&bival$late_interval=="Hettangian"),"late_interval"]<-c("Planorbis") #The formation name is Gijón. Following the original reference

  bival[bival$collection_no%in%225514:225517,"early_interval"]<-c("Late Capitanian")
  bival[bival$collection_no%in%225514:225517,"late_interval"]<-c("Late Capitanian") #"Uppermost Capitanian"

  bival[which(bival$formation=="Gorno"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Gorno"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Grabfeld"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Grabfeld"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Grantsville"),"early_interval"]<-c("Longobardian")
  bival[which(bival$formation=="Grantsville"),"late_interval"]<-c("Longobardian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Grey Beds"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Grey Beds"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Gryphite Grits"),"early_interval"]<-c("Early Bajocian")
  bival[which(bival$formation=="Gryphite Grits"),"late_interval"]<-c("Early Bajocian")
  bival[which(bival$formation=="Lower Trigonia Grit"),"early_interval"]<-c("Early Bajocian")
  bival[which(bival$formation=="Lower Trigonia Grit"),"late_interval"]<-c("Early Bajocian") #Barron et al. 2012

  bival[which(bival$formation=="Gungdang"),"early_interval"]<-c("Olenekian")
  bival[which(bival$formation=="Gungdang"),"late_interval"]<-c("Olenekian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Guemgouma"),"early_interval"]<-c("Early Bajocian")
  bival[which(bival$formation=="Guemgouma"),"late_interval"]<-c("Early Bajocian") #According to the ammonoid zone

  bival[which(bival$formation=="Gujo"),"early_interval"]<-c("Changhsingian")
  bival[which(bival$formation=="Gujo"),"late_interval"]<-c("Changhsingian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Gundara"&bival$early_interval=="Kungurian"),"early_interval"]<-c("Late Kungurian")
  bival[which(bival$formation=="Gundara"&bival$late_interval=="Kungurian"),"late_interval"]<-c("Late Kungurian")
  bival[which(bival$formation=="Gundara"&bival$early_interval=="Roadian"),"early_interval"]<-c("Early Roadian")
  bival[which(bival$formation=="Gundara"&bival$late_interval=="Roadian"),"late_interval"]<-c("Early Roadian") #Gundara Formation: Upper Kungurian-Lower Roadian in Angiolini et al. 2016

  bival[which(bival$formation=="Gusinaya"&bival$early_interval=="Sakmarian"),"early_interval"]<-c("Late Sakmarian")
  bival[which(bival$formation=="Gusinaya"&bival$late_interval=="Sakmarian"),"late_interval"]<-c("Late Sakmarian")
  bival[which(bival$formation=="Gusinaya"&bival$early_interval=="Artinskian"),"early_interval"]<-c("Early Artinskian")
  bival[which(bival$formation=="Gusinaya"&bival$late_interval=="Artinskian"),"late_interval"]<-c("Early Artinskian") #Biakov 2013

  bival[which(bival$formation=="Gusinozemelskaya"),"early_interval"]<-c("Ufimian")
  bival[which(bival$formation=="Gusinozemelskaya"),"late_interval"]<-c("Ufimian") #Ufimian

  bival[which(bival$formation=="Gypsum Spring"),"early_interval"]<-c("Late Bajocian")
  bival[which(bival$formation=="Gypsum Spring"),"late_interval"]<-c("Late Bajocian") #The marine fossils were reported from the Middle Member, which is Late Bajocian in age

  bival[which(bival$formation=="Habo beds"&bival$late_interval=="Oxfordian"),"early_interval"]<-c("Late Callovian")
  bival[which(bival$formation=="Habo beds"&bival$late_interval=="Oxfordian"),"late_interval"]<-c("Early Oxfordian") #The youngest part of these beds, Callovian-Oxfordian in PBDB

  bival[which(bival$formation=="Halleck"),"early_interval"]<-c("Sakmarian")
  bival[which(bival$formation=="Halleck"),"late_interval"]<-c("Artinskian") #Asselian in PBDB. But it is possibly middle-Late Cisuralian.

  bival[bival$collection_no==117444,"early_interval"]<-c("Kungurian")
  bival[bival$collection_no==117444,"late_interval"]<-c("Kungurian") #The age of this collection should be Kungurian according to Lee et al. 2019

  bival[which(bival$formation=="Hambleton Oolite"),"early_interval"]<-c("Early Oxfordian")
  bival[which(bival$formation=="Hambleton Oolite"),"late_interval"]<-c("Early Oxfordian") #According to the ammonoid zone

  bival[which(bival$formation=="Hanwang"&bival$member=="Lower"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Hanwang"&bival$member=="Lower"),"late_interval"]<-c("Julian")  #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Hauptdolomit"),"early_interval"]<-c("Lacian")
  bival[which(bival$formation=="Hauptdolomit"),"late_interval"]<-c("Alaunian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Heiberg"),"early_interval"]<-c("Alaunian")
  bival[which(bival$formation=="Heiberg"),"late_interval"]<-c("Alaunian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Helmsdale Boulder Beds"&bival$early_interval=="Kimmeridgian"),"early_interval"]<-c("Late Kimmeridgian")
  bival[which(bival$formation=="Helmsdale Boulder Beds"&bival$late_interval=="Kimmeridgian"),"late_interval"]<-c("Late Kimmeridgian") #According to the ammonoid zone

  bival[which(bival$formation=="Helmsdale Boulder Beds"&bival$early_interval=="Tithonian"),"early_interval"]<-c("Early Tithonian")
  bival[which(bival$formation=="Helmsdale Boulder Beds"&bival$late_interval=="Tithonian"),"late_interval"]<-c("Early Tithonian") #According to the ammonoid zone

  bival[which(bival$formation=="Heshan"&bival$early_interval=="Changhsingian"),"early_interval"]<-c("Early Changhsingian")
  bival[which(bival$formation=="Heshan"&bival$late_interval=="Changhsingian"),"late_interval"]<-c("Early Changhsingian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Hess"&bival$early_interval=="Artinskian"),"early_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Hess"&bival$late_interval=="Artinskian"),"late_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Hess"&bival$early_interval=="Kungurian"),"early_interval"]<-c("Early Kungurian")
  bival[which(bival$formation=="Hess"&bival$late_interval=="Kungurian"),"late_interval"]<-c("Early Kungurian") #GTS2020

  bival[which(bival$formation=="Hidegkut"),"early_interval"]<-c("Smithian")
  bival[which(bival$formation=="Hidegkut"),"late_interval"]<-c("Smithian") #Voros 2009

  bival[which(bival$formation=="Hirabara"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Hirabara"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Hirobatake"),"early_interval"]<-c("Olenekian")
  bival[which(bival$formation=="Hirobatake"),"late_interval"]<-c("Olenekian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Hoang Mai"),"early_interval"]<-c("Pelsonian")
  bival[which(bival$formation=="Hoang Mai"),"late_interval"]<-c("Illyrian") #"Late Anisian" in PBDB

  bival[which(bival$formation=="Hollenberg"),"early_interval"]<-c("Early Kungurian")
  bival[which(bival$formation=="Hollenberg"),"late_interval"]<-c("Early Kungurian") #Kungurian in PBDB. Lower Leonardian. Maybe Late Artinskian?

  bival[which(bival$formation=="Hong Hoi"&bival$early_interval=="Carnian"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Hong Hoi"&bival$late_interval=="Carnian"),"late_interval"]<-c("Julian") #According to Feng et al. 2005

  bival[which(bival$formation=="Hong Ngai"),"early_interval"]<-c("Late Wuchiapingian")
  bival[which(bival$formation=="Hong Ngai"),"late_interval"]<-c("Griesbachian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Hornos-Siles"),"early_interval"]<-c("Longobardian")
  bival[which(bival$formation=="Hornos-Siles"),"late_interval"]<-c("Longobardian") #"Upper Ladinian" in PBDB

  bival[which(bival$formation=="Hosselkus"),"early_interval"]<-c("Tuvalian")
  bival[which(bival$formation=="Hosselkus"),"late_interval"]<-c("Tuvalian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Hound Island Volcanics"),"early_interval"]<-c("Sevatian")
  bival[which(bival$formation=="Hound Island Volcanics"),"late_interval"]<-c("Sevatian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Hoyt Canyon"),"early_interval"]<-c("Sevatian")
  bival[which(bival$formation=="Hoyt Canyon"),"late_interval"]<-c("Sevatian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Huai Tak"),"early_interval"]<-c("Early Changhsingian")
  bival[which(bival$formation=="Huai Tak"),"late_interval"]<-c("Early Changhsingian")
  bival[which(bival$formation=="Huai Thak"),"early_interval"]<-c("Early Changhsingian")
  bival[which(bival$formation=="Huai Thak"),"late_interval"]<-c("Early Changhsingian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Huanggangliang"),"early_interval"]<-c("Wordian")
  bival[which(bival$formation=="Huanggangliang"),"late_interval"]<-c("Wordian") #Shen et al. 2021

  bival[which(bival$formation=="Huangzhishan"&bival$early_interval=="Changhsingian"),"early_interval"]<-c("Late Changhsingian")
  bival[which(bival$formation=="Huangzhishan"&bival$late_interval=="Changhsingian"),"late_interval"]<-c("Late Changhsingian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Hueco"&bival$member=="Robledo Mountains"),"early_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Hueco"&bival$member=="Robledo Mountains"),"late_interval"]<-c("Late Artinskian") #Lucas et al. 2015

  bival[which(bival$formation=="Hueco Canyon"&bival$early_interval=="Artinskian"),"early_interval"]<-c("Early Artinskian")
  bival[which(bival$formation=="Hueco Canyon"&bival$late_interval=="Artinskian"),"late_interval"]<-c("Early Artinskian") #Wolfcampian

  bival[which(bival$formation=="Huobachong"),"early_interval"]<-c("Alaunian")
  bival[which(bival$formation=="Huobachong"),"late_interval"]<-c("Early Rhaetian") #Tong et al. 2019, 2021

  bival[which(bival$formation=="Hybe"),"early_interval"]<-c("Late Rhaetian")
  bival[which(bival$formation=="Hybe"),"late_interval"]<-c("Late Rhaetian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Ilibei"),"early_interval"]<-c("Early Sakmarian")
  bival[which(bival$formation=="Ilibei"),"late_interval"]<-c("Early Sakmarian") #Tastubian

  bival[bival$collection_no%in%c(75986,75985),"early_interval"]<-c("Lacian")
  bival[bival$collection_no%in%c(75986,75985),"late_interval"]<-c("Lacian") #Upper Jiapila Formation
  bival[bival$collection_no==123659,"early_interval"]<-c("Carnian")
  bival[bival$collection_no==123659,"late_interval"]<-c("Lacian") #Unknown horizon of the Jiapila Formation

  bival[which(bival$formation=="Jieshandaji"),"early_interval"]<-c("Early Oxfordian") #The formation name should be Jieshandaban
  bival[which(bival$formation=="Jieshandaji"),"late_interval"]<-c("Early Oxfordian") #Sun et al. 2017

  bival[which(bival$formation=="Jinji"&bival$early_interval=="Hettangian"),"early_interval"]<-c("Late Hettangian")
  bival[which(bival$formation=="Jinji"&bival$late_interval=="Hettangian"),"late_interval"]<-c("Late Hettangian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Jungle Creek"&bival$early_interval=="Artinskian"),"early_interval"]<-c("Early Artinskian")
  bival[which(bival$formation=="Jungle Creek"&bival$late_interval=="Artinskian"),"late_interval"]<-c("Early Artinskian") #Waterhouse 2018

  bival[which(bival$formation=="Juripu"&bival$early_interval=="Capitanian"),"early_interval"]<-c("Late Capitanian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Kaibab"&bival$early_interval=="Kungurian"),"early_interval"]<-c("Late Kungurian")
  bival[which(bival$formation=="Kaibab"&bival$late_interval=="Kungurian"),"late_interval"]<-c("Early Roadian") #Lucas and Henderson 2021

  bival[which(bival$formation=="Kamar-e-Mehdi"&bival$member=="Echellon Limestone"),"early_interval"]<-c("Middle Callovian")
  bival[which(bival$formation=="Kamar-e-Mehdi"&bival$member=="Echellon Limestone"),"late_interval"]<-c("Late Callovian") #This member is the lowest member of the formation

  bival[which(bival$formation=="Kamishak"),"early_interval"]<-c("Sevatian")
  bival[which(bival$formation=="Kamishak"),"late_interval"]<-c("Sevatian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Kamosho"),"early_interval"]<-c("Lacian")
  bival[which(bival$formation=="Kamosho"),"late_interval"]<-c("Alaunian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Kang Pla"),"early_interval"]<-c("Tuvalian")
  bival[which(bival$formation=="Kang Pla"),"late_interval"]<-c("Tuvalian") #Feng et al. 2005

  bival[which(bival$formation=="Kangshare"&bival$early_interval=="Changhsingian"),"early_interval"]<-c("Late Changhsingian")
  bival[which(bival$formation=="Kangshare"&bival$late_interval=="Changhsingian"),"late_interval"]<-c("Late Changhsingian") #Shen et al. 2010

  bival[which(bival$formation=="Kankarin"),"early_interval"]<-c("Early Artinskian")
  bival[which(bival$formation=="Kankarin"),"late_interval"]<-c("Early Artinskian") #"Early Artinskian" in PBDB

  bival[which(bival$formation=="Kap Leslie"&bival$member%in%c("pernaryggen","Pernaryggen")),"early_interval"]<-c("Middle Volgian")
  bival[which(bival$formation=="Kap Leslie"&bival$member%in%c("pernaryggen","Pernaryggen")),"late_interval"]<-c("Middle Volgian") #Surlyk et al. 2021
  bival[which(bival$formation=="Kap Leslie"&bival$member=="Krebsedal"),"early_interval"]<-c("Early Volgian")
  bival[which(bival$formation=="Kap Leslie"&bival$member=="Krebsedal"),"late_interval"]<-c("Middle Volgian") #Surlyk et al. 2021
  bival[which(bival$formation=="Kap Leslie"&bival$member=="Astartedal"),"early_interval"]<-c("Middle Volgian")
  bival[which(bival$formation=="Kap Leslie"&bival$member=="Astartedal"),"late_interval"]<-c("Middle Volgian") #Surlyk et al. 2021
  bival[which(bival$formation=="Kap Leslie"&bival$member=="Aldinger Elv"),"early_interval"]<-c("Late Oxfordian")
  bival[which(bival$formation=="Kap Leslie"&bival$member=="Aldinger Elv"),"late_interval"]<-c("Late Oxfordian") #Surlyk et al. 2021

  bival[which(bival$formation=="Kapp Starostin"&bival$member=="Selanderneset"),"early_interval"]<-c("Wuchiapingian")
  bival[which(bival$formation=="Kapp Starostin"&bival$member=="Selanderneset"),"late_interval"]<-c("Wuchiapingian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Kapp Starostin"&bival$member=="Voringen"&bival$early_interval=="Artinskian"),"early_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Kapp Starostin"&bival$member=="Voringen"&bival$late_interval=="Artinskian"),"late_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Kapp Starostin"&bival$member=="Voringen"&bival$early_interval=="Kungurian"),"early_interval"]<-c("Early Kungurian")
  bival[which(bival$formation=="Kapp Starostin"&bival$member=="Voringen"&bival$late_interval=="Kungurian"),"late_interval"]<-c("Early Kungurian") #Lee et al. 2019

  bival[which(bival$formation=="Kapp Starostin"&bival$early_interval=="Artinskian"),"early_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Kapp Starostin"&bival$late_interval=="Artinskian"),"late_interval"]<-c("Late Artinskian") #The basal part of this formation (Voringen Member) is late Artinskian-early Kungurian according to Lee et al. 2019

  bival[which(bival$formation=="Kapp Toscana"&bival$early_interval=="Ladinian"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Kapp Toscana"&bival$late_interval=="Ladinian"),"late_interval"]<-c("Julian")
  bival[which(bival$formation=="Kapp Toscana"&bival$early_interval=="Norian"),"early_interval"]<-c("Lacian")
  bival[which(bival$formation=="Kapp Toscana"&bival$late_interval=="Norian"),"late_interval"]<-c("Lacian") #This formation is Julian-Lacian

  bival[which(bival$formation=="Kapp Toscana"&bival$member=="Tschermarkfjellet"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Kapp Toscana"&bival$member=="Tschermarkfjellet"),"late_interval"]<-c("Julian")
  bival[which(bival$formation=="Kapp Toscana"&bival$member=="Tschermakfjellet"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Kapp Toscana"&bival$member=="Tschermakfjellet"),"late_interval"]<-c("Julian") #The lower member

  bival[which(bival$formation=="Karadan"&bival$early_interval=="Norian"),"early_interval"]<-c("Alaunian")
  bival[which(bival$formation=="Karadan"&bival$late_interval=="Norian"),"late_interval"]<-c("Alaunian") #The two bivalve zones are largely overlaped with Alaunian

  bival[which(bival$formation=="Karadzhatykskaya"),"early_interval"]<-c("Spathian")
  bival[which(bival$formation=="Karadzhatykskaya"),"late_interval"]<-c("Spathian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Karazin"),"early_interval"]<-c("Aegean")
  bival[which(bival$formation=="Karazin"),"late_interval"]<-c("Aegean") #This formation includes some middle and late Anisian collections, but the bivalves are from the "lower Anisian"

  bival[which(bival$formation=="Karchowice"),"early_interval"]<-c("Pelsonian")
  bival[which(bival$formation=="Karchowice"),"late_interval"]<-c("Illyrian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation%in%c("Kardosret","Kardosr茅i M茅szko","Kardosr茅ti M茅szko")),"early_interval"]<-c("Hettangian")
  bival[which(bival$formation%in%c("Kardosret","Kardosr茅i M茅szko","Kardosr茅ti M茅szko")),"late_interval"]<-c("Hettangian") #Kardosréi Mészko, Kardosréti Mészko. Voros 2022

  bival[which(bival$formation=="Kashiwadaira"),"early_interval"]<-c("Wordian")
  bival[which(bival$formation=="Kashiwadaira"),"late_interval"]<-c("Wordian")
  bival[which(bival$formation=="Takakurayama"),"early_interval"]<-c("Wordian")
  bival[which(bival$formation=="Takakurayama"),"late_interval"]<-c("Wordian") #Following Ehiro 2022

  bival[which(bival$formation=="Kattamardzanaj"),"early_interval"]<-c("Longobardian")
  bival[which(bival$formation=="Kattamardzanaj"),"late_interval"]<-c("Longobardian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Kawhia Point Siltstone"),"early_interval"]<-c("Early Kimmeridgian")
  bival[which(bival$formation=="Kawhia Point Siltstone"),"late_interval"]<-c("Early Kimmeridgian") #"Early Kimmeridgian" in PBDB

  bival[which(bival$formation=="Kayitou"&bival$early_interval=="Changhsingian"),"early_interval"]<-c("Late Changhsingian")
  bival[which(bival$formation=="Kayitou"&bival$late_interval=="Changhsingian"),"late_interval"]<-c("Late Changhsingian") #P-T transitional bed

  bival[which(bival$formation=="Kellaways Clay"),"early_interval"]<-c("Early Callovian")
  bival[which(bival$formation=="Kellaways Clay"),"late_interval"]<-c("Early Callovian") #Scotney et al. 2012

  bival[which(bival$formation=="Kellaways Sand"),"early_interval"]<-c("Early Callovian")
  bival[which(bival$formation=="Kellaways Sand"),"late_interval"]<-c("Early Callovian") #Scotney et al. 2012

  bival[which(bival$formation=="Kendlbach"&bival$early_interval=="Rhaetian"),"early_interval"]<-c("Late Rhaetian")
  bival[which(bival$formation=="Kendlbach"&bival$late_interval=="Rhaetian"),"late_interval"]<-c("Late Rhaetian")
  bival[which(bival$formation=="Kendlbach"&bival$early_interval=="Hettangian"),"early_interval"]<-c("Planorbis")
  bival[which(bival$formation=="Kendlbach"&bival$late_interval=="Hettangian"),"late_interval"]<-c("Planorbis") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Keziliqiman"&bival$early_interval=="Kungurian"),"early_interval"]<-c("Early Kungurian")
  bival[which(bival$formation=="Keziliqiman"&bival$late_interval=="Kungurian"),"late_interval"]<-c("Early Kungurian") #Chen and Shi 2006

  bival[which(bival$formation=="Khabakh"&bival$early_interval=="Kungurian"),"early_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Khabakh"&bival$late_interval=="Kungurian"),"late_interval"]<-c("Late Artinskian") #Shi 2006 the Echian Horizon

  bival[which(bival$formation=="Khachik"&bival$early_interval=="Wuchiapingian"),"early_interval"]<-c("Early Wuchiapingian")
  bival[which(bival$formation=="Khachik"&bival$late_interval=="Wuchiapingian"),"late_interval"]<-c("Early Wuchiapingian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Khalalin"),"early_interval"]<-c("Ufimian")
  bival[which(bival$formation=="Khalalin"),"late_interval"]<-c("Ufimian") #Aphanaia korkodonica Zone, Ufimian, according to Biakov 2012

  bival[which(bival$formation=="Khaldzin"),"early_interval"]<-c("Early Kungurian")
  bival[which(bival$formation=="Khaldzin"),"late_interval"]<-c("Early Kungurian") #Biakov 2012; this species is common at the end of the Aphanaia lima Zone

  bival[which(bival$formation=="Khanalichan"),"early_interval"]<-c("Artinskian")
  bival[which(bival$formation=="Khanalichan"),"late_interval"]<-c("Artinskian") #Kungurian in Abramov and Grigorieva 1988, Artinskian in Klets 2000. The Sigskaya Formation below this formation is Sakmarian

  bival[which(bival$formation=="Khangsar"),"early_interval"]<-c("Induan")
  bival[which(bival$formation=="Khangsar"),"late_interval"]<-c("Induan") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Khorokytskaya"),"early_interval"]<-c("Late Asselian")
  bival[which(bival$formation=="Khorokytskaya"),"late_interval"]<-c("Early Sakmarian") #"Upper Asselian-Lower Sakmarian" in PBDB

  bival[which(bival$formation=="Khao Phra"),"early_interval"]<-c("Late Kungurian")
  bival[which(bival$formation=="Khao Phra"),"late_interval"]<-c("Late Kungurian") #Xu et al. 2022

  bival[which(bival$formation=="Khovsgol"&bival$early_interval=="Artinskian"),"early_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Khovsgol"&bival$late_interval=="Artinskian"),"late_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Khovsgol"&bival$early_interval=="Wordian"),"early_interval"]<-c("Kungurian")
  bival[which(bival$formation=="Khovsgol"&bival$late_interval=="Wordian"),"late_interval"]<-c("Kungurian") #Manankov et al. 2006 and the original reference

  bival[which(bival$formation=="Khun Huai"),"early_interval"]<-c("Toarcian")
  bival[which(bival$formation=="Khun Huai"),"late_interval"]<-c("Middle Aalenian") #According to Kozai et al. 2011

  bival[which(bival$formation=="Khunamuh"&bival$early_interval=="Changhsingian"),"late_interval"]<-c("Late Changhsingian")
  bival[which(bival$formation=="Khunamuh"&bival$early_interval=="Changhsingian"),"early_interval"]<-c("Late Changhsingian") #Changhsingian-Griesbachian in PBDB. Member E1 should be the latest Permian

  bival[bival$collection_no%in%c("1911","1613","1912"),"early_interval"]<-c("Baylei")
  bival[bival$collection_no%in%c("1911","1613","1912"),"late_interval"]<-c("Baylei") #According to ammonoid zone

  bival[bival$collection_no%in%c("8574","8600","8657"),"early_interval"]<-c("Mutabilis")
  bival[bival$collection_no%in%c("8574","8600","8657"),"late_interval"]<-c("Mutabilis") #According to ammonoid zone

  bival[bival$collection_no==38494,"early_interval"]<-c("Cymodoce")
  bival[bival$collection_no==38494,"late_interval"]<-c("Mutabilis") #According to ammonoid zone

  bival[which(bival$formation=="Kitamatadani"),"early_interval"]<-c("Early Pliensbachian")
  bival[which(bival$formation=="Kitamatadani"),"late_interval"]<-c("Early Pliensbachian") #Nakada et al. 2021

  bival[which(bival$formation=="Klingnau"),"early_interval"]<-c("Bathonian")
  bival[which(bival$formation=="Klingnau"),"late_interval"]<-c("Bathonian") #The "Varians Beds" may be middle Bathonian in age

  bival[which(bival$formation=="Klyuchevskaya"),"early_interval"]<-c("Early Wuchiapingian")
  bival[which(bival$formation=="Klyuchevskaya"),"late_interval"]<-c("Early Wuchiapingian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Kobyuma"),"early_interval"]<-c("Ufimian")
  bival[which(bival$formation=="Kobyuma"),"late_interval"]<-c("Roadian") #Davydov et al. 2022

  bival[which(bival$formation=="Koessen"&bival$member%in%c("Hochalm","Hochalm, Unit 1","Hochalm, Unit 2","Hochalm, Unit 3","Hochalm, Unit 4")),"early_interval"]<-c("Early Rhaetian")
  bival[which(bival$formation=="Koessen"&bival$member%in%c("Hochalm","Hochalm, Unit 1","Hochalm, Unit 2","Hochalm, Unit 3","Hochalm, Unit 4")),"late_interval"]<-c("Early Rhaetian")
  bival[which(bival$formation=="Koessen"&bival$member%in%c("Eiberg, Unit 1","Eiberg, Unit1","Eiberg, Unit 2","Eiberg, Unit 3")),"early_interval"]<-c("Late Rhaetian")
  bival[which(bival$formation=="Koessen"&bival$member%in%c("Eiberg, Unit 1","Eiberg, Unit1","Eiberg, Unit 2","Eiberg, Unit 3")),"late_interval"]<-c("Late Rhaetian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Konglomeratbank 3"),"early_interval"]<-c("Bithynian")
  bival[which(bival$formation=="Konglomeratbank 3"),"late_interval"]<-c("Bithynian") #"Bithynian" in PBDB

  bival[which(bival$formation=="Kossen"&bival$member%in%c("2","3","Unit 3","Unit 4","Hochalm","Hochalm, Unit 1","Hochalm,Unit1","Hochalm, Unit1","Hochalm, Unit 2",
                                                          "Hochalm,Unit2","Holchalm, Unit 2","Hochalm, Unit3","Hochalm,Unit3","Hochalm, Unit 3","Holchalm, Unit 3","Hochalm,Unit4","Hochalm, Unit 4")),"early_interval"]<-c("Early Rhaetian")
  bival[which(bival$formation=="Kossen"&bival$member%in%c("2","3","Unit 3","Unit 4","Hochalm","Hochalm, Unit 1","Hochalm,Unit1","Hochalm, Unit1","Hochalm, Unit 2",
                                                          "Hochalm,Unit2","Holchalm, Unit 2","Hochalm, Unit3","Hochalm,Unit3","Hochalm, Unit 3","Holchalm, Unit 3","Hochalm,Unit4","Hochalm, Unit 4")),"late_interval"]<-c("Early Rhaetian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Kossen"&bival$member%in%c("6月7日","Unit 6-7","Eiberg","Eiberg, Unit 1","Eiberg, Unit 2","Eiberg, Unit 3","Eiberg, Unit 4")),"early_interval"]<-c("Late Rhaetian")
  bival[which(bival$formation=="Kossen"&bival$member%in%c("6月7日","Unit 6-7","Eiberg","Eiberg, Unit 1","Eiberg, Unit 2","Eiberg, Unit 3","Eiberg, Unit 4")),"late_interval"]<-c("Late Rhaetian") #"6月7日" was 6-7; following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Kowhai Point Siltstone"),"early_interval"]<-c("Early Kimmeridgian")
  bival[which(bival$formation=="Kowhai Point Siltstone"),"late_interval"]<-c("Early Kimmeridgian") #"Early Kimmeridgian" in PBDB

  bival[which(bival$formation=="Kozhevnikov"&bival$early_interval=="Capitanian"),"early_interval"]<-c("Early Capitanian")
  bival[which(bival$formation=="Kozhevnikov"&bival$late_interval=="Capitanian"),"late_interval"]<-c("Early Capitanian") #Maitaia bella Zone, lower Capitanian

  bival[which(bival$formation=="Kulu"&bival$early_interval=="Wuchiapingian"),"early_interval"]<-c("Late Wuchiapingian")
  bival[which(bival$formation=="Kulu"&bival$late_interval=="Wuchiapingian"),"late_interval"]<-c("Late Wuchiapingian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Kupferschiefer"),"early_interval"]<-c("Early Wuchiapingian")
  bival[which(bival$formation=="Kupferschiefer"),"late_interval"]<-c("Early Wuchiapingian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Kurotaki"),"early_interval"]<-c("Smithian")
  bival[which(bival$formation=="Kurotaki"),"late_interval"]<-c("Smithian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Kusano"),"early_interval"]<-c("Olenekian")
  bival[which(bival$formation=="Kusano"),"late_interval"]<-c("Olenekian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Laishike"),"early_interval"]<-c("Tuvalian")
  bival[which(bival$formation=="Laishike"),"late_interval"]<-c("Tuvalian") #Zhao et al. 2021

  bival[which(bival$formation=="Lamei"),"early_interval"]<-c("Olenekian")
  bival[which(bival$formation=="Lamei"),"late_interval"]<-c("Olenekian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Lanashan"),"early_interval"]<-c("Alaunian")
  bival[which(bival$formation=="Lanashan"),"late_interval"]<-c("Alaunian") #The brachiopods from the Lanashan formation are similar to those from the Qulonggongba Formation, suggesting the same age

  bival[which(bival$formation=="Langobard"),"early_interval"]<-c("Longobardian")
  bival[which(bival$formation=="Langobard"),"late_interval"]<-c("Longobardian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Lazurnaya Bay"&bival$early_interval=="Olenekian"),"early_interval"]<-c("Smithian")
  bival[which(bival$formation=="Lazurnaya Bay"&bival$late_interval=="Olenekian"),"late_interval"]<-c("Smithian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Leikoupo"),"early_interval"]<-c("Anisian")
  bival[which(bival$formation=="Leikoupo"),"late_interval"]<-c("Anisian") #Following Flannery-Sutherland et al. 2022

  bival[bival$collection_no==225384,"early_interval"]<-c("Margaritatus")
  bival[bival$collection_no==225384,"late_interval"]<-c("Margaritatus") #According to ammonoid zone

  bival[bival$collection_no%in%225385:225386,"early_interval"]<-c("Spinatum")
  bival[bival$collection_no%in%225385:225386,"late_interval"]<-c("Spinatum") #According to ammonoid zone

  bival[which(bival$formation=="Lengwu"&bival$member=="Shiziling"),"early_interval"]<-c("Early Capitanian")
  bival[which(bival$formation=="Lengwu"&bival$member=="Shiziling"),"late_interval"]<-c("Early Capitanian") #Lower member of the Lengwu Formation
  bival[which(bival$formation=="Lengwu"&bival$member=="Cunhouling"),"early_interval"]<-c("Late Capitanian")
  bival[which(bival$formation=="Lengwu"&bival$member=="Cunhouling"),"late_interval"]<-c("Late Capitanian") #Upper member of the Lengwu Formation

  bival[which(bival$formation=="Liangshan"),"early_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Liangshan"),"late_interval"]<-c("Late Artinskian") #Shen et al. 2021

  bival[which(bival$formation=="Liard"&bival$early_interval=="Ladinian"),"early_interval"]<-c("Longobardian")
  bival[which(bival$formation=="Liard"&bival$late_interval=="Ladinian"),"late_interval"]<-c("Longobardian") #"Upper Ladinian" in PBDB

  bival[which(bival$formation=="Liard"&bival$early_interval=="Carnian"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Liard"&bival$late_interval=="Carnian"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Licha"&bival$early_interval=="Artinskian"),"early_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Licha"&bival$late_interval=="Artinskian"),"late_interval"]<-c("Early Kungurian") #Shen et al. 2017

  bival[which(bival$formation=="Licha"&bival$early_interval=="Asselian"),"early_interval"]<-c("Gzhelian")
  bival[which(bival$formation=="Licha"&bival$late_interval=="Asselian"),"late_interval"]<-c("Asselian") #Shen et al. 2017

  bival[which(bival$formation=="Liegende Bankkalk"),"early_interval"]<-c("Beckeri")
  bival[which(bival$formation=="Liegende Bankkalk"),"late_interval"]<-c("Beckeri") #The ammonoid zone

  bival[which(bival$formation=="Lilstock"),"early_interval"]<-c("Late Rhaetian")
  bival[which(bival$formation=="Lilstock"),"late_interval"]<-c("Late Rhaetian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Lindemans Bugt"),"early_interval"]<-c("Middle Volgian")
  bival[which(bival$formation=="Lindemans Bugt"),"late_interval"]<-c("Middle Volgian") #"Middle Volgian" in PBDB

  bival[which(bival$formation=="Longdongchuan"),"early_interval"]<-c("Late Changhsingian")
  bival[which(bival$formation=="Longdongchuan"),"late_interval"]<-c("Late Changhsingian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Longyin"),"early_interval"]<-c("Early Artinskian")
  bival[which(bival$formation=="Longyin"),"late_interval"]<-c("Early Artinskian") #The brachiopods from this formation were believed to be Sakmarian in age (Shen et al. 2017), but the ammonoids suggest an early Artinskian age (Zhou 2017)

  bival[which(bival$formation=="Loping"&bival$member=="Wangpanli"),"early_interval"]<-c("Early Changhsingian")
  bival[which(bival$formation=="Loping"&bival$member=="Wangpanli"),"late_interval"]<-c("Early Changhsingian") #Following Shen et al. 2017

  bival[bival$collection_no%in%204121:204123,"early_interval"]<-c("Davoei")
  bival[bival$collection_no%in%204121:204123,"late_interval"]<-c("Davoei") #According to ammonoid zone

  bival[bival$collection_no%in%204124:204125,"early_interval"]<-c("Ibex")
  bival[bival$collection_no%in%204124:204125,"late_interval"]<-c("Ibex") #According to ammonoid zone

  bival[bival$collection_no%in%204126:204129,"early_interval"]<-c("Jamesoni")
  bival[bival$collection_no%in%204126:204129,"late_interval"]<-c("Jamesoni") #According to ammonoid zone

  bival[bival$collection_no%in%204130:204133,"early_interval"]<-c("Raricostatum")
  bival[bival$collection_no%in%204131:204133,"late_interval"]<-c("Raricostatum") #According to ammonoid zone

  bival[bival$collection_no==204146,"early_interval"]<-c("Oxynotum")
  bival[bival$collection_no==204146,"late_interval"]<-c("Oxynotum") #According to ammonoid zone

  bival[bival$collection_no%in%204147:204148,"early_interval"]<-c("Obtusum")
  bival[bival$collection_no%in%204147:204148,"late_interval"]<-c("Obtusum") #According to ammonoid zone

  bival[bival$collection_no%in%204149:204151,"early_interval"]<-c("Turneri")
  bival[bival$collection_no%in%204149:204151,"late_interval"]<-c("Turneri") #According to ammonoid zone

  bival[bival$collection_no%in%204152:204154,"early_interval"]<-c("Semicostatum")
  bival[bival$collection_no%in%204152:204154,"late_interval"]<-c("Semicostatum") #According to ammonoid zone

  bival[bival$collection_no%in%204155:204157,"early_interval"]<-c("Bucklandi")
  bival[bival$collection_no%in%204155:204157,"late_interval"]<-c("Bucklandi") #According to ammonoid zone

  bival[bival$collection_no%in%204158:204159,"early_interval"]<-c("Angulata")
  bival[bival$collection_no%in%204158:204159,"late_interval"]<-c("Angulata") #According to ammonoid zone

  bival[bival$collection_no%in%204160:204161,"early_interval"]<-c("Planorbis")
  bival[bival$collection_no%in%204160:204161,"late_interval"]<-c("Planorbis") #According to ammonoid zone

  bival[which(bival$formation=="Lower"&bival$member=="White Lias Limestone"),"early_interval"]<-c("Late Rhaetian")
  bival[which(bival$formation=="Lower"&bival$member=="White Lias Limestone"),"late_interval"]<-c("Late Rhaetian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Lower Calcareous Grit"),"early_interval"]<-c("Mariae")
  bival[which(bival$formation=="Lower Calcaerous Grit"),"late_interval"]<-c("Cordatum") #According to ammonoid zone

  bival[which(bival$formation=="Lower Calcareous Grit"),"early_interval"]<-c("Early Oxfordian")
  bival[which(bival$formation=="Lower Calcareous Grit"),"late_interval"]<-c("Early Oxfordian") #According to ammonoid zone

  bival[which(bival$formation=="Lower Freestone"),"early_interval"]<-c("Aalenian")
  bival[which(bival$formation=="Lower Freestone"),"late_interval"]<-c("Aalenian") #Barron et al. 2012

  bival[bival$collection_no==204107,"early_interval"]<-c("Early Aalenian")
  bival[bival$collection_no==204107,"late_interval"]<-c("Early Aalenian") #Barron et al. 2012

  bival[which(bival$formation=="Lower Pospelovka"),"early_interval"]<-c("Early Kungurian")
  bival[which(bival$formation=="Lower Pospelovka"),"late_interval"]<-c("Early Kungurian") #Lower member, lower Kungurian

  bival[which(bival$formation=="Lower Shale-Siltstone"),"early_interval"]<-c("Late Tithonian")
  bival[which(bival$formation=="Lower Shale-Siltstone"),"late_interval"]<-c("Late Tithonian") #"Upper Tithonian" in PBDB

  bival[which(bival$formation=="Lugu"),"early_interval"]<-c("Late Kungurian")
  bival[which(bival$formation=="Lugu"),"late_interval"]<-c("Late Kungurian") #Xu et al. 2022

  bival[which(bival$formation=="Luning"&bival$early_interval=="Carnian"),"early_interval"]<-c("Tuvalian")
  bival[which(bival$formation=="Luning"&bival$late_interval=="Carnian"),"late_interval"]<-c("Tuvalian")
  bival[which(bival$formation=="Luning"&bival$early_interval=="Norian"),"early_interval"]<-c("Lacian")
  bival[which(bival$formation=="Luning"&bival$late_interval=="Norian"),"late_interval"]<-c("Lacian")
  bival[which(bival$formation=="Luning"&bival$early_interval=="Kaihikuan"),"early_interval"]<-c("Tuvalian")
  bival[which(bival$formation=="Luning"&bival$late_interval=="Kaihikuan"),"late_interval"]<-c("Lacian")

  bival[which(bival$formation=="luning"&bival$early_interval=="Carnian"),"early_interval"]<-c("Tuvalian")
  bival[which(bival$formation=="luning"&bival$late_interval=="Carnian"),"late_interval"]<-c("Tuvalian")
  bival[which(bival$formation=="luning"&bival$early_interval=="Norian"),"early_interval"]<-c("Lacian")
  bival[which(bival$formation=="luning"&bival$late_interval=="Norian"),"late_interval"]<-c("Lacian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Lunzer"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Lunzer"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Luojiadashan"),"early_interval"]<-c("Lacian")
  bival[which(bival$formation=="Luojiadashan"),"late_interval"]<-c("Lacian") #Tong et al. 2021

  bival[which(bival$formation=="Luolou"&bival$early_interval=="Changhsingian"),"early_interval"]<-c("Late Changhsingian")
  bival[which(bival$formation=="Luolou"&bival$late_interval=="Changhsingian"),"late_interval"]<-c("Late Changhsingian") #"Uppermost Changhsingian"

  bival[which(bival$formation=="Magan"),"early_interval"]<-c("Early Capitanian")
  bival[which(bival$formation=="Magan"),"late_interval"]<-c("Early Capitanian") #Maitaia bella Zone, lower Capitanian. Biakov 2013

  bival[which(bival$formation=="Magiveem"&bival$early_interval=="Gzhelian"),"late_interval"]<-c("Early Asselian") #"Gzhelian-lower Asselian" in PBDB

  bival[which(bival$formation=="Malton Oolite"),"early_interval"]<-c("Middle Oxfordian")
  bival[which(bival$formation=="Malton Oolite"),"late_interval"]<-c("Middle Oxfordian")  #According to ammonoid zone

  bival[which(bival$formation=="Mayang"),"early_interval"]<-c("Asselian")
  bival[which(bival$formation=="Mayang"),"late_interval"]<-c("Kungurian") #Lopingian in PBDB, but the "Stepanoviella" (=Cimmeriella) assemblage shows an Cisuralian age. See Shen et al. 2017

  bival[which(bival$formation=="Mangzongrong"),"early_interval"]<-c("Kungurian")
  bival[which(bival$formation=="Mangzongrong"),"late_interval"]<-c("Kungurian") #Lopingian in PBDB, but the "Stepanoviella" (=Cimmeriella) assemblage shows an Cisuralian age. Kungurian according to Xu et al. 2022

  bival[which(bival$formation=="Marnes de Latrecey"&bival$early_interval=="Late Oxfordian"),"early_interval"]<-c("Bifurcatus")
  bival[which(bival$formation=="Marnes de Latrecey"&bival$late_interval=="Late Oxfordian"),"late_interval"]<-c("Bifurcatus") #According to ammonoid zone

  bival[which(bival$formation=="Marsyangdi"),"early_interval"]<-c("Late Changhsingian")
  bival[which(bival$formation=="Marsyangdi"),"late_interval"]<-c("Late Changhsingian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Martin Bridge"&bival$early_interval=="Carnian"),"early_interval"]<-c("Tuvalian")
  bival[which(bival$formation=="Martin Bridge"&bival$late_interval=="Carnian"),"late_interval"]<-c("Tuvalian")
  bival[which(bival$formation=="Martin Bridge"&bival$late_interval=="Rhaetian"),"late_interval"]<-c("Norian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Matmor"),"early_interval"]<-c("Athleta")
  bival[which(bival$formation=="Matmor"),"late_interval"]<-c("Athleta") #According to ammonoid zone

  bival[which(bival$formation=="McCarthy"&bival$early_interval=="Norian"),"early_interval"]<-c("Sevatian")
  bival[which(bival$formation=="McCarthy"&bival$late_interval=="Norian"),"late_interval"]<-c("Sevatian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Megen"),"early_interval"]<-c("Asselian")
  bival[which(bival$formation=="Megen"),"late_interval"]<-c("Early Artinskian") #Makoshin and Kutygin 2020

  bival[which(bival$formation=="Mianwali"&bival$early_interval=="Changhsingian"),"early_interval"]<-c("Late Changhsingian")
  bival[which(bival$formation=="Mianwali"&bival$late_interval=="Changhsingian"),"late_interval"]<-c("Late Changhsingian") #PT transitional beds

  bival[which(bival$formation=="Middle Calcareous Grit"),"early_interval"]<-c("Plicatilis")
  bival[which(bival$formation=="Middle Calcareous Grit"),"late_interval"]<-c("Plicatilis") #According to ammonoid zone

  bival[which(bival$formation=="Milanovo"),"early_interval"]<-c("Fassanian")
  bival[which(bival$formation=="Milanovo"),"late_interval"]<-c("Fassanian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Mitarai"),"early_interval"]<-c("Late Tithonian")
  bival[which(bival$formation=="Mitarai"),"late_interval"]<-c("Early Berriasian") #Haggart and Matsukawa 2020

  bival[bival$collection_no%in%221258:221262,"early_interval"]<-c("Early Capitanian")
  bival[bival$collection_no%in%221258:221262,"late_interval"]<-c("Early Capitanian") #Maitaia bella Zone, lower Capitanian. Biakov 2013

  bival[bival$collection_no%in%221263:221265,"early_interval"]<-c("Early Capitanian")
  bival[bival$collection_no%in%221263:221265,"late_interval"]<-c("Early Capitanian") #Maitaia belliformis Zone, upper Capitanian. Biakov 2013

  bival[bival$collection_no==221266,"early_interval"]<-c("Late Wuchiapingian")
  bival[bival$collection_no==221266,"late_interval"]<-c("Early Changhsingian") #Lower Intomodesma costatum Zone. Biakov 2013

  bival[which(bival$formation=="Moewaka"),"early_interval"]<-c("Late Callovian")
  bival[which(bival$formation=="Moewaka"),"late_interval"]<-c("Middle Oxfordian") #"Early Heterian" in PBDB, late Callovian-early or middle Oxfordian

  bival[which(bival$formation=="Mol"),"early_interval"]<-c("Early Capitanian")
  bival[which(bival$formation=="Mol"),"late_interval"]<-c("Early Capitanian")
  bival[which(bival$formation=="Molskaya"),"early_interval"]<-c("Early Capitanian")
  bival[which(bival$formation=="Molskaya"),"late_interval"]<-c("Early Capitanian") #Biakov 2013

  bival[which(bival$formation=="Mont Grand"&bival$early_interval=="Late Toarcian"),"early_interval"]<-c("Aalensis")
  bival[which(bival$formation=="Mont Grand"&bival$late_interval=="Late Toarcian"),"late_interval"]<-c("Aalensis") #According to ammonoid zone

  bival[which(bival$formation=="Montejunto"),"early_interval"]<-c("Middle Oxfordian")
  bival[which(bival$formation=="Montejunto"),"late_interval"]<-c("Late Oxfordian") #Turner et al. 2017

  bival[which(bival$formation=="Moran"),"early_interval"]<-c("Sakmarian")
  bival[which(bival$formation=="Moran"),"late_interval"]<-c("Sakmarian") #Lucas 2018

  bival[which(bival$formation=="Morkvashi"),"early_interval"]<-c("Late Roadian")
  bival[which(bival$formation=="Morkvashi"),"late_interval"]<-c("Late Roadian") #Upper Kazanian

  bival[which(bival$formation=="Mount Greene"),"early_interval"]<-c("Asselian")
  bival[which(bival$formation=="Mount Greene"),"late_interval"]<-c("Sakmarian") #Kungurian in PBDB, but the brachiopods possible show an Asselian-Sakmarian age according to Waterhouse 2018 and Shi and Waterhouse 1996

  bival[bival$collection_no==20666,"early_interval"]<-c("Illyrian")
  bival[bival$collection_no==20666,"late_interval"]<-c("Illyrian") #This age of this collection should be the late Anisian according to the original reference

  bival[which(bival$formation=="Mughanniyya"&bival$early_interval=="Middle Jurassic"),"early_interval"]<-c("Callovian")
  bival[which(bival$formation=="Mughanniyya"&bival$late_interval=="Middle Jurassic"),"late_interval"]<-c("Callovian") #According to other collections of this formation

  bival[which(bival$formation=="Mugochan"),"early_interval"]<-c("Ufimian")
  bival[which(bival$formation=="Mugochan"),"late_interval"]<-c("Roadian") #Ufimian-Kazanian according to Biakov 2013

  bival[which(bival$formation=="Muong Trai"),"early_interval"]<-c("Longobardian")
  bival[which(bival$formation=="Muong Trai"),"late_interval"]<-c("Longobardian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Muschelkalk"&bival$member%in%c("Oberer Trochitenkalk","Ober Trochitenkalk","Trochitenkalk","Ceratitenschichten")),"early_interval"]<-c("Pelsonian")
  bival[which(bival$formation=="Muschelkalk"&bival$member%in%c("Oberer Trochitenkalk","Ober Trochitenkalk","Trochitenkalk","Ceratitenschichten")),"late_interval"]<-c("Illyrian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Muschelkalk"&bival$member=="Bank der klein terrebrat"),"early_interval"]<-c("Longobardian")
  bival[which(bival$formation=="Muschelkalk"&bival$member=="Bank der klein terrebrat"),"late_interval"]<-c("Longobardian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="N1"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="N1"),"late_interval"]<-c("Julian")
  bival[which(bival$formation=="N2"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="N2"),"late_interval"]<-c("Julian")
  bival[which(bival$formation=="N3"),"early_interval"]<-c("Tuvalian")
  bival[which(bival$formation=="N3"),"late_interval"]<-c("Tuvalian")
  bival[which(bival$formation=="N4"),"early_interval"]<-c("Lacian")
  bival[which(bival$formation=="N4"),"late_interval"]<-c("Lacian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Na Khuat"),"early_interval"]<-c("Ladinian")
  bival[which(bival$formation=="Na Khuat"),"late_interval"]<-c("Ladinian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Nabeyama"),"early_interval"]<-c("Kungurian")
  bival[which(bival$formation=="Nabeyama"),"late_interval"]<-c("Kungurian") #The Roadian collection should be Kungurian according to Tazawa et al. 2016

  bival[which(bival$formation=="Nadaska Limestones"),"early_interval"]<-c("Illyrian")
  bival[which(bival$formation=="Nadaska Limestones"),"late_interval"]<-c("Fassanian") #Near the Anisian/Ladinian boundry

  bival[which(bival$formation=="Naifa"&bival$member=="Billum"),"early_interval"]<-c("Late Oxfordian")
  bival[which(bival$formation=="Naifa"&bival$member=="Billum"),"late_interval"]<-c("Early Kimmeridgian")
  bival[which(bival$formation=="Naifa"&bival$member=="Kilya"),"early_interval"]<-c("Late Kimmeridgian")
  bival[which(bival$formation=="Naifa"&bival$member=="Kilya"),"late_interval"]<-c("Early Tithonian") #According to the original reference

  bival[which(bival$formation=="Nakatsuka"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Nakatsuka"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Nam Loong"),"early_interval"]<-c("Artinskian")
  bival[which(bival$formation=="Nam Loong"),"late_interval"]<-c("Artinskian") #Late Artinskian? See Ueno et al. 2015

  bival[which(bival$formation=="Nam Sam"),"early_interval"]<-c("Pelsonian")
  bival[which(bival$formation=="Nam Sam"),"late_interval"]<-c("Illyrian") #Fossils largely in upper part. Pelsonian forams were reported from the upper part of this formation by Miyahigashi et al. 2017

  bival[which(bival$formation=="Nam Tham"),"early_interval"]<-c("Fassanian")
  bival[which(bival$formation=="Nam Tham"),"late_interval"]<-c("Fassanian") #"Early Ladinian" in PBDB

  bival[which(bival$formation=="Nanlong"),"early_interval"]<-c("Wordian")
  bival[which(bival$formation=="Nanlong"),"late_interval"]<-c("Capitanian") #Late Maokouan in the original reference

  bival[which(bival$formation=="Naocangjiangou"),"early_interval"]<-c("Anisian")
  bival[which(bival$formation=="Naocangjiangou"),"late_interval"]<-c("Anisian") #Aegean-Bithynian in PBDB, but some brachiopod species are from the middle-late Anisian in the adjacent Qilian Mountains

  bival[which(bival$formation=="Narawara"),"early_interval"]<-c("Olenekian")
  bival[which(bival$formation=="Narawara"),"late_interval"]<-c("Olenekian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Nayband"&bival$member%in%c("Howz-e Khan","Howz-e-Khan")),"early_interval"]<-c("Early Rhaetian")
  bival[which(bival$formation=="Nayband"&bival$member%in%c("Howz-e Khan","Howz-e-Khan")),"late_interval"]<-c("Early Rhaetian")
  bival[which(bival$formation=="Nayband"&bival$member=="Howz-e-Sheikh"),"early_interval"]<-c("Sevatian")
  bival[which(bival$formation=="Nayband"&bival$member=="Howz-e-Sheikh"),"late_interval"]<-c("Early Rhaetian")
  bival[which(bival$formation=="Nayband"&bival$early_interval=="Norian"),"early_interval"]<-c("Alaunian")
  bival[which(bival$formation=="Nayband"&bival$late_interval=="Norian"),"late_interval"]<-c("Sevatian") #Sabbaghiyan et al. 2020

  bival[which(bival$formation=="Nekuchan"&bival$early_interval=="Changhsingian"),"early_interval"]<-c("Late Changhsingian")
  bival[which(bival$formation=="Nekuchan"&bival$late_interval=="Changhsingian"),"late_interval"]<-c("Late Changhsingian") #Biakov et al. 2018

  bival[which(bival$formation=="Nenyugin"),"early_interval"]<-c("Late Roadian")
  bival[which(bival$formation=="Nenyugin"),"late_interval"]<-c("Wordian") #Biakov 2013

  bival[bival$collection_no==110234,"early_interval"]<-c("Callovian")
  bival[bival$collection_no==110234,"late_interval"]<-c("Callovian") #Upper Member, Nieniexiongla Formation, Callovian according to Sun et al. 2017

  bival[which(bival$formation=="Nikitin"),"early_interval"]<-c("Late Changhsingian")
  bival[which(bival$formation=="Nikitin"),"late_interval"]<-c("Late Changhsingian") #"Late Changhsingian" in PBDB

  bival[which(bival$formation=="Nishinakayama"&bival$early_interval=="Pliensbachian"),"early_interval"]<-c("Late Pliensbachian")
  bival[which(bival$formation=="Nishinakayama"&bival$late_interval=="Pliensbachian"),"late_interval"]<-c("Late Pliensbachian")
  bival[which(bival$formation=="Nishinakayama"&bival$early_interval=="Toarcian"),"early_interval"]<-c("Early Toarcian")
  bival[which(bival$formation=="Nishinakayama"&bival$late_interval=="Toarcian"),"late_interval"]<-c("Early Toarcian") #Nakada et al. 2021

  bival[which(bival$formation=="Nordenskj枚ld"),"early_interval"]<-c("Late Tithonian")
  bival[which(bival$formation=="Nordenskj枚ld"),"late_interval"]<-c("Late Tithonian") #Nordenskjöld Formation. Ameghino Member

  bival[which(bival$formation=="Nothe Grits"),"early_interval"]<-c("Plicatilis")
  bival[which(bival$formation=="Nothe Grits"),"late_interval"]<-c("Plicatilis") #According to ammonoid zone

  bival[which(bival$formation=="Nunuluka"),"early_interval"]<-c("Sevatian")
  bival[which(bival$formation=="Nunuluka"),"late_interval"]<-c("Sevatian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Nusplingen Limestone"),"early_interval"]<-c("Beckeri")
  bival[which(bival$formation=="Nusplingen Limestone"),"late_interval"]<-c("Beckeri") #According to ammonoid zone

  bival[which(bival$formation=="Oberrhaet"),"early_interval"]<-c("Late Rhaetian")
  bival[which(bival$formation=="Oberrhaet"),"late_interval"]<-c("Late Rhaetian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Ohautira"&bival$early_interval=="Toarcian"),"early_interval"]<-c("Bajocian")
  bival[which(bival$formation=="Ohautira"&bival$late_interval=="Hettangian"),"late_interval"]<-c("Callovian") #"Late Temaikan" in PBDB

  bival[which(bival$formation=="Ohautira Conglomerate"),"early_interval"]<-c("Bajocian")
  bival[which(bival$formation=="Ohautira Conglomerate"),"late_interval"]<-c("Callovian") #"Late Temaikan" in PBDB

  bival[which(bival$formation=="Ohinereru"),"early_interval"]<-c("Late Oxfordian")
  bival[which(bival$formation=="Ohinereru"),"late_interval"]<-c("Early Kimmeridgian") #"Middle Heterian" in PBDB

  bival[which(bival$collection_no%in%c("35723","56292","63763","63764","133848","134103","134110","134127","134135","182757","182758","182759","182760","182784")),"early_interval"]<-c("Late Oxfordian")
  bival[which(bival$collection_no%in%c("35723","56292","63763","63764","133848","134103","134110","134127","134135","182757","182758","182759","182760","182784")),"late_interval"]<-c("Early Kimmeridgian") #"Middle Heterian"
  bival[which(bival$collection_no%in%c("63745","63746","63747","63749")),"early_interval"]<-c("Oxfordian")
  bival[which(bival$collection_no%in%c("63745","63746","63747","63749")),"late_interval"]<-c("Oxfordian") #"Lower Heterian"
  bival[which(bival$collection_no%in%c("182718","182719","182738")),"early_interval"]<-c("Late Oxfordian")
  bival[which(bival$collection_no%in%c("182718","182719","182738")),"late_interval"]<-c("Kimmeridgian") #"Middle-Late Heterian"
  bival[which(bival$collection_no%in%c("63766","63765")),"early_interval"]<-c("Tithonian")
  bival[which(bival$collection_no%in%c("63766","63765")),"late_interval"]<-c("Tithonian") #"Puaroan (Waikatoan substage)". These assignments are based on the correlation of fossils, not the absolute ages of the New Zealand regional stages

  bival[which(bival$formation=="Olinala"),"early_interval"]<-c("Late Capitanian")
  bival[which(bival$formation=="Olinala"),"late_interval"]<-c("Late Capitanian") #"Late Capitanian" in PBDB

  bival[which(bival$formation=="Olynian"),"early_interval"]<-c("Late Roadian")
  bival[which(bival$formation=="Olynian"),"late_interval"]<-c("Late Roadian") #Kolymia plicata bivalve zone, late Kazanian

  bival[which(bival$formation=="Omchak"&bival$early_interval=="Wuchiapingian"),"early_interval"]<-c("Early Wuchiapingian")
  bival[which(bival$formation=="Omchak"&bival$late_interval=="Wuchiapingian"),"late_interval"]<-c("Early Wuchiapingian")
  bival[which(bival$formation=="Omchak"&bival$early_interval=="Capitanian"),"early_interval"]<-c("Late Capitanian")
  bival[which(bival$formation=="Omchak"&bival$late_interval=="Capitanian"),"late_interval"]<-c("Late Capitanian") #Biakov 2013

  bival[which(bival$formation=="Oolite Marl"),"early_interval"]<-c("Aalenian")
  bival[which(bival$formation=="Oolite Marl"),"late_interval"]<-c("Aalenian") #Barron et al. 2012

  bival[which(bival$formation=="Oolithe de Trouville"),"early_interval"]<-c("Plicatilis")
  bival[which(bival$formation=="Oolithe de Trouville"),"late_interval"]<-c("Plicatilis") #According to ammonoid zone

  bival[which(bival$formation=="Oraka"),"early_interval"]<-c("Late Oxfordian")
  bival[which(bival$formation=="Oraka"),"late_interval"]<-c("Early Kimmeridgian") #"Middle Heterian" in PBDB

  bival[which(bival$formation=="Orn"&bival$early_interval=="Asselian"),"early_interval"]<-c("Early Asselian")
  bival[which(bival$formation=="Orn"&bival$late_interval=="Asselian"),"late_interval"]<-c("Early Asselian") #"Early Asselian" in PBDB
  bival[which(bival$formation=="Orn"&bival$early_interval=="Sakmarian"),"early_interval"]<-c("Early Sakmarian")
  bival[which(bival$formation=="Orn"&bival$late_interval=="Sakmarian"),"late_interval"]<-c("Early Sakmarian") #"Early Sakmarian" in PBDB

  bival[which(bival$formation=="Oro"),"early_interval"]<-c("Olenekian")
  bival[which(bival$formation=="Oro"),"late_interval"]<-c("Olenekian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Osgodby"&bival$member=="Redcliff"),"early_interval"]<-c("Koenigi")
  bival[which(bival$formation=="Osgodby"&bival$member=="Redcliff"),"late_interval"]<-c("Koenigi")
  bival[which(bival$formation=="Osgodby"&bival$member=="Hackness Rock"),"early_interval"]<-c("Lamberti")
  bival[which(bival$formation=="Osgodby"&bival$member=="Hackness Rock"),"late_interval"]<-c("Lamberti") #According to ammonoid zone

  bival[which(bival$formation=="Osipa"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Osipa"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Osmington Oolite"),"early_interval"]<-c("Transversarium")
  bival[which(bival$formation=="Osmington Oolite"),"late_interval"]<-c("Transversarium") #According to ammonoid zone

  bival[which(bival$formation=="Osobb"),"early_interval"]<-c("Lacian")
  bival[which(bival$formation=="Osobb"),"late_interval"]<-c("Lacian") #According to the original reference

  bival[which(bival$formation=="Otaniyama"),"early_interval"]<-c("Berriasian")
  bival[which(bival$formation=="Otaniyama"),"late_interval"]<-c("Berriasian") #Haggart and Matsukawa 2020

  bival[which(bival$formation=="Ozerninskaya"),"early_interval"]<-c("Late Kungurian")
  bival[which(bival$formation=="Ozerninskaya"),"late_interval"]<-c("Late Kungurian") #Kotlyar et al. 2018

  bival[which(bival$collection_no%in%c("225540","225541","225542","225546","225547","225551","225552","225554","225555","225556")),"early_interval"]<-c("Spinatum")
  bival[which(bival$collection_no%in%c("225540","225541","225542","225546","225547","225551","225552","225554","225555","225556")),"late_interval"]<-c("Spinatum") #According to ammonoid zone

  bival[which(bival$collection_no%in%c("225543","225548")),"early_interval"]<-c("Tenuicostatum")
  bival[which(bival$collection_no%in%c("225543","225548")),"late_interval"]<-c("Tenuicostatum") #According to ammonoid zone

  bival[which(bival$collection_no%in%c("225544","225549")),"early_interval"]<-c("Falciferum")
  bival[which(bival$collection_no%in%c("225544","225549")),"late_interval"]<-c("Falciferum") #According to ammonoid zone

  bival[which(bival$collection_no%in%c("225545","225550")),"early_interval"]<-c("Bifrons")
  bival[which(bival$collection_no%in%c("225545","225550")),"late_interval"]<-c("Bifrons") #According to ammonoid zone

  bival[which(bival$collection_no==225553),"early_interval"]<-c("Margaritatus")
  bival[which(bival$collection_no==225553),"late_interval"]<-c("Margaritatus") #According to ammonoid zone

  bival[which(bival$formation=="Painten"),"early_interval"]<-c("Beckeri")
  bival[which(bival$formation=="Painten"),"late_interval"]<-c("Beckeri") #According to ammonoid zone

  bival[bival$collection_no%in%c("47801","47905","47907","47908","47913"),"early_interval"]<-c("Wuchiapingian")
  bival[bival$collection_no%in%c("47801","47905","47907","47908","47913"),"late_interval"]<-c("Wuchiapingian")
  bival[bival$collection_no%in%c("47929","47958","47968","48000"),"early_interval"]<-c("Changhsingian")
  bival[bival$collection_no%in%c("47929","47958","47968","48000"),"late_interval"]<-c("Changhsingian") #Pamucak Formation. Ages following Angioloni et al. 2007

  bival[which(bival$collection_no==210188),"early_interval"]<-c("Planorbis")
  bival[which(bival$collection_no==210188),"late_interval"]<-c("Planorbis") #According to ammonoid zone

  bival[which(bival$formation=="Pangjang"),"early_interval"]<-c("Griesbachian")
  bival[which(bival$formation=="Pangjang"),"late_interval"]<-c("Griesbachian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Pardonet"&bival$early_interval=="Carnian"),"early_interval"]<-c("Tuvalian")
  bival[which(bival$formation=="Pardonet"&bival$late_interval=="Carnian"),"late_interval"]<-c("Tuvalian")
  bival[which(bival$formation=="Pardonet"&bival$early_interval=="Julian"),"early_interval"]<-c("Tuvalian")
  bival[which(bival$formation=="Pardonet"&bival$late_interval=="Julian"),"late_interval"]<-c("Tuvalian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Park City"&bival$member=="Grandeur"),"early_interval"]<-c("Kungurian")
  bival[which(bival$formation=="Park City"&bival$member=="Grandeur"),"late_interval"]<-c("Kungurian")
  bival[which(bival$formation=="Park City"&bival$member=="Ervay"),"early_interval"]<-c("Late Wuchiapingian")
  bival[which(bival$formation=="Park City"&bival$member=="Ervay"),"late_interval"]<-c("Early Changhsingian")
  bival[which(bival$formation=="Park City"&bival$member=="Franson"),"early_interval"]<-c("Early Wuchiapingian")
  bival[which(bival$formation=="Park City"&bival$member=="Franson"),"late_interval"]<-c("Early Wuchiapingian") #Wistort and Ritterbush 2022

  bival[which(bival$formation=="Passage Beds"&bival$cc=="UK"),"early_interval"]<-c("Cordatum")
  bival[which(bival$formation=="Passage Beds"&bival$cc=="UK"),"late_interval"]<-c("Cordatum") #According to ammonoid zone

  bival[which(bival$formation=="Pastakh"),"early_interval"]<-c("Spathian")
  bival[which(bival$formation=="Pastakh"),"late_interval"]<-c("Spathian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Pastanakhskaya/Ystannakhskaya"),"early_interval"]<-c("Spathian")
  bival[which(bival$formation=="Pastanakhskaya/Ystannakhskaya"),"late_interval"]<-c("Spathian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Payand茅"),"early_interval"]<-c("Alaunian")
  bival[which(bival$formation=="Payand茅"),"late_interval"]<-c("Sevatian") #The formation name should be Payandé. Mojica and Prinz-Grimm 2000

  bival[which(bival$collection_no==204106),"early_interval"]<-c("Murchisonae")
  bival[which(bival$collection_no==204106),"late_interval"]<-c("Murchisonae") #Barron et al. 2012

  bival[which(bival$formation=="Pechishchi"),"early_interval"]<-c("Late Roadian")
  bival[which(bival$formation=="Pechishchi"),"late_interval"]<-c("Late Roadian") #"Upper Kazanian" in PBDB

  bival[which(bival$formation=="Peschanka"),"early_interval"]<-c("Lacian")
  bival[which(bival$formation=="Peschanka"),"late_interval"]<-c("Alaunian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Pha Daeng"),"early_interval"]<-c("Carnian")
  bival[which(bival$formation=="Pha Daeng"),"late_interval"]<-c("Carnian") #Feng et al. 2005

  bival[which(bival$formation=="Philippovian"),"early_interval"]<-c("Early Kungurian")
  bival[which(bival$formation=="Philippovian"),"late_interval"]<-c("Early Kungurian") #"Lower Kungurian" in PBDB

  bival[which(bival$formation=="Phosphoria"&bival$member=="Ervay"),"early_interval"]<-c("Late Wuchiapingian")
  bival[which(bival$formation=="Phosphoria"&bival$member=="Ervay"),"late_interval"]<-c("Early Changhsingian")
  bival[which(bival$formation=="Phosphoria"&bival$member=="Tosi Chert"),"early_interval"]<-c("Late Wuchiapingian")
  bival[which(bival$formation=="Phosphoria"&bival$member=="Tosi Chert"),"late_interval"]<-c("Early Changhsingian")
  bival[which(bival$formation=="Phosphoria"&bival$member=="Tosi"),"early_interval"]<-c("Late Wuchiapingian")
  bival[which(bival$formation=="Phosphoria"&bival$member=="Tosi"),"late_interval"]<-c("Early Changhsingian")
  bival[which(bival$formation=="Phosphoria"&bival$member=="Retor"),"early_interval"]<-c("Late Wuchiapingian")
  bival[which(bival$formation=="Phosphoria"&bival$member=="Retor"),"late_interval"]<-c("Late Wuchiapingian")
  bival[which(bival$formation=="Phosphoria"&bival$member=="Retort Phosphatic Shale"),"early_interval"]<-c("Late Wuchiapingian")
  bival[which(bival$formation=="Phosphoria"&bival$member=="Retort Phosphatic Shale"),"late_interval"]<-c("Late Wuchiapingian")
  bival[which(bival$formation=="Phosphoria"&bival$member=="Franson"),"early_interval"]<-c("Early Wuchiapingian")
  bival[which(bival$formation=="Phosphoria"&bival$member=="Franson"),"late_interval"]<-c("Early Wuchiapingian")
  bival[which(bival$formation=="Phosphoria"&bival$member=="Rex Chert"),"early_interval"]<-c("Early Wuchiapingian")
  bival[which(bival$formation=="Phosphoria"&bival$member=="Rex Chert"),"late_interval"]<-c("Early Wuchiapingian")
  bival[which(bival$formation=="Phosphoria"&bival$member=="Rex"),"early_interval"]<-c("Early Wuchiapingian")
  bival[which(bival$formation=="Phosphoria"&bival$member=="Rex"),"late_interval"]<-c("Early Wuchiapingian") #Wistort and Ritterbush 2022

  bival[which(bival$collection_no%in%c("10701","10702")),"early_interval"]<-c("Ibex")
  bival[which(bival$collection_no%in%c("10701","10702")),"late_interval"]<-c("Davoei")  #According to ammonoid zone

  bival[which(bival$collection_no%in%c("33850","33851","33853","33855","36896")),"early_interval"]<-c("Davoei")
  bival[which(bival$collection_no%in%c("33850","33851","33853","33855","36896")),"late_interval"]<-c("Spinatum")  #According to ammonoid zone

  bival[which(bival$formation=="Pionerskii"),"early_interval"]<-c("Wordian")
  bival[which(bival$formation=="Pionerskii"),"late_interval"]<-c("Wordian") #Kolymia multiformis Zone, Wordian

  bival[which(bival$formation=="Piqiang"&bival$early_interval=="Artinskian"),"early_interval"]<-c("Early Artinskian")
  bival[which(bival$formation=="Piqiang"&bival$late_interval=="Artinskian"),"late_interval"]<-c("Early Artinskian") #"Early Artinskian" in PBDB

  bival[which(bival$formation=="Pit"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Pit"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Portland Limestone"),"early_interval"]<-c("Anguiformis")
  bival[which(bival$formation=="Portland Limestone"),"late_interval"]<-c("Anguiformis") #According to ammonoid zone

  bival[which(bival$formation=="Pounawea"),"early_interval"]<-c("Early Callovian")
  bival[which(bival$formation=="Pounawea"),"late_interval"]<-c("Middle Callovian") #Gardner and Campbell 2002. "Molluscan taxa from Pounawea Formation have strong Callovian affinities."

  bival[which(bival$formation=="Prikazan"),"early_interval"]<-c("Late Roadian")
  bival[which(bival$formation=="Prikazan"),"late_interval"]<-c("Late Roadian") #Upper Kazanian

  bival[which(bival$formation=="Privolninskaya"),"early_interval"]<-c("Late Changhsingian")
  bival[which(bival$formation=="Privolninskaya"),"late_interval"]<-c("Late Changhsingian") #Biakov and Kutygin 2021

  bival[which(bival$formation=="Privolynin"),"early_interval"]<-c("Late Wuchiapingian")
  bival[which(bival$formation=="Privolynin"),"late_interval"]<-c("Changhsingian") #Davydov et al. 2022

  bival[which(bival$formation=="Pueblo"),"early_interval"]<-c("Asselian")
  bival[which(bival$formation=="Pueblo"),"late_interval"]<-c("Asselian") #Lucas 2018

  bival[which(bival$formation=="Putnam"),"early_interval"]<-c("Early Artinskian")
  bival[which(bival$formation=="Putnam"),"late_interval"]<-c("Early Artinskian") #Lucas 2018. Coleman Junction ls

  bival[which(bival$formation=="Qieerma"),"early_interval"]<-c("Pelsonian")
  bival[which(bival$formation=="Qieerma"),"late_interval"]<-c("Illyrian") #Upper part of the Anisian Junzihe Group

  bival[which(bival$formation=="Qingyan"),"early_interval"]<-c("Pelsonian")
  bival[which(bival$formation=="Qingyan"),"late_interval"]<-c("Illyrian") #These collections were from the Leidapo and Yuqing members of the middle-upper Anisian. The Yingshangpo records of Chen J. et al. 2010 will be discarded

  bival[which(bival$formation=="Qipan"&bival$early_interval=="Artinskian"),"early_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Qipan"&bival$late_interval=="Artinskian"),"late_interval"]<-c("Late Artinskian") #According to brachiopod zone

  bival[which(bival$collection_no%in%62740:62753),"early_interval"]<-c("Early Kungurian")
  bival[which(bival$collection_no%in%62740:62753),"late_interval"]<-c("Early Kungurian")
  bival[which(bival$collection_no%in%62757:62764),"early_interval"]<-c("Late Kungurian")
  bival[which(bival$collection_no%in%62757:62764),"late_interval"]<-c("Late Kungurian") #According to brachiopod zone

  bival[which(bival$formation=="Qixia"&bival$early_interval=="Artinskian"),"early_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Qixia"&bival$late_interval=="Artinskian"),"late_interval"]<-c("Late Artinskian") #Shen et al. 2021

  bival[which(bival$formation=="Qubuerga"&bival$ref_author=="Xu et al."),"early_interval"]<-c("Wuchiapingian")
  bival[which(bival$formation=="Qubuerga"&bival$ref_author=="Xu et al."),"late_interval"]<-c("Early Changhsingian") #According to the original reference

  bival[which(bival$formation=="Qudi"),"early_interval"]<-c("Artinskian")
  bival[which(bival$formation=="Qudi"),"late_interval"]<-c("Artinskian") #Following Shen et al. 2021

  bival[which(bival$formation=="Qulonggongba"),"early_interval"]<-c("Alaunian")
  bival[which(bival$formation=="Qulonggongba"),"late_interval"]<-c("Alaunian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Quy Lang"),"early_interval"]<-c("Pelsonian")
  bival[which(bival$formation=="Quy Lang"),"late_interval"]<-c("Pelsonian") #Shigeta et al. 2010

  bival[which(bival$formation=="Ramla"),"early_interval"]<-c("Early Bathonian")
  bival[which(bival$formation=="Ramla"),"late_interval"]<-c("Early Bathonian") #Ahmad et al. 2017

  bival[which(bival$formation=="Ramsau Dolomite"&bival$early_interval=="Carnian"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Ramsau Dolomite"&bival$late_interval=="Carnian"),"late_interval"]<-c("Julian") #Missoni and Gawlick 2011

  bival[which(bival$formation=="Rat Buri"),"early_interval"]<-c("Roadian")
  bival[which(bival$formation=="Rat Buri"),"late_interval"]<-c("Roadian") #Waterhouse 2013

  bival[which(bival$formation=="Red Eagle Limestone"),"early_interval"]<-c("Early Asselian")
  bival[which(bival$formation=="Red Eagle Limestone"),"late_interval"]<-c("Early Asselian") #Wahlman and West 2010

  bival[which(bival$formation=="Redcar Mudstone"),"early_interval"]<-c("Late Rhaetian")
  bival[which(bival$formation=="Redcar Mudstone"),"late_interval"]<-c("Late Rhaetian") #The original reference

  bival[which(bival$formation=="Reiflingerkalk"),"early_interval"]<-c("Illyrian")
  bival[which(bival$formation=="Reiflingerkalk"),"late_interval"]<-c("Illyrian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Renggeri Marl"),"early_interval"]<-c("Mariae")
  bival[which(bival$formation=="Renggeri Marl"),"late_interval"]<-c("Transversarium") #According to ammonoid zone

  bival[which(bival$formation=="Reshetnikovo"),"early_interval"]<-c("Late Kungurian")
  bival[which(bival$formation=="Reshetnikovo"),"late_interval"]<-c("Early Roadian") #Kotlyar 2015

  bival[which(bival$formation=="Rezi Dolomite"),"early_interval"]<-c("Sevatian")
  bival[which(bival$formation=="Rezi Dolomite"),"late_interval"]<-c("Sevatian") #Haas et al. 2022

  bival[which(bival$formation=="Richthofen"),"early_interval"]<-c("Illyrian")
  bival[which(bival$formation=="Richthofen"),"late_interval"]<-c("Illyrian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Ringstead Coral Bed"),"early_interval"]<-c("Baylei")
  bival[which(bival$formation=="Ringstead Coral Bed"),"late_interval"]<-c("Baylei") #According to ammonoid zone

  bival[which(bival$formation=="Riva di Solto"),"early_interval"]<-c("Sevatian")
  bival[which(bival$formation=="Riva di Solto"),"late_interval"]<-c("Sevatian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Road"&bival$early_interval=="Roadian"),"early_interval"]<-c("Early Roadian")
  bival[which(bival$formation=="Road"&bival$late_interval=="Roadian"),"late_interval"]<-c("Early Roadian")
  bival[which(bival$formation=="Road Canyon"&bival$early_interval=="Roadian"),"early_interval"]<-c("Early Roadian")
  bival[which(bival$formation=="Road Canyon"&bival$late_interval=="Roadian"),"late_interval"]<-c("Early Roadian")
  bival[which(bival$formation=="Road Canyon"&bival$early_interval=="Kungurian"),"early_interval"]<-c("Late Kungurian")
  bival[which(bival$formation=="Road Canyon"&bival$late_interval=="Kungurian"),"late_interval"]<-c("Late Kungurian") #Menning 2006

  bival[which(bival$formation=="Roca Shale"),"early_interval"]<-c("Early Asselian")
  bival[which(bival$formation=="Roca Shale"),"late_interval"]<-c("Early Asselian") #Wahlman and West 2010

  bival[which(bival$formation=="Rodiles"&bival$early_interval=="Late Toarcian"),"early_interval"]<-c("Aalensis")
  bival[which(bival$formation=="Rodiles"&bival$late_interval=="Late Toarcian"),"late_interval"]<-c("Aalensis")
  bival[which(bival$formation=="Rodiles"&bival$early_interval=="Levesquei"),"early_interval"]<-c("Pseudoradiosa")
  bival[which(bival$formation=="Rodiles"&bival$late_interval=="Levesquei"),"late_interval"]<-c("Pseudoradiosa") #According to ammonoid zone

  bival[which(bival$formation=="Rustler"),"early_interval"]<-c("Early Changhsingian")
  bival[which(bival$formation=="Rustler"),"late_interval"]<-c("Early Changhsingian") #"Early Changhsingian" in PBDB

  bival[which(bival$formation=="Sadlerochit"),"early_interval"]<-c("Roadian")
  bival[which(bival$formation=="Sadlerochit"),"late_interval"]<-c("Roadian") #Lee et al. 2019

  bival[which(bival$formation=="Sakhana"),"early_interval"]<-c("Ufimian")
  bival[which(bival$formation=="Sakhana"),"late_interval"]<-c("Ufimian") #According to the original reference

  bival[which(bival$formation=="Salmon River"),"early_interval"]<-c("Late Toarcian")
  bival[which(bival$formation=="Salmon River"),"late_interval"]<-c("Early Bajocian") #Gagnon et al. 2012

  bival[which(bival$formation=="Salperton Limestone"&bival$early_interval=="Bajocian"),"early_interval"]<-c("Late Bajocian")
  bival[which(bival$formation=="Salperton Limestone"&bival$late_interval=="Bajocian"),"late_interval"]<-c("Late Bajocian") #Barron et al. 2012

  bival[which(bival$formation=="San Cassiano"&bival$early_interval=="Ladinian"),"early_interval"]<-c("Longobardian")
  bival[which(bival$formation=="San Cassiano"&bival$late_interval=="Ladinian"),"late_interval"]<-c("Longobardian") #"Uppermost Ladinian"
  bival[which(bival$formation=="San Cassiano"&bival$early_interval=="Carnian"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="San Cassiano"&bival$late_interval=="Carnian"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="San Hipolito"&bival$member=="Limestone"),"early_interval"]<-c("Sevatian")
  bival[which(bival$formation=="San Hipolito"&bival$member=="Limestone"),"late_interval"]<-c("Sevatian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$collection_no==1573),"early_interval"]<-c("Cautisnigrae")
  bival[which(bival$collection_no==1573),"late_interval"]<-c("Cautisnigrae") #According to brachiopod zone
  bival[which(bival$collection_no%in%1567:1568),"early_interval"]<-c("Pseudocordata")
  bival[which(bival$collection_no%in%1567:1568),"late_interval"]<-c("Pseudocordata") #According to brachiopod zone

  bival[which(bival$formation=="Sanhedong"),"early_interval"]<-c("Lacian")
  bival[which(bival$formation=="Sanhedong"),"late_interval"]<-c("Lacian") #Wu et al. 2022

  bival[which(bival$formation=="Sanqiao"),"early_interval"]<-c("Tuvalian")
  bival[which(bival$formation=="Sanqiao"),"late_interval"]<-c("Norian") #Carnian in old references, Norian in Tong et al. 2021

  bival[which(bival$formation=="Santa Clara"),"early_interval"]<-c("Tuvalian")
  bival[which(bival$formation=="Santa Clara"),"late_interval"]<-c("Tuvalian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Santa Linya"&bival$early_interval=="Late Toarcian"),"early_interval"]<-c("Aalensis") #"Toarcian-Aalenian boundary beds"

  bival[which(bival$collection_no%in%225387:225388),"early_interval"]<-c("Tenuicostatum")
  bival[which(bival$collection_no%in%225387:225388),"late_interval"]<-c("Tenuicostatum")
  bival[which(bival$collection_no==225389),"early_interval"]<-c("Serpentinum")
  bival[which(bival$collection_no==225389),"late_interval"]<-c("Serpentinum") #According to ammonoid zone

  bival[which(bival$formation=="Sarga"),"early_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Sarga"),"late_interval"]<-c("Late Artinskian") #Naugolnykh 2020

  bival[which(bival$formation=="Savina"),"early_interval"]<-c("Late Capitanian")
  bival[which(bival$formation=="Savina"),"late_interval"]<-c("Late Capitanian") #See Biakov 2013

  bival[which(bival$formation=="Sawtooth"&bival$early_interval=="Bajocian"),"early_interval"]<-c("Late Bajocian")
  bival[which(bival$formation=="Sawtooth"&bival$late_interval=="Bajocian"),"late_interval"]<-c("Late Bajocian")
  bival[which(bival$formation=="Sawtooth"&bival$early_interval=="Bathonian"),"early_interval"]<-c("Early Bathonian")
  bival[which(bival$formation=="Sawtooth"&bival$late_interval=="Bathonian"),"late_interval"]<-c("Early Bathonian") #Parcell and Williams 2005

  bival[which(bival$formation=="Scarborough"),"early_interval"]<-c("Humphriesianum")
  bival[which(bival$formation=="Scarborough"),"late_interval"]<-c("Humphriesianum") #Barron et al. 2012

  bival[which(bival$formation=="Schlern 1"),"early_interval"]<-c("Illyrian")
  bival[which(bival$formation=="Schlern 1"),"late_interval"]<-c("Fassanian")
  bival[which(bival$collection_no==49915),"early_interval"]<-c("Illyrian")
  bival[which(bival$collection_no==49915),"late_interval"]<-c("Illyrian") #According to the original reference

  bival[which(bival$formation=="Schlern"&bival$ref_author=="Friesenbichler et al."),"early_interval"]<-c("Longobardian")
  bival[which(bival$formation=="Schlern"&bival$ref_author=="Friesenbichler et al."),"late_interval"]<-c("Longobardian")
  bival[which(bival$collection_no%in%202042:202043),"early_interval"]<-c("Julian")
  bival[which(bival$collection_no%in%202042:202043),"late_interval"]<-c("Julian") #According to the original reference

  bival[which(bival$formation=="Schn枚ll"),"early_interval"]<-c("Hettangian")
  bival[which(bival$formation=="Schn枚ll"),"late_interval"]<-c("Hettangian")
  bival[which(bival$formation=="Sch枚ll"),"early_interval"]<-c("Hettangian")
  bival[which(bival$formation=="Sch枚ll"),"late_interval"]<-c("Hettangian") #The formation name should be Schnöll. According to the original reference

  bival[which(bival$formation=="Schuchert Dal"),"early_interval"]<-c("Changhsingian")
  bival[which(bival$formation=="Schuchert Dal"),"late_interval"]<-c("Changhsingian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="scissum Beds"),"early_interval"]<-c("Early Aalenian")
  bival[which(bival$formation=="scissum Beds"),"late_interval"]<-c("Early Aalenian") #Barron et al. 2012

  bival[which(bival$formation=="Sedrina"&bival$early_interval=="Early Hettangian"),"early_interval"]<-c("Planorbis")
  bival[which(bival$formation=="Sedrina"&bival$late_interval=="Early Hettangian"),"late_interval"]<-c("Planorbis")
  bival[which(bival$formation=="Sedrina"&bival$early_interval=="Late Hettangian"),"early_interval"]<-c("Angulata")
  bival[which(bival$formation=="Sedrina"&bival$late_interval=="Late Hettangian"),"late_interval"]<-c("Angulata") #According to ammonoid zone

  bival[which(bival$formation=="Selander"),"early_interval"]<-c("Wuchiapingian")
  bival[which(bival$formation=="Selander"),"late_interval"]<-c("Wuchiapingian") #Blomeier et al. 2013

  bival[which(bival$formation=="Senja"),"early_interval"]<-c("Early Changhsingian")
  bival[which(bival$formation=="Senja"),"late_interval"]<-c("Early Changhsingian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Seven Rivers"),"early_interval"]<-c("Early Capitanian")
  bival[which(bival$formation=="Seven Rivers"),"late_interval"]<-c("Early Capitanian") #Nestell and Nestell 2006

  bival[which(bival$formation=="Shalem Colony"),"early_interval"]<-c("Early Asselian")
  bival[which(bival$formation=="Shalem Colony"),"late_interval"]<-c("Early Asselian") #Lucas et al. 2015

  bival[which(bival$formation=="Shedhorn"),"early_interval"]<-c("Wuchiapingian")
  bival[which(bival$formation=="Shedhorn"),"late_interval"]<-c("Early Changhsingian")
  bival[which(bival$formation=="Shedhorn"&bival$member=="Lower"),"early_interval"]<-c("Early Wuchiapingian")
  bival[which(bival$formation=="Shedhorn"&bival$member=="Lower"),"late_interval"]<-c("Early Wuchiapingian")
  bival[which(bival$formation=="Shedhorn"&bival$member=="Upper"),"early_interval"]<-c("Late Wuchiapingian")
  bival[which(bival$formation=="Shedhorn"&bival$member=="Upper"),"late_interval"]<-c("Early Changhsingian") #Wistort and Ritterbush 2022

  bival[which(bival$formation=="Sherkirtinskie"),"early_interval"]<-c("Ufimian")
  bival[which(bival$formation=="Sherkirtinskie"),"late_interval"]<-c("Ufimian") #"Ufimian" in PBDB

  bival[which(bival$formation=="Sheshma"),"early_interval"]<-c("Ufimian")
  bival[which(bival$formation=="Sheshma"),"late_interval"]<-c("Ufimian") #"Ufimian" in PBDB

  bival[which(bival$formation=="Sheshmian"),"early_interval"]<-c("Ufimian")
  bival[which(bival$formation=="Sheshmian"),"late_interval"]<-c("Early Roadian") #"Ufimian/Lower Kazanian" in PBDB

  bival[which(bival$formation%in%c("Shikhan","Shikhansk")),"early_interval"]<-c("Late Asselian")
  bival[which(bival$formation%in%c("Shikhan","Shikhansk")),"late_interval"]<-c("Late Asselian") #Nassichuk 1995; GTS2020

  bival[which(bival$formation=="Shinatani"),"early_interval"]<-c("Early Toarcian")
  bival[which(bival$formation=="Shinatani"),"late_interval"]<-c("Middle Toarcian") #Nakada et al. 2021. The overlying Otakidani Formation is Late Toarcian in age

  bival[which(bival$formation=="Shionosawa Limestone"),"early_interval"]<-c("Smithian")
  bival[which(bival$formation=="Shionosawa Limestone"),"late_interval"]<-c("Smithian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Shugurovo"),"early_interval"]<-c("Ufimian")
  bival[which(bival$formation=="Shugurovo"),"late_interval"]<-c("Early Roadian") #"Ufimian/Lower Kazanian" in PBDB

  bival[which(bival$formation=="Shuizhutang"),"early_interval"]<-c("Late Wuchiapingian")
  bival[which(bival$formation=="Shuizhutang"),"late_interval"]<-c("Late Wuchiapingian") #Between Wangpanli and Longtan formations

  bival[which(bival$formation=="Shurtan"),"early_interval"]<-c("Early Kungurian")
  bival[which(bival$formation=="Shurtan"),"late_interval"]<-c("Early Kungurian") #"Lower Kungurian" in PBDB

  bival[which(bival$formation=="Skinner Ranch"&bival$early_interval=="Artinskian"),"early_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Skinner Ranch"&bival$late_interval=="Artinskian"),"late_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Skinner Ranch"&bival$early_interval=="Kungurian"),"early_interval"]<-c("Early Kungurian")
  bival[which(bival$formation=="Skinner Ranch"&bival$late_interval=="Kungurian"),"late_interval"]<-c("Early Kungurian") #Menning 2006

  bival[which(bival$formation=="Smolegowa"),"early_interval"]<-c("Bajocian")
  bival[which(bival$formation=="Smolegowa"),"late_interval"]<-c("Bajocian") #Wierzbowski et al. 2004

  bival[which(bival$formation=="Sobral"&bival$early_interval=="Kimmeridgian"),"early_interval"]<-c("Late Kimmeridgian")
  bival[which(bival$formation=="Sobral"&bival$late_interval=="Kimmeridgian"),"late_interval"]<-c("Late Kimmeridgian") #Fürsich et al. 2022

  bival[which(bival$formation=="Sogur"),"early_interval"]<-c("Sakmarian")
  bival[which(bival$formation=="Sogur"),"late_interval"]<-c("Artinskian") #Klets et al. 2000

  bival[which(bival$formation=="Sokolin"),"early_interval"]<-c("Kungurian")
  bival[which(bival$formation=="Sokolin"),"late_interval"]<-c("Kungurian") #Roadian in PBDB. Bundikov et al. 2020

  bival[which(bival$formation=="Soktui"),"early_interval"]<-c("Wordian")
  bival[which(bival$formation=="Soktui"),"late_interval"]<-c("Early Capitanian") #Biakov 2002, 2003

  bival[which(bival$formation=="Solikamsk"),"early_interval"]<-c("Ufimian")
  bival[which(bival$formation=="Solikamsk"),"late_interval"]<-c("Ufimian") #"The layers are assigned to the Solikamsk horizon of the Ufimian"

  bival[which(bival$formation=="Spilsby Sandstone"),"early_interval"]<-c("Middle Volgian")
  bival[which(bival$formation=="Spilsby Sandstone"),"late_interval"]<-c("Middle Volgian") #"Middle Volgian" in PBDB
  bival[which(bival$collection_no==157894),"early_interval"]<-c("Middle Volgian")
  bival[which(bival$collection_no==157894),"late_interval"]<-c("Late Volgian") #"Middle TO upper Volgian" in PBDB

  bival[which(bival$formation=="Staffin Bay"),"early_interval"]<-c("Early Callovian")
  bival[which(bival$formation=="Staffin Bay"),"late_interval"]<-c("Early Callovian") #According to ammonoid zone

  bival[which(bival$collection_no==163707),"early_interval"]<-c("Middle Callovian")
  bival[which(bival$collection_no==163707),"late_interval"]<-c("Middle Callovian")
  bival[which(bival$collection_no==163721),"early_interval"]<-c("Plicatilis")
  bival[which(bival$collection_no==163721),"late_interval"]<-c("Plicatilis") #According to ammonoid zone

  bival[which(bival$formation=="Staratel"&bival$early_interval=="Wuchiapingian"),"early_interval"]<-c("Late Wuchiapingian")
  bival[which(bival$formation=="Staratel"&bival$late_interval=="Wuchiapingian"),"late_interval"]<-c("Late Wuchiapingian")
  bival[which(bival$formation=="Staratel"&bival$early_interval=="Changhsingian"),"early_interval"]<-c("Early Changhsingian")
  bival[which(bival$formation=="Staratel"&bival$late_interval=="Changhsingian"),"late_interval"]<-c("Early Changhsingian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="steinalm Limestones"),"early_interval"]<-c("Anisian")
  bival[which(bival$formation=="steinalm Limestones"),"late_interval"]<-c("Anisian")
  bival[which(bival$formation=="Steinalm Limestones"),"early_interval"]<-c("Anisian")
  bival[which(bival$formation=="Steinalm Limestones"),"late_interval"]<-c("Anisian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Stonesfield Slate"),"early_interval"]<-c("Middle Bathonian")
  bival[which(bival$formation=="Stonesfield Slate"),"late_interval"]<-c("Middle Bathonian") #Barron et al. 2012

  bival[which(bival$formation=="Strimbes Limestone"),"early_interval"]<-c("Sinemurian")
  bival[which(bival$formation=="Strimbes Limestone"),"late_interval"]<-c("Pliensbachian") #"Sinemurian-Pliensbachian" in PBDB

  bival[which(bival$formation=="Stuttgart"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Stuttgart"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Staratel"&bival$early_interval=="Wuchiapingian"),"early_interval"]<-c("Late Wuchiapingian")
  bival[which(bival$formation=="Staratel"&bival$late_interval=="Wuchiapingian"),"late_interval"]<-c("Late Wuchiapingian")

  bival[which(bival$formation=="Sungjar"),"early_interval"]<-c("Olenekian")
  bival[which(bival$formation=="Sungjar"),"late_interval"]<-c("Olenekian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Sullivan Peak"&bival$early_interval=="Kungurian"),"early_interval"]<-c("Early Kungurian")
  bival[which(bival$formation=="Sullivan Peak"&bival$late_interval=="Kungurian"),"late_interval"]<-c("Early Kungurian") #Menning 2006

  bival[which(bival$formation=="Sumra"&bival$early_interval=="Norian"),"early_interval"]<-c("Sevatian")
  bival[which(bival$formation=="Sumra"&bival$late_interval=="Norian"),"late_interval"]<-c("Sevatian") #Following Flannery-Sutherland et al. 2022. The age of this formation is uncertain. Some authors suggest it belongs to Rhaetian

  bival[which(bival$formation=="Sundance"&bival$member=="Stockade Beaver Shale"),"early_interval"]<-c("Late Bathonian")
  bival[which(bival$formation=="Sundance"&bival$member=="Stockade Beaver Shale"),"late_interval"]<-c("Late Bathonian") #Massare et al. 2014

  bival[which(bival$formation=="S茫o Gi茫o"&bival$early_interval=="Levesquei"),"early_interval"]<-c("Pseudoradiosa")
  bival[which(bival$formation=="S茫o Gi茫o"&bival$late_interval=="Levesquei"),"late_interval"]<-c("Pseudoradiosa") #The formation name should be São Gião. Levesquei should be a Late Toarcian zone, but the absolute age is Bajocian in fossilbrush

  bival[which(bival$formation=="Tak Fa"),"early_interval"]<-c("Kungurian")
  bival[which(bival$formation=="Tak Fa"),"late_interval"]<-c("Kungurian") #Xu et al. 2022

  bival[which(bival$formation=="Takakurayama"),"early_interval"]<-c("Wuchiapingian")
  bival[which(bival$formation=="Takakurayama"),"late_interval"]<-c("Wuchiapingian") #Tazawa et al. 2015

  bival[which(bival$formation=="Talpa"),"early_interval"]<-c("Early Kungurian")
  bival[which(bival$formation=="Talpa"),"late_interval"]<-c("Early Kungurian") #Lucas 2018. May be Late Artinskian

  bival[which(bival$formation=="Talung"&bival$ref_author=="Shen et al."),"early_interval"]<-c("Late Changhsingian")
  bival[which(bival$formation=="Talung"&bival$ref_author=="Shen et al."),"late_interval"]<-c("Late Changhsingian") #"The uppermost Changhsingian" in PBDB

  bival[which(bival$stratgroup=="Taungnyo"),"early_interval"]<-c("Late Kungurian")
  bival[which(bival$stratgroup=="Taungnyo"),"late_interval"]<-c("Late Kungurian") #Xu et al. 2022

  bival[which(bival$formation=="Tansill"),"early_interval"]<-c("Late Capitanian")
  bival[which(bival$formation=="Tansill"),"late_interval"]<-c("Late Capitanian") #Nestell and Nestell 2006

  bival[which(bival$formation=="Tartalinskaya"),"early_interval"]<-c("Spathian")
  bival[which(bival$formation=="Tartalinskaya"),"late_interval"]<-c("Spathian") #"Late Olenekian"

  bival[which(bival$formation=="Tastuba"),"early_interval"]<-c("Early Sakmarian")
  bival[which(bival$formation=="Tastuba"),"late_interval"]<-c("Early Sakmarian") #Kotlyar et al. 2018

  bival[which(bival$formation=="Tataouina"),"early_interval"]<-c("Callovian")
  bival[which(bival$formation=="Tataouina"),"late_interval"]<-c("Callovian")
  bival[which(bival$formation=="Tataouine"),"early_interval"]<-c("Callovian")
  bival[which(bival$formation=="Tataouine"),"late_interval"]<-c("Callovian") #Alméras et al., 2005

  bival[which(bival$formation=="Taynton Stone"),"early_interval"]<-c("Middle Bathonian")
  bival[which(bival$formation=="Taynton Stone"),"late_interval"]<-c("Middle Bathonian") #Barron et al. 2012

  bival[which(bival$formation=="Tazigzaout"),"late_interval"]<-c("Early Bathonian") #Ait Addi and Chafiki 2013

  bival[which(bival$formation=="Teradani"),"early_interval"]<-c("Late Pliensbachian")
  bival[which(bival$formation=="Teradani"),"late_interval"]<-c("Late Pliensbachian") #"Late Pliensbachian" in PBDB; see also Nakada et al. 2021

  bival[which(bival$formation=="Thaynes"&bival$early_interval=="Early Triassic"),"early_interval"]<-c("Olenekian")
  bival[which(bival$formation=="Thaynes"&bival$late_interval=="Early Triassic"),"late_interval"]<-c("Olenekian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Theoi Marls"),"early_interval"]<-c("Plicatilis")
  bival[which(bival$formation=="Theoi Marls"),"late_interval"]<-c("Plicatilis") #According to ammonoid zone

  bival[which(bival$formation=="Tiryakh"),"early_interval"]<-c("Early Capitanian")
  bival[which(bival$formation=="Tiryakh"),"late_interval"]<-c("Early Capitanian") #Roadian in PBDB, but these species were reported from the Capitanian according to Davydov et al. 2022

  bival[which(bival$formation=="Titan"&bival$early_interval=="Capitanian"),"early_interval"]<-c("Late Capitanian")
  bival[which(bival$formation=="Titan"&bival$late_interval=="Capitanian"),"late_interval"]<-c("Late Capitanian")
  bival[which(bival$formation=="Titan"&bival$early_interval=="Wuchiapingian"),"early_interval"]<-c("Early Wuchiapingian")
  bival[which(bival$formation=="Titan"&bival$late_interval=="Wuchiapingian"),"late_interval"]<-c("Early Wuchiapingian") #Biakov 2013

  bival[which(bival$formation=="Togotui"),"early_interval"]<-c("Late Capitanian")
  bival[which(bival$formation=="Togotui"),"late_interval"]<-c("Late Capitanian") #Biakov 2013

  bival[which(bival$formation=="Trigonia clavellata"),"early_interval"]<-c("Transversarium")
  bival[which(bival$formation=="Trigonia clavellata"),"late_interval"]<-c("Cautisnigrae") #According to ammonoid zone

  bival[which(bival$formation=="Trochitenkalk"),"early_interval"]<-c("Illyrian")
  bival[which(bival$formation=="Trochitenkalk"),"late_interval"]<-c("Illyrian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Trold Fiord"),"early_interval"]<-c("Wordian")
  bival[which(bival$formation=="Trold Fiord"),"late_interval"]<-c("Capitanian") #The ages are uncertain, may be Wordian-Wuchiapingian (Lee et al. 2019). Capitanian or Wuchiapingian in PBDB, but usually Wordian in systematic papers

  bival[which(bival$formation=="Tschermakfjellet"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Tschermakfjellet"),"late_interval"]<-c("Julian") #The lower member

  bival[which(bival$formation=="Tumul"&bival$early_interval=="Norian"),"early_interval"]<-c("Alaunian")
  bival[which(bival$formation=="Tumul"&bival$late_interval=="Norian"),"late_interval"]<-c("Alaunian") #According to bivalve zone

  bival[which(bival$formation=="Tunlonggongba"),"early_interval"]<-c("Kungurian")
  bival[which(bival$formation=="Tunlonggongba"),"late_interval"]<-c("Kungurian") #Following Shen et al. 2021; Xu et al. 2022

  bival[which(bival$formation=="Turmiel"&bival$early_interval=="Levesquei"),"early_interval"]<-c("Pseudoradiosa")
  bival[which(bival$formation=="Turmiel"&bival$late_interval=="Levesquei"),"late_interval"]<-c("Pseudoradiosa")
  bival[which(bival$formation=="Turmiel"&bival$early_interval=="Late Toarcian"),"early_interval"]<-c("Thouarsense")
  bival[which(bival$formation=="Turmiel"&bival$late_interval=="Late Toarcian"),"late_interval"]<-c("Thouarsense") #Fallaciosum subzone
  bival[which(bival$collection_no%in%c(211066:211068,211168:211177)),"early_interval"]<-c("Tenuicostatum")
  bival[which(bival$collection_no%in%c(211066:211068,211168:211177)),"late_interval"]<-c("Tenuicostatum")
  bival[which(bival$collection_no%in%c(211178:211180,211184:211211)),"early_interval"]<-c("Serpentinum")
  bival[which(bival$collection_no%in%c(211178:211180,211184:211211)),"late_interval"]<-c("Serpentinum")
  bival[which(bival$collection_no%in%c(211212:211216)),"early_interval"]<-c("Bifrons")
  bival[which(bival$collection_no%in%c(211212:211216)),"late_interval"]<-c("Bifrons")
  bival[which(bival$collection_no==51613),"early_interval"]<-c("Serpentinum")
  bival[which(bival$collection_no==51613),"late_interval"]<-c("Bifrons") #According to ammonoid zone

  bival[which(bival$formation=="Turuzov"&bival$early_interval=="Asselian"),"early_interval"]<-c("Kasimovian")
  bival[which(bival$formation=="Turuzov"&bival$late_interval=="Asselian"),"late_interval"]<-c("Kasimovian")
  bival[which(bival$formation=="Turuzov"&bival$early_interval=="Sakmarian"),"early_interval"]<-c("Gzhelian")
  bival[which(bival$formation=="Turuzov"&bival$late_interval=="Sakmarian"),"late_interval"]<-c("Gzhelian") #Asselian-Sakmarian in PBDB, but according to Budnikov et al. 2020, this formation should be Kasimovian-Gzhelian

  bival[which(bival$formation%in%c("Tuwaiq","Tuwaiq Mountain","Tuwaiq Mt.Lst.")&bival$early_interval=="Callovian"),"early_interval"]<-c("Middle Callovian")
  bival[which(bival$formation%in%c("Tuwaiq","Tuwaiq Mountain","Tuwaiq Mt.Lst.")&bival$late_interval=="Callovian"),"late_interval"]<-c("Late Callovian") #Alméras et al. 2010

  bival[which(bival$formation=="Tyaughton"&bival$early_interval=="Rhaetian"),"early_interval"]<-c("Early Rhaetian")
  bival[which(bival$formation=="Tyaughton"&bival$late_interval=="Rhaetian"),"late_interval"]<-c("Early Rhaetian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation%in%c("Ufimskiy","Ufim")),"early_interval"]<-c("Ufimian")
  bival[which(bival$formation%in%c("Ufimskiy","Ufim")),"late_interval"]<-c("Ufimian") #May be Ufimian

  bival[which(bival$collection_no%in%c(124544:124546)),"early_interval"]<-c("Aegean")
  bival[which(bival$collection_no%in%c(124544:124546)),"late_interval"]<-c("Aegean")
  bival[which(bival$collection_no%in%c(124547:124550,124574)),"early_interval"]<-c("Bithynian")
  bival[which(bival$collection_no%in%c(124547:124550,124574)),"late_interval"]<-c("Pelsonian")
  bival[which(bival$collection_no%in%c(124551:124553,124640:124652)),"early_interval"]<-c("Illyrian")
  bival[which(bival$collection_no%in%c(124551:124553,124640:124652)),"late_interval"]<-c("Illyrian")
  bival[which(bival$collection_no%in%c(124554:124556,124653:124655)),"early_interval"]<-c("Fassanian")
  bival[which(bival$collection_no%in%c(124554:124556,124653:124655)),"late_interval"]<-c("Fassanian")
  bival[which(bival$collection_no%in%c(124558:124564,124656:124661,124663:124669)),"early_interval"]<-c("Longobardian")
  bival[which(bival$collection_no%in%c(124558:124564,124656:124661,124663:124669)),"late_interval"]<-c("Longobardian")
  bival[which(bival$collection_no%in%c(124670:124675)),"early_interval"]<-c("Julian")
  bival[which(bival$collection_no%in%c(124670:124675)),"late_interval"]<-c("Julian") #According to the original reference

  bival[which(bival$formation=="Upper Calcareous Grit"),"early_interval"]<-c("Pumilus")
  bival[which(bival$formation=="Upper Calcareous Grit"),"late_interval"]<-c("Cautisnigrae") #According to ammonoid zone

  bival[which(bival$formation=="Upper Trigonia Grit"),"early_interval"]<-c("Late Bajocian")
  bival[which(bival$formation=="Upper Trigonia Grit"),"late_interval"]<-c("Late Bajocian") #Barron et al. 2012

  bival[which(bival$formation=="Urushten"),"early_interval"]<-c("Late Changhsingian")
  bival[which(bival$formation=="Urushten"),"late_interval"]<-c("Late Changhsingian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Ushimaru"),"early_interval"]<-c("Tithonian")
  bival[which(bival$formation=="Ushimaru"),"late_interval"]<-c("Tithonian") #Haggart and Matsukawa 2020

  bival[which(bival$formation=="Uzen"),"early_interval"]<-c("Spathian")
  bival[which(bival$formation=="Uzen"),"late_interval"]<-c("Spathian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Valtos Sandstone"),"early_interval"]<-c("Subcontractus")
  bival[which(bival$formation=="Valtos Sandstone"),"late_interval"]<-c("Subcontractus") #Barron et al. 2012

  bival[which(bival$formation=="van Hauen"),"early_interval"]<-c("Wordian")
  bival[which(bival$formation=="van Hauen"),"late_interval"]<-c("Capitanian") #Lee et al. 2019

  bival[which(bival$formation=="Vardekloft"),"early_interval"]<-c("Early Bathonian")
  bival[which(bival$formation=="Vardekloft"),"late_interval"]<-c("Early Bathonian") #According to ammonoid zone

  bival[which(bival$formation=="Vellerat"),"early_interval"]<-c("Late Oxfordian")
  bival[which(bival$formation=="Vellerat"),"late_interval"]<-c("Late Oxfordian") #Thuy et al. 2013

  bival[which(bival$formation=="Verkhnii Uslon"),"early_interval"]<-c("Late Roadian")
  bival[which(bival$formation=="Verkhnii Uslon"),"late_interval"]<-c("Late Roadian") #Upper Kazanian

  bival[which(bival$formation=="Vester"&bival$early_interval=="Carnian"),"early_interval"]<-c("Tuvalian")
  bival[which(bival$formation=="Vester"&bival$late_interval=="Carnian"),"late_interval"]<-c("Tuvalian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Veszpr茅m Marl"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Veszpr茅m Marl"),"late_interval"]<-c("Julian") #The formation name should be Veszprém Marl. Following Flannery-Sutherland et al. 2022

  bival[which(bival$collection_no==128375),"early_interval"]<-c("Late Pliensbachian")
  bival[which(bival$collection_no==128375),"late_interval"]<-c("Late Pliensbachian") #Domerian Substage

  bival[which(bival$collection_no==127112),"early_interval"]<-c("Gracilis")
  bival[which(bival$collection_no==127112),"late_interval"]<-c("Athleta") #According to ammonoid zone

  bival[which(bival$formation=="Vitiacua"),"early_interval"]<-c("Permian")
  bival[which(bival$formation=="Vitiacua"),"late_interval"]<-c("Permian") #The age of this formation is uncertain. It will be discarded

  bival[which(bival$formation=="Waluba"),"early_interval"]<-c("Alaunian")
  bival[which(bival$formation=="Waluba"),"late_interval"]<-c("Sevatian") #Tong et al. 2021

  bival[which(bival$formation=="Wangpanli"),"early_interval"]<-c("Early Changhsingian")
  bival[which(bival$formation=="Wangpanli"),"late_interval"]<-c("Early Changhsingian") #Shen et al. 2017

  bival[which(bival$formation=="Wargal"&bival$member=="Kalabagh"),"early_interval"]<-c("Late Wuchiapingian")
  bival[which(bival$formation=="Wargal"&bival$member=="Kalabagh"),"late_interval"]<-c("Late Wuchiapingian") #Waterhouse 2010

  bival[which(bival$formation=="Weishan"),"early_interval"]<-c("Lacian")
  bival[which(bival$formation=="Weishan"),"late_interval"]<-c("Lacian") #Tiechang Member, Sanhedong Formation

  bival[which(bival$formation=="Weiyuanjiang"),"early_interval"]<-c("Carnian")
  bival[which(bival$formation=="Weiyuanjiang"),"late_interval"]<-c("Carnian") #Tong et al. 2021

  bival[which(bival$formation=="Wellenkalk"&bival$member=="Gogoliner Schichten"),"early_interval"]<-c("Aegean")
  bival[which(bival$formation=="Wellenkalk"&bival$member=="Gogoliner Schichten"),"late_interval"]<-c("Aegean") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation%in%c("Wengen","Wengener Schichten")),"early_interval"]<-c("Longobardian")
  bival[which(bival$formation%in%c("Wengen","Wengener Schichten")),"late_interval"]<-c("Longobardian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Werfen"&bival$early_interval=="Changhsingian"),"early_interval"]<-c("Late Changhsingian")
  bival[which(bival$formation=="Werfen"&bival$late_interval=="Changhsingian"),"late_interval"]<-c("Late Changhsingian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Wetterstein"&bival$early_interval=="Anisian"),"early_interval"]<-c("Illyrian")
  bival[which(bival$formation=="Wetterstein"&bival$late_interval=="Anisian"),"late_interval"]<-c("Illyrian") #According to ammonoid zone
  bival[which(bival$formation=="Wetterstein Limestone"&bival$early_interval=="Carnian"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Wetterstein Limestone"&bival$late_interval=="Carnian"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022
  bival[which(bival$collection_no%in%c(202276:202280)),"early_interval"]<-c("Pelsonian")
  bival[which(bival$collection_no%in%c(202276:202280)),"late_interval"]<-c("Illyrian") #The brachiopods and ammonoids suggest late Anisian

  bival[which(bival$formation=="Whitby Mudstone"&bival$member=="Alum Shale"),"early_interval"]<-c("Bifrons")
  bival[which(bival$formation=="Whitby Mudstone"&bival$member=="Alum Shale"),"late_interval"]<-c("Bifrons")
  bival[which(bival$formation=="Whitby Mudstone"&bival$member=="Grey Shale"),"early_interval"]<-c("Tenuicostatum")
  bival[which(bival$formation=="Whitby Mudstone"&bival$member=="Grey Shale"),"late_interval"]<-c("Tenuicostatum")
  bival[which(bival$formation=="Whitby Mudstone"&bival$member=="Mulgrave Shale"),"early_interval"]<-c("Falciferum")
  bival[which(bival$formation=="Whitby Mudstone"&bival$member=="Mulgrave Shale"),"late_interval"]<-c("Falciferum")
  bival[which(bival$collection_no==219475),"early_interval"]<-c("Tenuicostatum")
  bival[which(bival$collection_no==219475),"late_interval"]<-c("Tenuicostatum")
  bival[which(bival$collection_no==219476),"early_interval"]<-c("Falciferum")
  bival[which(bival$collection_no==219476),"late_interval"]<-c("Falciferum") #According to ammonoid zone

  bival[which(bival$formation=="White Lias"&bival$early_interval=="Rhaetian"),"early_interval"]<-c("Late Rhaetian")
  bival[which(bival$formation=="White Lias"&bival$late_interval=="Rhaetian"),"late_interval"]<-c("Late Rhaetian")
  bival[which(bival$formation=="White Lias"&bival$early_interval=="Hettangian"),"early_interval"]<-c("Tilmani/spelae")
  bival[which(bival$formation=="White Lias"&bival$late_interval=="Hettangian"),"late_interval"]<-c("Tilmani/spelae") #Near T/J boundry

  bival[which(bival$formation=="White Limestone"&bival$early_interval=="Bathonian"),"early_interval"]<-c("Subcontractus")
  bival[which(bival$formation=="White Limestone"&bival$late_interval=="Bathonian"),"late_interval"]<-c("Retrocostatum") #Barron et al. 2012

  bival[which(bival$formation=="Whitehorse"),"early_interval"]<-c("Wordian")
  bival[which(bival$formation=="Whitehorse"),"late_interval"]<-c("Early Capitanian") #Capitanian in PBDB, mostly Wordian in Foster et al. 2014

  bival[which(bival$formation=="Winnemucca"),"early_interval"]<-c("Lacian")
  bival[which(bival$formation=="Winnemucca"),"late_interval"]<-c("Lacian") #"The species is known only from the early Norian"

  bival[which(bival$formation=="Wordie Creek"&bival$early_interval=="Early Triassic"),"early_interval"]<-c("Induan")
  bival[which(bival$formation=="Wordie Creek"&bival$late_interval=="Early Triassic"),"late_interval"]<-c("Induan") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Wutankule"),"early_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Wutankule"),"late_interval"]<-c("Late Artinskian") #Chen et al. 2003

  bival[which(bival$formation=="Xiaowa"),"early_interval"]<-c("Julian")
  bival[which(bival$formation=="Xiaowa"),"late_interval"]<-c("Julian") #Most species are from the lower part. Zhao et al. 2021

  bival[which(bival$formation=="Xintianmen"&bival$early_interval=="Hettangian"),"early_interval"]<-c("Late Hettangian")
  bival[which(bival$formation=="Xintianmen"&bival$late_interval=="Hettangian"),"late_interval"]<-c("Late Hettangian")
  bival[which(bival$formation=="Xintianmen"&bival$early_interval=="Sinemurian"),"early_interval"]<-c("Early Sinemurian")
  bival[which(bival$formation=="Xintianmen"&bival$late_interval=="Sinemurian"),"late_interval"]<-c("Early Sinemurian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Yakoun"),"early_interval"]<-c("Bajocian")
  bival[which(bival$formation=="Yakoun"),"late_interval"]<-c("Bajocian") #Kottachchi et al. 2002

  bival[which(bival$formation=="Yanjar"),"early_interval"]<-c("Olenekian")
  bival[which(bival$formation=="Yanjar"),"late_interval"]<-c("Olenekian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Yamamba Limestone"),"early_interval"]<-c("Capitanian")
  bival[which(bival$formation=="Yamamba Limestone"),"late_interval"]<-c("Capitanian") #Shen and Shi 2004

  bival[which(bival$formation=="Yenduyet"),"early_interval"]<-c("Early Changhsingian")
  bival[which(bival$formation=="Yenduyet"),"late_interval"]<-c("Early Changhsingian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Yinkeng"&bival$early_interval=="Changhsingian"),"early_interval"]<-c("Late Changhsingian")
  bival[which(bival$formation=="Yinkeng"&bival$late_interval=="Changhsingian"),"late_interval"]<-c("Late Changhsingian") #Near P/T boundry

  bival[which(bival$formation=="Ystanakh"),"early_interval"]<-c("Spathian")
  bival[which(bival$formation=="Ystanakh"),"late_interval"]<-c("Spathian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Yundoutan"),"early_interval"]<-c("Late Wuchiapingian")
  bival[which(bival$formation=="Yundoutan"),"late_interval"]<-c("Late Wuchiapingian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Zalgiris"),"early_interval"]<-c("Late Wuchiapingian")
  bival[which(bival$formation=="Zalgiris"),"late_interval"]<-c("Late Wuchiapingian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Zechstein"&bival$member=="Upper Werra Anhydrite"),"early_interval"]<-c("Early Wuchiapingian")
  bival[which(bival$formation=="Zechstein"&bival$member=="Upper Werra Anhydrite"),"late_interval"]<-c("Early Wuchiapingian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Zewan"&bival$member=="A"),"early_interval"]<-c("Early Wuchiapingian")
  bival[which(bival$formation=="Zewan"&bival$member=="A"),"late_interval"]<-c("Early Wuchiapingian")
  bival[which(bival$formation=="Zewan"&bival$member=="C"),"early_interval"]<-c("Late Wuchiapingian")
  bival[which(bival$formation=="Zewan"&bival$member=="C"),"late_interval"]<-c("Late Wuchiapingian")
  bival[which(bival$formation=="Zewan"&bival$member=="D"),"early_interval"]<-c("Early Changhsingian")
  bival[which(bival$formation=="Zewan"&bival$member=="D"),"late_interval"]<-c("Early Changhsingian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Zhuganpo"),"early_interval"]<-c("Longobardian")
  bival[which(bival$formation=="Zhuganpo"),"late_interval"]<-c("Julian") #Benton et al. 2013

  bival[which(bival$formation=="Zlambach"),"early_interval"]<-c("Rhaetian")
  bival[which(bival$formation=="Zlambach"),"late_interval"]<-c("Rhaetian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$formation=="Zor Hauran"),"early_interval"]<-c("Rhaetian")
  bival[which(bival$formation=="Zor Hauran"),"late_interval"]<-c("Rhaetian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$member=="Carditaschichten"),"early_interval"]<-c("Julian")
  bival[which(bival$member=="Carditaschichten"),"late_interval"]<-c("Julian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$member=="Nerinea Bed"),"early_interval"]<-c("Kimmeridgian")
  bival[which(bival$member=="Nerinea Bed"),"late_interval"]<-c("Kimmeridgian") #Kimmeridgian-Tithonian in PBDB. Schrank 2010

  bival[which(bival$member=="Middle Saurian Bed"),"early_interval"]<-c("Late Kimmeridgian")
  bival[which(bival$member=="Middle Saurian Bed"),"late_interval"]<-c("Late Kimmeridgian") #Schrank 2010

  bival[which(bival$member=="Trigonia Smeei Bed"),"early_interval"]<-c("Early Tithonian")
  bival[which(bival$member=="Trigonia Smeei Bed"),"late_interval"]<-c("Early Tithonian")
  bival[which(bival$member=="Trigonia smeei Bed"),"early_interval"]<-c("Early Tithonian")
  bival[which(bival$member=="Trigonia smeei Bed"),"late_interval"]<-c("Early Tithonian") #Aberhan 2002; Schrank 2010

  bival[which(bival$member=="Napeng"),"early_interval"]<-c("Rhaetian")
  bival[which(bival$member=="Napeng"),"late_interval"]<-c("Rhaetian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$member=="Halobia Limestone"&bival$early_interval=="Norian"),"early_interval"]<-c("Lacian")
  bival[which(bival$member=="Halobia Limestone"&bival$late_interval=="Norian"),"late_interval"]<-c("Lacian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$collection_no==85014),"early_interval"]<-c("Otamitan")
  bival[which(bival$collection_no==85014),"late_interval"]<-c("Otamitan") #"Otamitan stage equivalent" in PBDB

  bival[which(bival$stratgroup=="Yangshiping"&bival$early_interval=="Oxfordian"),"early_interval"]<-c("Callovian")
  bival[which(bival$stratgroup=="Yangshiping"&bival$late_interval=="Oxfordian"),"late_interval"]<-c("Callovian") #Sun et al. 2017

  bival[which(bival$collection_no==155211),"early_interval"]<-c("Aalensis")
  bival[which(bival$collection_no==155211),"late_interval"]<-c("Aalensis") #According to ammonoid zone

  bival[which(bival$collection_no==38487),"early_interval"]<-c("Middle Callovian")
  bival[which(bival$collection_no==38487),"late_interval"]<-c("Middle Callovian") #Barron et al. 2012

  bival[which(bival$stratgroup=="Yakuno"&bival$early_interval=="Induan"),"early_interval"]<-c("Olenekian") #Following Flannery-Sutherland et al. 2022

  bival[which(bival$stratgroup=="Fukumoto"&bival$early_interval=="Induan"),"early_interval"]<-c("Olenekian") #The Kusano Formation is Olenekian, and the Induan-Olenekian records may be Olenekian as well

  bival[which(bival$collection_no==61741),"early_interval"]<-c("Longobardian")
  bival[which(bival$collection_no==61741),"late_interval"]<-c("Julian") #According to the original reference

  bival[which(bival$stratgroup=="Yakoun"),"early_interval"]<-c("Bajocian")
  bival[which(bival$stratgroup=="Yakoun"),"late_interval"]<-c("Bajocian") #Kottachchi et al. 2002





  #####In addition to the records mentioned above, these records without the formation name are also revised

  bival[which(bival$early_interval=="Levesquei"),"early_interval"]<-c("Pseudoradiosa")
  bival[which(bival$late_interval=="Levesquei"),"late_interval"]<-c("Pseudoradiosa") #Levesquei should be a Late Toarcian zone, but the absolute age is Bajocian in fossilbrush

  bival[which(bival$early_interval=="Norian"&bival$late_interval=="Rhaetian"&bival$ref_author=="Dagys"),"early_interval"]<-c("Rhaetian") #See comments in PBDB collection 128039: Listed as "Norian-Rhaetian" but only because there was debate at the time whether the Rhaetian was a separate stage. These "Norian-Rhaetian" taxa were also regarded as Rhaetian ones by Ruban (2010)



  ######These revisions are about the Triassic and Jurassic collections from New Zealand
  #The New Zealand stages were transfered to the international stages, because the absolute ages of the international ages have been revised according to GTS2020
  #However, the absolute ages of the New Zealand stages were based on faunal correlation

  #Triassic:
  bival[which(bival$cc=="NZ"&bival$early_interval=="Kaihikuan"),"early_interval"]<-c("Carnian")
  bival[which(bival$cc=="NZ"&bival$late_interval=="Kaihikuan"),"late_interval"]<-c("Carnian")
  bival[which(bival$cc=="NZ"&bival$early_interval=="Oretian"),"early_interval"]<-c("Lacian")
  bival[which(bival$cc=="NZ"&bival$late_interval=="Oretian"),"late_interval"]<-c("Lacian")
  bival[which(bival$cc=="NZ"&bival$early_interval=="Otamitan"),"early_interval"]<-c("Alaunian")
  bival[which(bival$cc=="NZ"&bival$late_interval=="Otamitan"),"late_interval"]<-c("Alaunian")
  bival[which(bival$cc=="NZ"&bival$early_interval=="Warepan"),"early_interval"]<-c("Sevatian")
  bival[which(bival$cc=="NZ"&bival$late_interval=="Warepan"),"late_interval"]<-c("Sevatian")
  bival[which(bival$cc=="NZ"&bival$early_interval=="Otapiran"),"early_interval"]<-c("Rhaetian")
  bival[which(bival$cc=="NZ"&bival$late_interval=="Otapiran"),"late_interval"]<-c("Rhaetian") #See Raine et al. 2015

  bival[which(bival$collection_no%in%c(44467,44472,44473,44484:44490)),"early_interval"]<-c("Lacian")
  bival[which(bival$collection_no%in%c(44467,44472,44473,44484:44490)),"late_interval"]<-c("Lacian") #Carnian-Norian in PBDB. But the presence of Halobia austriaca and other Halobia species suggest an Early Norian age according to Campbell 2019

  bival[which(bival$collection_no==101407),"early_interval"]<-c("Julian")
  bival[which(bival$collection_no==101407),"late_interval"]<-c("Julian") #"Lower Carnian" in PBDB

  bival[which(bival$collection_no==201498),"early_interval"]<-c("Alaunian")
  bival[which(bival$collection_no==201498),"late_interval"]<-c("Sevatian") #"Otamitan-Warepan" in PBDB

  bival[which(bival$collection_no%in%c(201930,201932,201934,201935,201937,201939,201943)),"early_interval"]<-c("Norian")
  bival[which(bival$collection_no%in%c(201930,201932,201934,201935,201937,201939,201943)),"late_interval"]<-c("Norian") #Carnian in PBDB, but the presence of Halobia possibly indicates Norian

  bival[which(bival$collection_no%in%c(50759)),"early_interval"]<-c("Early Rhaetian")
  bival[which(bival$collection_no%in%c(50759)),"late_interval"]<-c("Early Rhaetian") #"Early Otapirian"

  bival[which(bival$collection_no%in%c(50760)),"early_interval"]<-c("Late Rhaetian")
  bival[which(bival$collection_no%in%c(50760)),"late_interval"]<-c("Late Rhaetian") #"Late Otapirian"

  bival[which(bival$collection_no%in%c(203926)),"early_interval"]<-c("Sevatian")
  bival[which(bival$collection_no%in%c(203926)),"late_interval"]<-c("Sevatian") #"Warepan"

  bival[which(bival$collection_no%in%c(203932)),"early_interval"]<-c("Alaunian")
  bival[which(bival$collection_no%in%c(203932)),"late_interval"]<-c("Sevatian") #"Oretian-Warepan"

  bival[which(bival$collection_no%in%c(134423,134432,134433,134459,134465,134471,134473,134476,134479,134483,134486,134487,134496,134497,134524:134593,134595:134598,134625
                                       ,134628:134633,134771,134784:134786,134789,134790,134792:134799,134815,134817,134822:134824,134827:134834,134839,134841,134852:134859
                                       ,134870:134873,134880,134881,134883:134887,134889:134892,134896,134907,134910,134921,134922,134960,134961,134963,134971:134976,134979
                                       ,134984:134986,134989:134994,134996:134999,135006,135020,135021,135085,135137:135139,135141,135144:135150,135156,135165:135167,135170
                                       ,135171,135191:135193,135201:135206,135208:135210,135213:135216,135218:135221,135233,135234,135239,135241,135244,135246:135248,135250
                                       ,135251,135252,135254,135255,135260:135265,135275,135277,135280,135283,135284,135285,135288:135292,135295:135300,135307:135311,137175:137178
                                       ,137182,137183,137868:137871,197505:197508,201925,201926,201945,44459:44466,44468:44471,44478,60997,134429,134430,134472,134800)),"early_interval"]<-c("Carnian")
  bival[which(bival$collection_no%in%c(134423,134432,134433,134459,134465,134471,134473,134476,134479,134483,134486,134487,134496,134497,134524:134593,134595:134598,134625
                                       ,134628:134633,134771,134784:134786,134789,134790,134792:134799,134815,134817,134822:134824,134827:134834,134839,134841,134852:134859
                                       ,134870:134873,134880,134881,134883:134887,134889:134892,134896,134907,134910,134921,134922,134960,134961,134963,134971:134976,134979
                                       ,134984:134986,134989:134994,134996:134999,135006,135020,135021,135085,135137:135139,135141,135144:135150,135156,135165:135167,135170
                                       ,135171,135191:135193,135201:135206,135208:135210,135213:135216,135218:135221,135233,135234,135239,135241,135244,135246:135248,135250
                                       ,135251,135252,135254,135255,135260:135265,135275,135277,135280,135283,135284,135285,135288:135292,135295:135300,135307:135311,137175:137178
                                       ,137182,137183,137868:137871,197505:197508,201925,201926,201945,44459:44466,44468:44471,44478,60997,134429,134430,134472,134800)),"late_interval"]<-c("Carnian") #Most of these collections are Ladinian in PBDB. These collections are Kaihikuan in age.
  #These taxa were regarded as Ladinian ones by Treatise and these records were re-assigned to the Longobardian by Flannery-Sutherland et al. 2022.
  #Mundil et al. (2010) report a provisional CA-TIMS Pb-U zircon age of 237 Ma for a tuff within upper Etalian strata (See Raine et al. 2015).
  #This age is consistent with the age of the Ladinian/Carnian boundry. Although the Kaihikuan may extend to the uppermost Ladinian, here it is treated to be equivalent to Carnian

  bival[which(bival$collection_no%in%c(134434:134440,134482,134484,134485,134488,134489,134490,134494,134626,134778,134782,134783,134791,134818,134820,134821,134825,134826
                                       ,134845,134848,134869,134882,134926,134927,134964,134965,134968:134970,134995,135110,135140,135194:135197,135235,135238,135259,137495
                                       ,137809:137817,44453:44455,44457,44458,44474,44476,44477,44479,44480:44482,60990)),"early_interval"]<-c("Ladinian")
  bival[which(bival$collection_no%in%c(134434:134440,134482,134484,134485,134488,134489,134490,134494,134626,134778,134782,134783,134791,134818,134820,134821,134825,134826
                                       ,134845,134848,134869,134882,134926,134927,134964,134965,134968:134970,134995,135110,135140,135194:135197,135235,135238,135259,137495
                                       ,137809:137817,44453:44455,44457,44458,44474,44476,44477,44479,44480:44482,60990)),"late_interval"]<-c("Ladinian") #Late Etalian according to the brachiopod and bivalve range

  bival[which(bival$collection_no%in%c(134620,134627,134781,134843,134851,134928,135119,135198,135304)),"early_interval"]<-c("Anisian")
  bival[which(bival$collection_no%in%c(134620,134627,134781,134843,134851,134928,135119,135198,135304)),"late_interval"]<-c("Anisian") #Early Etalian according to the brachiopod range

  bival[which(bival$collection_no%in%c(134780,134787,134788,134819,134846,134849,134850,134888,134893:134895,134905,134912:134914,134918:134920,44483)),"early_interval"]<-c("Pelsonian")
  bival[which(bival$collection_no%in%c(134780,134787,134788,134819,134846,134849,134850,134888,134893:134895,134905,134912:134914,134918:134920,44483)),"late_interval"]<-c("Fassanian") #Middle Etalian according to the brachiopod range

  bival[which(bival$collection_no%in%c(134877,134878,134879,134897,134899:134903,134911,134915:134917)),"early_interval"]<-c("Spathian")
  bival[which(bival$collection_no%in%c(134877,134878,134879,134897,134899:134903,134911,134915:134917)),"late_interval"]<-c("Spathian") #"Malakovian"

  bival[which(bival$collection_no%in%c(41666:41678,41680,41682,41684,41685,41688)),"early_interval"]<-c("Rhaetian")
  bival[which(bival$collection_no%in%c(41666:41678,41680,41682,41684,41685,41688)),"late_interval"]<-c("Rhaetian") #"Warepan" in the original reference

  #Jurassic:
  bival[which(bival$cc=="NZ"&bival$early_interval=="Puaroan"),"early_interval"]<-c("Semiforme")
  bival[which(bival$cc=="NZ"&bival$late_interval=="Puaroan"),"late_interval"]<-c("Late Tithonian")
  bival[which(bival$cc=="NZ"&bival$early_interval=="Ohauan"),"early_interval"]<-c("Late Kimmeridgian")
  bival[which(bival$cc=="NZ"&bival$late_interval=="Ohauan"),"late_interval"]<-c("Darwini")
  bival[which(bival$cc=="NZ"&bival$early_interval=="Heterian"),"early_interval"]<-c("Coronatum")
  bival[which(bival$cc=="NZ"&bival$late_interval=="Heterian"),"late_interval"]<-c("Early Kimmeridgian")
  bival[which(bival$cc=="NZ"&bival$early_interval=="Temaikan"),"early_interval"]<-c("Late Toarcian")
  bival[which(bival$cc=="NZ"&bival$late_interval=="Temaikan"),"late_interval"]<-c("Anceps")
  bival[which(bival$cc=="NZ"&bival$early_interval=="Ururoan"),"early_interval"]<-c("Ibex")
  bival[which(bival$cc=="NZ"&bival$late_interval=="Ururoan"),"late_interval"]<-c("Variabilis")
  bival[which(bival$cc=="NZ"&bival$early_interval=="Aratauran"),"early_interval"]<-c("Hettangian")
  bival[which(bival$cc=="NZ"&bival$late_interval=="Aratauran"),"late_interval"]<-c("Jamesoni") #According to Raine et al. 2015

  bival[which(bival$collection_no%in%c(63755,63859)),"early_interval"]<-c("Semiforme")
  bival[which(bival$collection_no%in%c(63755,63859)),"late_interval"]<-c("Pallasioides") #"Mangaoran"

  bival[which(bival$collection_no%in%c(182779,63741,63765,63766,63758)),"early_interval"]<-c("Rotunda")
  bival[which(bival$collection_no%in%c(182779,63741,63765,63766,63758)),"late_interval"]<-c("Late Tithonian") #"Upper Puaroan or Waikatoan". "An upper Middle Tithonian (upper Fallauxi Zone) correlation is adopted for the base of the Waikatoan". So Rotunda is selected to represent the "upper Fallauxi Zone"

  bival[which(bival$collection_no%in%c(134137:134141)),"early_interval"]<-c("Hybonotum")
  bival[which(bival$collection_no%in%c(134137:134141)),"late_interval"]<-c("Darwini") #"Late Ohauan"

  bival[which(bival$collection_no%in%c(182791,38962,63869)),"early_interval"]<-c("Semiforme")
  bival[which(bival$collection_no%in%c(182791,38962,63869)),"late_interval"]<-c("Late Tithonian") #"Puaroan"

  bival[which(bival$collection_no%in%c(63829,63836,63837,137847)),"early_interval"]<-c("Late Kimmeridgian")
  bival[which(bival$collection_no%in%c(63829,63836,63837,137847)),"late_interval"]<-c("Darwini") #"Ohauan"

  bival[which(bival$collection_no%in%c(134142,134143,63742,63856:63858,63866,63875,63876,63882)),"early_interval"]<-c("Late Kimmeridgian")
  bival[which(bival$collection_no%in%c(134142,134143,63742,63856:63858,63866,63875,63876,63882)),"late_interval"]<-c("Late Kimmeridgian") #"Early Ohauan"

  bival[which(bival$collection_no%in%c(134010,51934,56229,56230,63745:63747,63749,63751,63880,63881)),"early_interval"]<-c("Coronatum")
  bival[which(bival$collection_no%in%c(134010,51934,56229,56230,63745:63747,63749,63751,63880,63881)),"late_interval"]<-c("Cautisnigrae") #"Early Heterian"

  bival[which(bival$collection_no%in%c(133865,134009,35723,56289,56292,56296,133848,133855,133856,133858:133863,133956:133963,133971,134086:134090,134101:134104,134110,134127,134135,134160,134162,182717
                                       ,182720,182728:182737,182739:182741,182757:182760,182773,182775,182776,182784,182785,56231,56290,63763,63764)),"early_interval"]<-c("Pseudocordata")
  bival[which(bival$collection_no%in%c(133865,134009,35723,56289,56292,56296,133848,133855,133856,133858:133863,133956:133963,133971,134086:134090,134101:134104,134110,134127,134135,134160,134162,182717
                                       ,182720,182728:182737,182739:182741,182757:182760,182773,182775,182776,182784,182785,56231,56290,63763,63764)),"late_interval"]<-c("Pseudocordata") #"Middle Heterian"

  bival[which(bival$collection_no%in%c(133910,133972,134091,134167,182718,182719,182738)),"early_interval"]<-c("Pseudocordata")
  bival[which(bival$collection_no%in%c(133910,133972,134091,134167,182718,182719,182738)),"late_interval"]<-c("Early Kimmeridgian") #"Middle-late Heterian"

  bival[which(bival$collection_no%in%c(133973,133974,63855)),"early_interval"]<-c("Early Kimmeridgian")
  bival[which(bival$collection_no%in%c(133973,133974,63855)),"late_interval"]<-c("Early Kimmeridgian") #"Late Heterian"

  bival[which(bival$collection_no%in%c(133914,133925)),"early_interval"]<-c("Early Kimmeridgian")
  bival[which(bival$collection_no%in%c(133914,133925)),"late_interval"]<-c("Late Kimmeridgian") #"Late Heterian-early Ohauan". The base of the Middle Ohauan is correlated within the upper Kimmeridgian (within the last Kimmeridgian ammonoid zone, beckeri zone?)

  bival[which(bival$member=="Captain Kings Shellbed"),"early_interval"]<-c("Pseudocordata")
  bival[which(bival$member=="Captain Kings Shellbed"),"late_interval"]<-c("Pseudocordata") #This member is Middle Heterian

  bival[which(bival$collection_no%in%c(38961)),"early_interval"]<-c("Coronatum")
  bival[which(bival$collection_no%in%c(38961)),"late_interval"]<-c("Early Kimmeridgian") #"Heterian"

  bival[which(bival$collection_no%in%c(133850,133899,133936,133937,133866,133867:133880)),"early_interval"]<-c("Middle Bathonian")
  bival[which(bival$collection_no%in%c(133850,133899,133936,133937,133866,133867:133880)),"late_interval"]<-c("Anceps") #Late late Temaikan, treated as the middle Bathonian to Callovian part of late Temaikan

  bival[which(bival$collection_no%in%c(133921,133923,133924,56241,56245,56249,56250,56241,56283,56285,56241,56273,56241,56285,134288,134403,182714:182716,182722:182725,182726,182755,182774,182780,182781,182783,182792
                                       ,63736,63737,63774,182782,133922)),"early_interval"]<-c("Late Bajocian")
  bival[which(bival$collection_no%in%c(133921,133923,133924,56241,56245,56249,56250,56241,56283,56285,56241,56273,56241,56285,134288,134403,182714:182716,182722:182725,182726,182755,182774,182780,182781,182783,182792
                                       ,63736,63737,63774,182782,133922)),"late_interval"]<-c("Anceps") #"Late Temaikan"

  bival[which(bival$collection_no%in%c(134361,56252,133898,133901,133915,134402,134404,134405,4878)),"early_interval"]<-c("Late Aalenian")
  bival[which(bival$collection_no%in%c(134361,56252,133898,133901,133915,134402,134404,134405,4878)),"late_interval"]<-c("Anceps") #"Middle-late Temaikan"

  bival[which(bival$collection_no%in%c(134228,134229,182710:182713,182756,182787,56237:56240,56242:56244,56264)),"early_interval"]<-c("Late Aalenian")
  bival[which(bival$collection_no%in%c(134228,134229,182710:182713,182756,182787,56237:56240,56242:56244,56264)),"late_interval"]<-c("Early Bajocian") #"Middle Temaikan"

  bival[which(bival$collection_no%in%c(134291,134355,182727)),"early_interval"]<-c("Early Bajocian")
  bival[which(bival$collection_no%in%c(134291,134355,182727)),"late_interval"]<-c("Early Bajocian") #"Upper middle Temaikan"

  bival[which(bival$collection_no%in%c(134370,182786)),"early_interval"]<-c("Late Bajocian")
  bival[which(bival$collection_no%in%c(134370,182786)),"late_interval"]<-c("Late Bajocian") #"Base of upper Temaikan"

  bival[which(bival$collection_no%in%c(135072,133916:133918,38958,38963,38964,56279:56281,56398,63729,63732,63734,63738,63753,63775,63817:63820)),"early_interval"]<-c("Late Toarcian")
  bival[which(bival$collection_no%in%c(135072,133916:133918,38958,38963,38964,56279:56281,56398,63729,63732,63734,63738,63753,63775,63817:63820)),"late_interval"]<-c("Middle Aalenian") #"Early Temaikan"

  bival[which(bival$collection_no%in%c(135103,135211,135271,135312:135314)),"early_interval"]<-c("Late Toarcian")
  bival[which(bival$collection_no%in%c(135103,135211,135271,135312:135314)),"late_interval"]<-c("Late Toarcian") #"Lowest Temaikan"

  bival[which(bival$collection_no%in%c(51932)),"early_interval"]<-c("Late Toarcian")
  bival[which(bival$collection_no%in%c(51932)),"late_interval"]<-c("Anceps") #"Temaikan"

  bival[which(bival$collection_no%in%c(133926,133935,133939,133941:133955,133966,133967,133969,133970,133975:133979,133993,133994,133996:134002,134014,134015,134033,134034
                                       ,134072:134080,134082,134083,134145,134147,134148,134205:134207,134214,134278,134283,134285:134287,134354,134356,134359,134366,134374
                                       ,134375,134377:134380,134391:134398,135121,135122,135126,160386,38957,133938,133940,134146)),"early_interval"]<-c("Bifrons")
  bival[which(bival$collection_no%in%c(133926,133935,133939,133941:133955,133966,133967,133969,133970,133975:133979,133993,133994,133996:134002,134014,134015,134033,134034
                                       ,134072:134080,134082,134083,134145,134147,134148,134205:134207,134214,134278,134283,134285:134287,134354,134356,134359,134366,134374
                                       ,134375,134377:134380,134391:134398,135121,135122,135126,160386,38957,133938,133940,134146)),"late_interval"]<-c("Variabilis") #"Late Ururoan"

  bival[which(bival$collection_no%in%c(63809,63814,63831,137930,38931:38933)),"early_interval"]<-c("Ibex")
  bival[which(bival$collection_no%in%c(63809,63814,63831,137930,38931:38933)),"late_interval"]<-c("Variabilis") #"Ururoan"

  bival[which(bival$collection_no%in%c(134003,134181,135007,135008,135015,135098,135113:135115,135127,135224,135226,135228,135230:135232,135258,135266:135267,135270,135274,137848
                                       ,204591,50763)),"early_interval"]<-c("Ibex")
  bival[which(bival$collection_no%in%c(134003,134181,135007,135008,135015,135098,135113:135115,135127,135224,135226,135228,135230:135232,135258,135266:135267,135270,135274,137848
                                       ,204591,50763)),"late_interval"]<-c("Serpentinum") #"Early Ururoan"

  bival[which(bival$collection_no%in%c(133928,133934,134005,134011,134012,134016,134017,134131,134158,134209,134210,134218,134381,134383:134387,135010:135012,135014,135045
                                       ,135046,135073,135075:135079,135086:135090,135099,135102,135104:135107,135111,135112,135120,135123,135173,135175,135178,135185,135222
                                       ,135223,135257,38929)),"early_interval"]<-c("Sinemurian")
  bival[which(bival$collection_no%in%c(133928,133934,134005,134011,134012,134016,134017,134131,134158,134209,134210,134218,134381,134383:134387,135010:135012,135014,135045
                                       ,135046,135073,135075:135079,135086:135090,135099,135102,135104:135107,135111,135112,135120,135123,135173,135175,135178,135185,135222
                                       ,135223,135257,38929)),"late_interval"]<-c("Jamesoni") #"Late Aratauran"

  bival[which(bival$collection_no%in%c(38928,38934,38955,51864,51868)),"early_interval"]<-c("Hettangian")
  bival[which(bival$collection_no%in%c(38928,38934,38955,51864,51868)),"late_interval"]<-c("Jamesoni") #"Aratauran"


  ######These revisions are about the Permian collections from New Zealand. Ages following Waterhouse 2002, 2021

  bival[which(bival$collection_no%in%c(41775,41791,52787,62063)),"early_interval"]<-c("Early Artinskian")
  bival[which(bival$collection_no%in%c(41775,41791,52787,62063)),"late_interval"]<-c("Early Artinskian") #Notostrophia zealandicus, Notostrophia homeri,Terrakea dickinsi zones

  bival[which(bival$collection_no%in%c(41790,137851)),"early_interval"]<-c("Late Artinskian")
  bival[which(bival$collection_no%in%c(41790,137851)),"late_interval"]<-c("Late Artinskian") #Echinalosia prideri (now Echinalosia conata) brachiopod zone

  bival[which(bival$collection_no%in%c(52696,39926,39927,39928,39929,39930,39931,41768,41771,52763,52766,52782,52798,52896,41780,52832,120440,73851,120639,41769,120441,120423,120426,120430:120433
                                       ,120436:120439,121735,41774,41786,41765)),"early_interval"]<-c("Early Changhsingian")
  bival[which(bival$collection_no%in%c(52696,39926,39927,39928,39929,39930,39931,41768,41771,52763,52766,52782,52798,52896,41780,52832,120440,73851,120639,41769,120441,120423,120426,120430:120433
                                       ,120436:120439,121735,41774,41786,41765)),"late_interval"]<-c("Early Changhsingian") #Plekonella multicostata, Spinomartinia spinosa zones

  bival[which(bival$collection_no%in%c(7276,120641,52751,52752,52753,52755,52757,52759,52760,52767,52777:52780,52783:52786,52788,52793,52796,7204,98149,52244,52245,52774,52775,52776,52781,52794)),"early_interval"]<-c("Late Changhsingian")
  bival[which(bival$collection_no%in%c(7276,120641,52751,52752,52753,52755,52757,52759,52760,52767,52777:52780,52783:52786,52788,52793,52796,7204,98149,52244,52245,52774,52775,52776,52781,52794)),"late_interval"]<-c("Late Changhsingian") #Wairakiella rostrata, Marginalosia planata zones

  bival[which(bival$collection_no%in%c(39923:39925,41772,120434,120435,41794,41770)),"early_interval"]<-c("Late Wuchiapingian")
  bival[which(bival$collection_no%in%c(39923:39925,41772,120434,120435,41794,41770)),"late_interval"]<-c("Late Wuchiapingian") #Martiniopsis woodi Zone

  bival[which(bival$formation=="Flowers"),"early_interval"]<-c("Early Capitanian")
  bival[which(bival$formation=="Flowers"),"late_interval"]<-c("Early Capitanian") #Wordian in PBDB. Waterhouse 2002

  bival[which(bival$collection_no%in%c(213211)),"early_interval"]<-c("Anisian")
  bival[which(bival$collection_no%in%c(213211)),"late_interval"]<-c("Anisian") #Tongue Point Member (Greville Formation), not Permian, but Triassic. See Waterhouse 2002

  bival[which(bival$formation=="Letham"),"early_interval"]<-c("Late Kungurian")
  bival[which(bival$formation=="Letham"),"late_interval"]<-c("Roadian") #Waterhouse 2002

  bival[which(bival$collection_no%in%c(120427,41767,41777,41778,41779,41784,41785,41788,41789,52188,52189,52773,52795,52833:52835)),"early_interval"]<-c("Capitanian")
  bival[which(bival$collection_no%in%c(120427,41767,41777,41778,41779,41784,41785,41788,41789,52188,52189,52773,52795,52833:52835)),"late_interval"]<-c("Capitanian") #Waterhouse 2002

  bival[which(bival$collection_no%in%c(41796)),"early_interval"]<-c("Late Kungurian")
  bival[which(bival$collection_no%in%c(41796)),"late_interval"]<-c("Roadian")  #Echinalosia discinia Zone. Wordian in PBDB. Waterhouse 2002.

  bival[which(bival$collection_no%in%c(41798)),"early_interval"]<-c("Late Artinskian")
  bival[which(bival$collection_no%in%c(41798)),"late_interval"]<-c("Early Kungurian") #The species was discovered from the Kungurian and Artinskian



  ######These revisions are about the Permian collections from Australia. See Waterhouse 2015, 2021

  #These collections were treated to be Guadalupian by Waterhouse according to brachiopod zones. But they were dated as Wuchiapingian by geochronologic methods. Here we follow the absolute ages
  #Thus, the Guadalupian records will be much fewer than in PBDB
  bival[which(bival$formation=="Barfield"),"early_interval"]<-c("Wordian")
  bival[which(bival$formation=="Barfield"),"late_interval"]<-c("Wordian") #Wuchiapingian in PBDB. Pseudostrophalosia blakei Zone (Wordian) in Waterhouse 2015
  bival[which(bival$formation=="Blenheim"),"early_interval"]<-c("Wordian")
  bival[which(bival$formation=="Blenheim"),"late_interval"]<-c("Capitanian")
  bival[which(bival$formation=="Blenheim"&bival$member=="Moonlight Sandstone"),"early_interval"]<-c("Wordian")
  bival[which(bival$formation=="Blenheim"&bival$member=="Moonlight Sandstone"),"late_interval"]<-c("Wordian") #Mostly Wuchiapingian in PBDB. According to brachiopod zone. See Waterhouse 2008
  bival[which(bival$formation=="Flat Top"),"early_interval"]<-c("Wordian")
  bival[which(bival$formation=="Flat Top"),"late_interval"]<-c("Wordian") ##Wuchiapingian in PBDB. Wordian in Waterhouse 2015
  bival[which(bival$formation=="Catherine"),"early_interval"]<-c("Capitanian")
  bival[which(bival$formation=="Catherine"),"late_interval"]<-c("Capitanian") #Wuchiapingian in PBDB. Echinalosia ovalis Zone (Wordian or Capitanian) in Waterhouse 2015
  bival[which(bival$formation=="Freitag"),"early_interval"]<-c("Kungurian")
  bival[which(bival$formation=="Freitag"),"late_interval"]<-c("Kungurian") #Wuchiapingian in PBDB. Wyndhamia typica Zone in Waterhouse 2015
  bival[which(bival$formation=="Ingelara"),"early_interval"]<-c("Roadian")
  bival[which(bival$formation=="Ingelara"),"late_interval"]<-c("Wordian") #Wuchiapingian in PBDB. Roadian-Wordian in Waterhouse 2015
  bival[which(bival$formation=="Oxtrack"),"early_interval"]<-c("Roadian")
  bival[which(bival$formation=="Oxtrack"),"late_interval"]<-c("Roadian") #Wordian in PBDB. Roadian in Waterhouse 2015. This formation is below the Barfield Formation and may be Wuchiapingian as well???
  bival[which(bival$formation=="Peawaddy"),"early_interval"]<-c("Wordian")
  bival[which(bival$formation=="Peawaddy"),"late_interval"]<-c("Wordian") #Wuchiapingian in PBDB. Wordian in Waterhouse 2015

  bival[which(bival$formation=="Abels Bay"),"early_interval"]<-c("Capitanian")
  bival[which(bival$formation=="Abels Bay"),"late_interval"]<-c("Wuchiapingian") #Wuchiapingian in PBDB. Middle Permian in Waterhouse 2015, but Birchsella was regarded as a Cisuralian genus by Waterhouse (2016). Capitanian in Mii et al. 2012

  bival[which(bival$formation=="Aldebaran"),"early_interval"]<-c("Artinskian")
  bival[which(bival$formation=="Aldebaran"),"late_interval"]<-c("Kungurian") #Roadian in PBDB. The Aldebaran Formation of the Springsure region contains late Early Permian brachiopods, but may range into older Permian. Waterhouse 2015

  bival[which(bival$formation=="Allandale"),"early_interval"]<-c("Late Asselian")
  bival[which(bival$formation=="Allandale"),"late_interval"]<-c("Early Sakmarian") #Sakmarian in PBDB. The Allandale formation correlates to the Crassispinosella subcircularis Zone by Waterhouse (2015),
  #which is thought to be middle Asselian by Waterhouse (2015). The Alum Rock correlates to subcircularis Zone or possibly concentrica Zone below it. The two U–Pb zircon SHRIMP ages from the Alum Rock Conglomerate are
  #approximately 294 Ma and 296 Ma, and they together may support a late Asselian to earliest Sakmarian age (See Shi et al. 2022). Therefore, here the collections belong to middle Asselian biozones of Waterhouse
  #and Sakmarian in PBDB are regarded as late Asselian-early Sakmarian

  bival[which(bival$formation=="Alum Rock Beds"),"early_interval"]<-c("Late Asselian")
  bival[which(bival$formation=="Alum Rock Beds"),"late_interval"]<-c("Early Sakmarian") #Sakmarian in PBDB. Shi et al. 2022

  bival[which(bival$formation=="Baker"),"early_interval"]<-c("Early Roadian")
  bival[which(bival$formation=="Baker"),"late_interval"]<-c("Early Roadian") #Kungurian in PBDB. Haig 2018

  bival[which(bival$formation=="Beckers"),"early_interval"]<-c("Late Asselian")
  bival[which(bival$formation=="Beckers"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  bival[which(bival$formation=="Belford"),"early_interval"]<-c("Roadian")
  bival[which(bival$formation=="Belford"),"late_interval"]<-c("Roadian") #Roadian-Wordian in PBDB. Waterhouse 2015

  bival[which(bival$formation=="Berriedale"),"early_interval"]<-c("Early Artinskian")
  bival[which(bival$formation=="Berriedale"),"late_interval"]<-c("Early Artinskian") #Kungurian in PBDB. Taeniothaerus subquadratus Zone. Late Sakmarian in Waterhouse 2015, bur this zone was regarded as early Artinskian by Shi et al. 2020

  bival[which(bival$formation=="Berry"),"early_interval"]<-c("Early Wordian")
  bival[which(bival$formation=="Berry"),"late_interval"]<-c("Early Wordian") #Wordian in PBDB. Shi et al. 2022

  bival[which(bival$formation=="Berserker"),"early_interval"]<-c("Early Artinskian")
  bival[which(bival$formation=="Berserker"),"late_interval"]<-c("Early Artinskian") #Kungurian in PBDB. Taeniothaerus subquadratus Zone. Lakes Creek beds in Waterhouse 2015

  bival[which(bival$formation=="Billidee"),"early_interval"]<-c("Early Artinskian")
  bival[which(bival$formation=="Billidee"),"late_interval"]<-c("Early Artinskian") #jimbaensis zone. Waterhouse 2015

  bival[which(bival$formation=="Billop"),"early_interval"]<-c("Late Asselian")
  bival[which(bival$formation=="Billop"),"late_interval"]<-c("Early Sakmarian") #Billpo Formation. Sakmarian in PBDB, Asselian in Waterhouse 2015

  bival[which(bival$formation=="Brae"),"early_interval"]<-c("Late Kungurian")
  bival[which(bival$formation=="Brae"),"late_interval"]<-c("Late Kungurian") #Roadian in PBDB. Echinalosia discinia Zone. Latest Kungurian according to Waterhouse 2015

  bival[which(bival$formation=="Branxton"),"early_interval"]<-c("Late Kungurian")
  bival[which(bival$formation=="Branxton"),"late_interval"]<-c("Roadian") #Kungurian-Roadian in PBDB. Wyndhamia typica-Echinalosia discinia zones. Late Kungurian in Waterhouse 2015. But the the lower Branxton Formation 12m above the topmost Greta Coal seam was dated as 272.2 ± 3.2 Ma (see Waterhouse 2002), which is Roadian now

  bival[which(bival$formation=="Buffel"),"early_interval"]<-c("Sakmarian")
  bival[which(bival$formation=="Buffel"),"late_interval"]<-c("Early Artinskian")
  bival[which(bival$formation=="Buffel"&bival$member=="Dresden"),"early_interval"]<-c("Sakmarian")
  bival[which(bival$formation=="Buffel"&bival$member=="Dresden"),"late_interval"]<-c("Sakmarian")
  bival[which(bival$formation=="Buffel"&bival$member=="Fairyland"),"early_interval"]<-c("Sakmarian")
  bival[which(bival$formation=="Buffel"&bival$member=="Fairyland"),"late_interval"]<-c("Sakmarian")
  bival[which(bival$formation=="Buffel"&bival$member=="Elvinia"),"early_interval"]<-c("Late Sakmarian")
  bival[which(bival$formation=="Buffel"&bival$member=="Elvinia"),"late_interval"]<-c("Late Sakmarian")
  bival[which(bival$formation=="Buffel"&bival$member=="Rose's Pride"),"early_interval"]<-c("Artinskian")
  bival[which(bival$formation=="Buffel"&bival$member=="Rose's Pride"),"late_interval"]<-c("Artinskian") #Artinskian-Kungurian in PBDB. Waterhouse 2015

  bival[which(bival$formation=="Bulgadoo"),"early_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Bulgadoo"),"late_interval"]<-c("Late Artinskian") #Kungurian in PBDB. Haig et al. 2017

  bival[which(bival$formation=="Bundella"),"early_interval"]<-c("Late Asselian")
  bival[which(bival$formation=="Bundella"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  bival[which(bival$formation=="Burnett"),"early_interval"]<-c("Late Asselian")
  bival[which(bival$formation=="Burnett"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  bival[which(bival$formation=="Callytharra"&bival$early_interval=="Artinskian"),"early_interval"]<-c("Early Artinskian")
  bival[which(bival$formation=="Callytharra"&bival$late_interval=="Artinskian"),"late_interval"]<-c("Early Artinskian") #Sakmarian-Artinskian in PBDB. "Coronalosia irwinensis" Zone. Waterhouse 2015

  bival[which(bival$formation=="Calytrix"),"early_interval"]<-c("Late Asselian")
  bival[which(bival$formation=="Calytrix"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  bival[which(bival$formation=="Camboon"),"early_interval"]<-c("Sakmarian")
  bival[which(bival$formation=="Camboon"),"late_interval"]<-c("Sakmarian") #Kungurian in PBDB. Possibly Sakmarian in Waterhouse 2015

  bival[which(bival$formation=="Carmila"),"early_interval"]<-c("Sakmarian")
  bival[which(bival$formation=="Carmila"),"late_interval"]<-c("Sakmarian") #Artinskian in PBDB. But the species possibly suggest Sakmarian

  bival[which(bival$formation=="Carolyn"),"early_interval"]<-c("Late Asselian")
  bival[which(bival$formation=="Carolyn"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  bival[which(bival$formation=="Cattle Creek"),"early_interval"]<-c("Artinskian")
  bival[which(bival$formation=="Cattle Creek"),"late_interval"]<-c("Artinskian") #Artinskian-Kungurian in PBDB. Waterhouse 2015

  bival[which(bival$formation=="Collinsville Coal Measures"),"early_interval"]<-c("Kungurian")
  bival[which(bival$formation=="Collinsville Coal Measures"),"late_interval"]<-c("Kungurian") #Glendoo Sandstone thought to be lateral equivalent of the middle part of the Gebbie Formation,
  #most likely belonging to the upper Echinalosia maxwelli, E. davidi, or lower E. discinia brachiopod zones of Briggs (1998). Possibly Kungurian according to Shi et al. 2022. Kungurian in Waterhouse 2015

  bival[which(bival$formation=="Colraine"),"early_interval"]<-c("Sakmarian")
  bival[which(bival$formation=="Colraine"),"late_interval"]<-c("Sakmarian")  #Vanderlaan and Ebach 2015

  bival[which(bival$formation=="Coyrie"),"early_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Coyrie"),"late_interval"]<-c("Late Artinskian") #Artinskian in PBDB. Haig et al. 2017

  bival[which(bival$formation=="Cundlego"),"early_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Cundlego"),"late_interval"]<-c("Early Kungurian") #Artinskian-Kungurian in PBDB. #Haig et al. 2017

  bival[which(bival$formation=="Darlington"),"early_interval"]<-c("Late Asselian")
  bival[which(bival$formation=="Darlington"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  bival[which(bival$formation=="Eight Mile Creek"),"early_interval"]<-c("Kungurian")
  bival[which(bival$formation=="Eight Mile Creek"),"late_interval"]<-c("Kungurian") #Roadian in PBDB. Echinalosia maxwelli of Briggs. Possibly Kungurian according to Shi et al. 2022

  bival[which(bival$formation=="Elderslie"),"early_interval"]<-c("Kungurian")
  bival[which(bival$formation=="Elderslie"),"late_interval"]<-c("Kungurian") #Roadian in PBDB. Echinalosia maxwelli of Briggs. Possibly Kungurian according to Shi et al. 2022

  bival[which(bival$formation=="Eurydesma Beds"),"early_interval"]<-c("Sakmarian")
  bival[which(bival$formation=="Eurydesma Beds"),"late_interval"]<-c("Sakmarian") #Artinskian in PBDB. Bookeria pollex Zone. Sakmarian in Waterhouse 2015

  bival[which(bival$formation=="Exmoor"),"early_interval"]<-c("Wuchiapingian")
  bival[which(bival$formation=="Exmoor"),"late_interval"]<-c("Wuchiapingian") #The species is Wuchiapingian in age

  bival[which(bival$formation=="Farley"),"early_interval"]<-c("Late Sakmarian")
  bival[which(bival$formation=="Farley"),"late_interval"]<-c("Artinskian") #Artinskian in PBDB. Sakmarian in Waterhouse 2015

  bival[which(bival$formation=="Gebbie"),"early_interval"]<-c("Kungurian")
  bival[which(bival$formation=="Gebbie"),"late_interval"]<-c("Kungurian") #Roadian in PBDB. Kungurian in Waterhouse 2015

  bival[which(bival$formation=="Glencoe"),"early_interval"]<-c("Late Asselian")
  bival[which(bival$formation=="Glencoe"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  bival[which(bival$formation=="Golden Valley"),"early_interval"]<-c("Late Asselian")
  bival[which(bival$formation=="Golden Valley"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  bival[which(bival$formation=="Gray Siltstone"),"early_interval"]<-c("Late Sakmarian")
  bival[which(bival$formation=="Gray Siltstone"),"late_interval"]<-c("Late Sakmarian") #Artinskian in PBDB. Upper Gray Formation or basal Berriedale Limestone-Enstone Park limestone correlate

  bival[which(bival$formation=="High Cliff Sandstone"),"early_interval"]<-c("Late Sakmarian")
  bival[which(bival$formation=="High Cliff Sandstone"),"late_interval"]<-c("Early Artinskian") #Artinskian in PBDB. Dillinger et al. 2018

  bival[which(bival$formation=="Hickman"),"early_interval"]<-c("Late Asselian")
  bival[which(bival$formation=="Hickman"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  bival[which(bival$formation=="Inglis"),"early_interval"]<-c("Late Asselian")
  bival[which(bival$formation=="Inglis"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  bival[which(bival$formation=="Jimba Jimba"),"early_interval"]<-c("Early Artinskian")
  bival[which(bival$formation=="Jimba Jimba"),"late_interval"]<-c("Early Artinskian") #Artinskian in PBDB. Waterhouse 2015

  bival[which(bival$formation=="Kansas Creek"),"early_interval"]<-c("Late Asselian")
  bival[which(bival$formation=="Kansas Creek"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  bival[which(bival$formation=="Kensington"),"early_interval"]<-c("Late Asselian")
  bival[which(bival$formation=="Kensington"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  bival[which(bival$formation=="Lochinvar"),"early_interval"]<-c("Late Asselian")
  bival[which(bival$formation=="Lochinvar"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  bival[which(bival$formation=="Madeline"),"early_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Madeline"),"late_interval"]<-c("Late Artinskian") #Artinskian-Kungurian in PBDB. Haig et al. 2017

  bival[which(bival$formation=="Malbina"&bival$early_interval=="Roadian"),"early_interval"]<-c("Kungurian")
  bival[which(bival$formation=="Malbina"&bival$late_interval=="Roadian"),"late_interval"]<-c("Kungurian")
  bival[which(bival$formation=="Malbina"&bival$early_interval=="Wordian"),"early_interval"]<-c("Roadian")
  bival[which(bival$formation=="Malbina"&bival$late_interval=="Wordian"),"late_interval"]<-c("Roadian") #According to the brachiopod zone and the ages shown in Shi et al. 2022

  bival[which(bival$formation=="Mallens"),"early_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Mallens"),"late_interval"]<-c("Late Artinskian") #Artinskian in PBDB. Haig et al. 2017

  bival[which(bival$formation=="Mingenew"),"early_interval"]<-c("Artinskian")
  bival[which(bival$formation=="Mingenew"),"late_interval"]<-c("Artinskian") #Artinskian-Kungurian in PBDB. Artinskian in Waterhouse 2015

  bival[which(bival$formation=="Nalbia"),"early_interval"]<-c("Late Kungurian")
  bival[which(bival$formation=="Nalbia"),"late_interval"]<-c("Late Kungurian") #Kungurian in PBDB. Haig et al. 2017

  bival[which(bival$formation=="Noonkanbah"&bival$early_interval=="Artinskian"),"early_interval"]<-c("Late Artinskian")
  bival[which(bival$formation=="Noonkanbah"&bival$late_interval=="Artinskian"),"late_interval"]<-c("Late Artinskian") #Haig et al. 2017

  bival[which(bival$formation=="One Gum"),"early_interval"]<-c("Early Artinskian")
  bival[which(bival$formation=="One Gum"),"late_interval"]<-c("Early Artinskian") #Artinskian in PBDB. Strophalosia jimbaensis zone

  bival[which(bival$formation=="Poole"&bival$early_interval=="Sakmarian"),"early_interval"]<-c("Late Sakmarian")
  bival[which(bival$formation=="Poole"&bival$late_interval=="Sakmarian"),"late_interval"]<-c("Late Sakmarian")
  bival[which(bival$formation=="Poole"&bival$early_interval=="Artinskian"),"early_interval"]<-c("Early Artinskian")
  bival[which(bival$formation=="Poole"&bival$late_interval=="Artinskian"),"late_interval"]<-c("Early Artinskian") #Haig et al. 2017

  bival[which(bival$formation=="Quamby"),"early_interval"]<-c("Late Asselian")
  bival[which(bival$formation=="Quamby"),"late_interval"]<-c("Early Sakmarian") #Asselian-Samkarian in PBDB; Asselian in Waterhouse 2015

  bival[which(bival$formation=="Quinnanie"),"early_interval"]<-c("Early Kungurian")
  bival[which(bival$formation=="Quinnanie"),"late_interval"]<-c("Early Kungurian") #Artinskian in PBDB. Haig et al. 2017

  bival[which(bival$formation=="Rammutt"),"early_interval"]<-c("Late Asselian")
  bival[which(bival$formation=="Rammutt"),"late_interval"]<-c("Sakmarian") #Artinskian in PBDB. Asselian in Waterhouse 2015

  bival[which(bival$formation=="Rutherford"),"early_interval"]<-c("Sakmarian")
  bival[which(bival$formation=="Rutherford"),"late_interval"]<-c("Sakmarian") #Sakmarian-Artinskian in PBDB. Sakmarian in Waterhouse 2015

  bival[which(bival$formation=="Silver Spur"),"early_interval"]<-c("Sakmarian")
  bival[which(bival$formation=="Silver Spur"),"late_interval"]<-c("Sakmarian") #Sakmarian-Artinskian in PBDB. Asselian in Waterhouse 2015. Bandoproductus Zone. The Artinskian records in PBDB include some species possible of the Bookeria Zone

  bival[which(bival$formation=="Snapper Point"),"early_interval"]<-c("Early Kungurian")
  bival[which(bival$formation=="Snapper Point"),"late_interval"]<-c("Early Kungurian") #Roadian in PBDB. Shi et al. 2022

  bival[which(bival$formation=="South Curra"),"early_interval"]<-c("Changhsingian")
  bival[which(bival$formation=="South Curra"),"late_interval"]<-c("Changhsingian") #Artinskian-Wuchiapingian in PBDB. Changhsingian in Waterhouse 2015

  bival[which(bival$formation=="Spreyton"),"early_interval"]<-c("Late Asselian")
  bival[which(bival$formation=="Spreyton"),"late_interval"]<-c("Early Sakmarian") #Sakmarian in PBDB. May be Strophalosiaria concentrica Zone

  bival[which(bival$formation=="Stanleigh"),"early_interval"]<-c("Sakmarian")
  bival[which(bival$formation=="Stanleigh"),"late_interval"]<-c("Sakmarian") #Artinskian in PBDB. The species Terrakea pollex possibly belongs to the Bookeria Zone

  bival[which(bival$formation=="Swifts Jetty Sandstone"),"early_interval"]<-c("Late Asselian")
  bival[which(bival$formation=="Swifts Jetty Sandstone"),"late_interval"]<-c("Early Sakmarian")
  bival[which(bival$stratgroup=="Masseys Creek"),"early_interval"]<-c("Late Asselian")
  bival[which(bival$stratgroup=="Masseys Creek"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  bival[which(bival$formation=="Tiverton"),"early_interval"]<-c("Late Sakmarian")
  bival[which(bival$formation=="Tiverton"),"late_interval"]<-c("Artinskian") #Artinskian in PBDB. Waterhouse 2015

  bival[which(bival$formation=="Tomago"),"early_interval"]<-c("Wuchiapingian")
  bival[which(bival$formation=="Tomago"),"late_interval"]<-c("Wuchiapingian") #Melehan et al. 2021. Wordian in PBDB

  bival[which(bival$formation=="Wallaby"),"early_interval"]<-c("Sakmarian")
  bival[which(bival$formation=="Wallaby"),"late_interval"]<-c("Sakmarian") #Artinskian in PBDB. Sakmarian in Waterhouse 2015

  bival[which(bival$formation=="Wandrawandian"),"early_interval"]<-c("Late Kungurian")
  bival[which(bival$formation=="Wandrawandian"),"late_interval"]<-c("Roadian") #Roadian-Wordian in PBDB. Shi et al. 2022

  bival[which(bival$formation=="Wasp Head"),"early_interval"]<-c("Late Asselian")
  bival[which(bival$formation=="Wasp Head"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  bival[which(bival$formation=="Watermark"),"early_interval"]<-c("Capitanian")
  bival[which(bival$formation=="Watermark"),"late_interval"]<-c("Capitanian") #Wordian or Wuchiapingian in PBDB. Laurie et al. 2016

  bival[which(bival$formation=="Woody Island"),"early_interval"]<-c("Late Asselian")
  bival[which(bival$formation=="Woody Island"),"late_interval"]<-c("Early Sakmarian") #Samkarian in PBDB; Asselian in Waterhouse 2015

  bival[which(bival$formation=="Yarrol"),"early_interval"]<-c("Late Sakmarian")
  bival[which(bival$formation=="Yarrol"),"late_interval"]<-c("Artinskian") #Artinskian in PBDB. Sakmarian in Waterhouse 2015

  bival[which(bival$stratgroup=="Lyons"&bival$early_interval=="Sakmarian"),"early_interval"]<-c("Early Sakmarian")
  bival[which(bival$stratgroup=="Lyons"&bival$late_interval=="Sakmarian"),"late_interval"]<-c("Early Sakmarian") #See Waterhouse 2015

  bival[which(bival$stratgroup=="Back Creek"&bival$early_interval=="Kungurian"),"early_interval"]<-c("Artinskian")
  bival[which(bival$stratgroup=="Back Creek"&bival$late_interval=="Kungurian"),"late_interval"]<-c("Artinskian") #Echinalosia preovalis Zone of Briggs 1998

  return(bival)
}
