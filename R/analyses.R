####################
# GENERAL STAN SPECS
####################
options(mc.cores = parallel::detectCores())

###################
# GENERAL FUNCTIONS
###################
readFile  <-  function (filePath, ...) {
    read.csv(filePath, header = TRUE, stringsAsFactors = FALSE, ...)
}

niceThousands  <-  function (number) {
    formatC(number, format = 'd', big.mark = ',')
}

#########################
# DOWNLOAD DATA FROM NOAA
#########################
checkIfNoaaErddapLayersExist  <-  function () {
    for (j in c('chlorophyll', 'sst')) {
        ncdfFiles   <-  noaaErddap::noaaErddapFiles(j, full.name = TRUE)
        needed      <-  dumpRequiredFileDates(j)
        dates       <-  sort(unique(extractFileDates(ncdfFiles, j)))
        if (!all(needed %in% dates)) {
            message(sprintf('You currently do not have all the required %s layers; attempting to download...', j), '\n')
            neededDownloadData  <-  createNeededDownloadData(neededDates = needed, observedDates = dates, type = j)
            plyr::ddply(neededDownloadData, .(year, month), consistentLayerDownload, type = j)
        }
    }
    invisible(NULL)
}

dumpRequiredFileDates  <-  function (type) {
    if (type == 'sst') {
        as.character(1997:2007)
    } else if (type == 'chlorophyll') {
        sort(c(paste0(month.name[9:12], '-1997'), sapply(month.name, function (x) paste0(x, '-', 1998:2007))))
    }
}

extractFileDates  <-  function (fileNames, type) {
    tools::file_path_sans_ext(basename(fileNames))
}

createNeededDownloadData  <-  function (neededDates, observedDates, type) {
    if (type == 'sst') {
        data.frame(year = as.numeric(neededDates[!(neededDates %in% observedDates)]), month = 1)
    } else if (type == 'chlorophyll') {
        stillNeeded  <-  neededDates[!(neededDates %in% observedDates)]
        plyr::ldply(stillNeeded, function (x) {
            datesSplit  <-  strsplit(x, '-')[[1]]
            data.frame(year = as.numeric(datesSplit[2]), month = match(datesSplit[1], month.name), stringsAsFactors = FALSE)
        })
    }
}

consistentLayerDownload  <-  function (...) {
    links  <-  downloadLayer(...)
    cat('\n\n')
    while (inherits(links, 'try-error')) {
        links  <-  downloadLayer(...)
        cat('\n\n')
    }
    data.frame(links = links, stringsAsFactors = FALSE)
}

downloadLayer  <-  function (data, type) {
    try(noaaErddap::erddapDownload(year = data$year, month = data$month, type = type, overwrite = TRUE, httr::timeout(4800), httr::verbose()), silent = TRUE)
}

####################
# PHYLOGENETIC TREES
####################
readTree  <-  function (treeTopology) {
    ape::read.tree(text = treeTopology)
}

clinidaeTree   <-  function () {
    message('Heterostichus rostratus reinserted assuming that Clinidae is a sister group to Blenniidae\n')
    readTree('(Heterostichus_rostratus_ott257317);')
}

barracudaTree   <-  function () {
    message('Sphyraena argentea reinserted assuming that Sphyraenidae is a sister group to Carangidae-Xiphiidae (see Fig. 7, Carangimorphariae relationships in Betancur-R. et al. 2013 PLOS Currents Tree of Life. doi: 10.1371/currents.tol.53ba26640df0ccaee75bb165c8c26288\n')
    readTree('(Sphyraena_argentea_ott757641);')
}

damselTree  <-  function () {
    damselTopology  <-  '((((Microspathodon_bairdii_ott237630,(Microspathodon_chrysurus_ott847660,Microspathodon_dorsalis_ott205758)),Hypsypops_rubicundus_ott847666),((Stegastes_planifrons_ott665837,Stegastes_acapulcoensis_ott3635554),(Stegastes_partitus_ott345269,(Stegastes_flavilatus_ott3635541,((Stegastes_variabilis_ott323173,Stegastes_leucostictus_ott100830),(Stegastes_adustus_ott323181,(Stegastes_fuscus_ott3635543,Stegastes_diencaeus_ott729161))))))),((Chromis_atripectoralis_ott741423,(Chromis_multilineata_ott437016,Chromis_atrilobata_ott436999)),(((Abudefduf_septemfasciatus_ott129790,Abudefduf_sordidus_ott1053071),(Abudefduf_bengalensis_ott318931,(Abudefduf_vaigiensis_ott1053067,(Abudefduf_saxatilis_ott405751,Abudefduf_troschelii_ott961357)))),((Acanthochromis_polyacanthus_ott100410,Amphiprion_melanopus_ott45635),(Pomacentrus_amboinensis_ott880239,Pomacentrus_coelestis_ott622060)))));'
    message('Pomacentridae species reinserted right next to Labridae using topology published in Frédérich et al. 2013 Am Nat 181, 94--113\n')
    readTree(damselTopology)
}

########################################
# TREE EXTRACTION AND MATCHING WITH DATA
########################################
downloadAndProcessTree  <-  function () {
    actn  <-  rotl::tnrs_match_names(names = 'Actinopterygii', context_name = 'Animals')
    tree  <-  rotl::tol_subtree(ott_id = actn$ott_id)
    attr(tree, 'order')  <-  'cladewise'
    # add Dipnoi species as an outer group, necessary to root the tree
    out   <-  rotl::tol_subtree(ott_id = rotl::tnrs_match_names(names = 'Lepidosiren paradoxa', context_name = 'Animals')$ott_id)
    tree2 <-  readTree(paste0('(', out, ');'))
    tp    <-  ape::getMRCA(tree, tree$tip.label)
    tree  <-  ape::bind.tree(tree, tree2, where = tp)
    tree  <-  ape::root(tree, outgroup = out, resolve.root = TRUE)
    tree  <-  ape::compute.brlen(tree, method = 'Grafen')
    tree  <-  graftMissingTree(tree, treeToBeInsertedFun = list('damselTree', 'barracudaTree', 'clinidaeTree'), anchorFamily = list('Labridae', 'Carangidae', 'Blenniidae'))
    invisible(tree)
}

graftMissingTree  <-  function (tree, treeToBeInsertedFun, anchorFamily) {
    spp  <-  sapply(tree$tip.label, function (x)paste(strsplit(x, '_')[[1]][1:2], collapse = ' '))
    for (i in seq_along(treeToBeInsertedFun)) {
        newTree       <-  get(treeToBeInsertedFun[[i]])()
        fbSpp         <-  rfishbase::species_list(Family = anchorFamily[[i]])
        anchorPoint   <-  ape::getMRCA(tree, tree$tip.label[na.omit(match(fbSpp, spp))])
        tree          <-  ape::bind.tree(tree, newTree, where = anchorPoint)
    }
    # compute branch lengths on way out
    ape::compute.brlen(tree, method = 'Grafen')
}

cleanAndExtractPhyloTreeAndMatchedData  <-  function (tree, otlSubs, ...) {
    data  <-  readFile(...)
    data  <-  fixSpNames(data)
    all   <-  createSpeciesOtl(data, tree, otlSubs)
    tree  <-  replaceDuplicatedNodes(all$tree)
    tree  <-  treeCleanUpForAnalysis(all$data, tree)
    
    list('data' = all$data,
         'tree' = tree)
}

fixSpNames  <-  function (data) {
    # there are other Notothenia and Patagonotothen species in database; remove ambiguous species
    data  <-  data[!(data$Species %in% c('Notothenia sp.', 'Patagonotothen sp.', 'Auxis sp.')), ]
    data$Species[grep(' sp.', data$Species, fixed = TRUE)]  <-  gsub(' sp.', '', data$Species[grep(' sp.', data$Species, fixed = TRUE)], fixed = TRUE)
    data
}

modifyTreeTip  <-  function (treeTip, species) {
    brokenName  <-  strsplit(treeTip, '_')[[1]]
    paste0(gsub(' ', '_', species), '_', grep('ott', brokenName, value = TRUE))
}

treeCleanUpForAnalysis  <-  function (data, tree) {
    # find species that are in the data, but not in the tree, and remove them from data
    missingFromData  <-  setdiff(tree$tip.label, unique(data$otlSpecies))
    ape::drop.tip(tree, missingFromData)
}

replaceDuplicatedNodes  <-  function (tree) {
    if (any(duplicated(tree$node.label))) {
        tree$node.label  <-  paste0('ott', seq_along(tree$node.label))
    }
    tree
}

createSpeciesOtl  <-  function (data, tree, otlSubs) {
    # create a column in datafile to match OTL name
    # there are species from data that are found as subspecies in the
    # Actinopterygii tree from the Open Tree of Life. Some others are
    # not found in the tree: they are either misnomers or do not exist
    # at all, though have closely-related species from the same genera
    # so change all those to facilitate things. These are found in the
    # data.frame otlSubs
    spp        <-  unique(data$Species)
    ottFrames  <-  paste0(gsub(' ', '_', spp), '_ott')
    out        <-  data.frame()
    for (i in seq_along(ottFrames)) {
        x  <-  grep(ottFrames[i], tree$tip.label, fixed = TRUE)
        if (length(x) == 0) {
            x  <-  grep(otlSubs$ottFrame[otlSubs$Species == spp[i]], tree$tip.label, fixed = TRUE)
            tree$tip.label[x]  <-  modifyTreeTip(tree$tip.label[x], spp[i])
        }
        out  <-  rbind(out, data.frame(Species = spp[i], otlSpecies = tree$tip.label[x], stringsAsFactors = FALSE))
    }
    if (nrow(out) != nrow(out[complete.cases(out), ])) {
        stop('missing species in tree tips')
    }
    data$otlSpecies  <-  out$otlSpecies[match(data$Species, out$Species)]
    data$animal      <-  as.factor(data$otlSpecies)
    list('data' = data,
         'tree' = tree)
}

####################################
# MATCH AND CLEAN ENVIRONMENTAL DATA
####################################
makeCoordinatesTable  <-  function (phyloTreeAndData) {
    unique(phyloTreeAndData$data[, c('Longitude', 'Latitude')])
}

matchNcdfDataFrameStyle  <-  function (type, coordinates) {
    if (type != 'chlorophyll') {
        coordinates$Longitude[coordinates$Longitude < 0]  <-  coordinates$Longitude[coordinates$Longitude < 0] + 360
    }
    ncdfFiles   <-  noaaErddap::noaaErddapFiles(type, full.name = TRUE)
    dates       <-  extractFileDates(ncdfFiles, type)
    ncdfFiles   <-  ncdfFiles[dates %in% dumpRequiredFileDates(type)]
    ncdfFiles   <-  ncdfFiles[order(transformNoaaErddapFileNamesToDates(ncdfFiles))]
    ncdfValues  <-  plyr::ldply(ncdfFiles, openAndExtractNcdfData, coordinates = coordinates, type = type)
    ncdfValues  <-  ncdfValues[!duplicated(paste(ncdfValues$dates, ncdfValues$Longitude, ncdfValues$Latitude)), ]
    ncdfValues
}

transformNoaaErddapFileNamesToDates  <-  function (fileName) {
    zoo::as.Date(zoo::as.yearmon(gsub('-', ' ', tools::file_path_sans_ext(basename(fileName)))))
}

openAndExtractNcdfData  <-  function (filePath, coordinates, type) {
    dates   <-  getLayerDates(filePath, type)
    slices  <-  data.frame(dates = dates, band = seq_along(dates), stringsAsFactors = FALSE)
    plyr::ddply(slices, .(dates), dateFrameFromRaster, filePath = filePath, coordinates = coordinates, type = type)
}

getLayerDates  <-  function (filePath, type) {
    envNc   <-  ncdf4::nc_open(filename = filePath)
    dates   <-  noaaErddap::transformDateForLayer(envNc, type)
    ncdf4::nc_close(envNc)
    dates
}

dateFrameFromRaster  <-  function (slices, filePath, coordinates, type) {
    envNc    <-  raster::raster(filePath, band = slices$band)
    envVals  <-  raster::extract(envNc, coordinates, buffer = 2e5, fun = median, na.rm = TRUE)
    cbind(coordinates, variable = type, values = envVals, stringsAsFactors = FALSE)
}

###############################
# ENVIRONMENTAL COLOR FUNCTIONS
###############################
makeEnvironmentalPredictabilityTable  <-  function (chlTable, sstTable) {
    chlTable$Longitude[chlTable$Longitude < 0]  <-  chlTable$Longitude[chlTable$Longitude < 0] + 360
    rbind(cbind(variable = 'chlorophyll',  extractEnvPredForAllCoordinates(envTable = chlTable, datesVecName = 'dates', delta = 8, isUneven = TRUE, interpolate = TRUE, noiseMethod = 'LombScargle', showWarnings = FALSE), stringsAsFactors = FALSE),
          cbind(variable = 'sst', extractEnvPredForAllCoordinates(envTable = sstTable, datesVecName = 'dates', delta = 1, isUneven = FALSE, interpolate = FALSE, noiseMethod = 'spectrum', showWarnings = FALSE), stringsAsFactors = FALSE)
    )
}

extractEnvPredForAllCoordinates  <-  function (envTable, datesVecName, ...) {
    plyr::ddply(envTable, .(Longitude, Latitude), function (data, datesVecName, ...) {
        envPred::envPredictability(rawTimeSeries = data$values, datesVector = data[[datesVecName]], ...)
    }, datesVecName = datesVecName, ...)
}

produceAbioticData  <-  function (chlTable, sstTable, predictabilityTable) {
    chlTable$Longitude[chlTable$Longitude < 0]  <-  chlTable$Longitude[chlTable$Longitude < 0] + 360
    chlDat       <-  cbind(variable = 'chlorophyll', chlTable, stringsAsFactors = FALSE)
    sstDat       <-  cbind(variable = 'sst', sstTable, stringsAsFactors = FALSE)
    envSummary   <-  rbind(ddply(chlDat, .(variable, Longitude, Latitude), produceEnvSummary), ddply(sstDat, .(variable, Longitude, Latitude), produceEnvSummary))
    abioticData  <-  merge(predictabilityTable[, c('variable', 'Longitude', 'Latitude', 'boundedSeasonality', 'environmentalColor')], envSummary)
    aDatChl      <-  abioticData[abioticData$variable == 'chlorophyll', setdiff(names(abioticData), c('variable', 'Longitude', 'Latitude'))]
    aDatSst      <-  abioticData[abioticData$variable == 'sst', setdiff(names(abioticData), 'variable')]
    
    names(aDatChl)      <-  paste0('chl_', names(aDatChl))
    names(aDatChl)[names(aDatChl) == 'chl_boundedSeasonality']  <-  'chl_seasonality'
    notCoordinateNames  <-  which(!(names(aDatSst) %in% c('Longitude', 'Latitude')))
    names(aDatSst)[notCoordinateNames]  <-  paste0('sst_', names(aDatSst)[notCoordinateNames])
    names(aDatSst)[names(aDatSst) == 'sst_boundedSeasonality']  <-  'sst_seasonality'
    cbind(aDatSst, aDatChl)
}

produceEnvSummary  <-  function (data) {
    missing  <-  nrow(data[is.na(data$values), ])
    means    <-  mean(data$values, na.rm = TRUE)
    sds      <-  sd(data$values, na.rm = TRUE)
    data.frame(average           =  means,
               cvPercent         =  sds / means * 100,
               missing           =  missing,
               coveragePercent   =  round((1 - missing / nrow(data)) * 100, 2),
               stringsAsFactors  =  FALSE
    )
}

###########################
# PHYLOGENETIC HIERARCHICAL
# BAYESIAN MODELS
###########################
filterDataForAnalysis  <-  function (phyloTreeAndData, abioticData) {
    data  <-  phyloTreeAndData$data
    tree  <-  phyloTreeAndData$tree
    
    data$Longitude[data$Longitude < 0]  <-  data$Longitude[data$Longitude < 0] + 360
    data                                <-  merge(data, abioticData)
    
    # remove data with < 87% of coverage
    data         <-  data[data$chl_coveragePercent >= 87, ]
    tree         <-  ape::drop.tip(tree, tree$tip.label[!(tree$tip.label %in% unique(data$otlSpecies))])

    list('data' = data,
         'tree' = tree)
}

brmsModel  <-  function (predictorAsCharacter, data, A) {
    eval(parse(text = sprintf("brms::brm(lnEggSize ~ %s + (1 | animal) + (1 | otlSpecies), data = data, family = gaussian(), cov_ranef = list(animal = A), prior = c(brms::prior(normal(1, 2), 'b'), brms::prior(normal(3, 3), 'Intercept'), brms::prior(student_t(3, 0, 20), 'sd'), brms::prior(student_t(3, 0, 20), 'sigma')), sample_prior = TRUE, chains = 3, cores = 3, iter = 1.5e4, warmup = 1.5e4 / 2, control = list(adapt_delta = 0.999, max_treedepth = 20))", predictorAsCharacter)))
}

runAllModels  <-  function (filteredData) {
    data  <-  filteredData$data
    tree  <-  filteredData$tree
    A     <-  ape::vcv(tree, corr = FALSE)
    list(
        model1   =  brmsModel('spawningMode + sst_average + chl_average + chl_environmentalColor + chl_seasonality + sst_environmentalColor + sst_seasonality', data = data, A = A),
        model2   =  brmsModel('sst_average + chl_average + chl_environmentalColor + chl_seasonality + sst_environmentalColor + sst_seasonality', data = data, A = A),
        model3   =  brmsModel('spawningMode + sst_average + chl_average + chl_environmentalColor + chl_seasonality + sst_seasonality', data = data, A = A),
        model4   =  brmsModel('sst_average + chl_average + chl_environmentalColor + chl_seasonality + sst_seasonality', data = data, A = A)
    )
}

runSpawningCheckModel  <-  function (filteredData, spawningModeTab) {
    data       <-  filteredData$data
    sourceSpw  <-  spawningModeTab$source[match(data$Species, spawningModeTab$species)]
    
    data  <-  data[!is.na(sourceSpw), ]
    tree  <-  filteredData$tree
    tree  <-  ape::drop.tip(tree, tree$tip.label[!(tree$tip.label %in% unique(data$otlSpecies))])
    A     <-  ape::vcv(tree, corr = FALSE)

    brmsModel('sst_average + chl_average + chl_environmentalColor + chl_seasonality + sst_seasonality', data = data, A = A)
}

looComparison  <-  function (allModels) {
    looCompTabIc     <-  brms::LOO(allModels$model1, allModels$model2, allModels$model3, allModels$model4, compare = TRUE, pointwise = FALSE, update_args = list('chains' = 3, 'cores' = 3))
    looCompTab       <-  data.frame(looCompTabIc$ic_diffs__)
    looCompTab$pVal  <-  pValFromMeanAndSe(looCompTab$LOOIC, looCompTab$SE)
    looCompTab$sgnf  <-  looCompTab$pVal < 0.05
    looCompTab
}

kFoldComparison  <-  function (allModels) {
    brms::kfold(allModels$model1, allModels$model2, allModels$model3, allModels$model4, compare = TRUE, K = 10, update_args = list('chains' = 3, 'cores' = 3))
}

pValFromMeanAndSe  <-  function (meanVal, seVal) {
    2 * pnorm(-abs((meanVal - 0) / seVal))
}

getSummaryTableOfLooPairwiseComparisons  <-  function (comparisonTab) {
    plyr::ddply(comparisonTab, .(baseModel), function (x) {
        data.frame(nPositiveElpdDiff = length(x$elpd_diff[x$elpd_diff > 0]), stringsAsFactors = FALSE)
    })
}
