###############
# LOAD CM ROMAN
###############
extrafont::loadfonts(quiet = TRUE)

######################
# AUXILLIARY FUNCTIONS
######################
toDev  <-  function (expr, dev, filename, ..., verbose = TRUE) {
    if (verbose) {
        cat(sprintf('Creating %s\n', filename))
    }
    dev(filename, family = 'CM Roman', ...)
    on.exit(dev.off())
    eval.parent(substitute(expr))
}

toPdf  <-  function (expr, filename, ...) {
    toDev(expr, pdf, filename, ...)
}

##########################
# ACTUAL FIGURES FUNCTIONS
##########################
makeFig1  <-  function (dest, ...) {
    toPdf(fig1(...), dest, width = 6, height = 6)
    extrafont::embed_fonts(dest)
}

fig1  <-  function (data) {
    data         <-  data$data
    par(omi = rep(0.5, 4), cex = 1, cex.lab = 1.2)
    plot(NA, xlab = 'Absolute latitude (degrees)', ylab = 'Egg diameter (mm)', xlim = c(0, 50), ylim = c(0, 10), axes = FALSE, xpd = NA)
    usr  <-  par('usr')
    rect(usr[1], usr[3], usr[2], usr[4], col = 'grey90', border = NA)
    LoLinR::whiteGrid()
    box()
    axis(1)
    axis(2, las = 1)
    cols  <-  ifelse(data$Latitude < 0, 'dodgerblue2', 'tomato')
    points(data$absLatitude, data$eggSize_mm, col = LoLinR::transparentColor(cols, 0.8), pch = 16, cex = 1.3)
    LoLinR::proportionalLabel(0.05, 0.95, substitute(italic('n') == a, list(a = niceThousands(nrow(data)))), adj = c(0, 0.5), cex = 1)
    LoLinR::proportionalLabel(0.05, 0.85, paste0(length(unique(data$Species)), ' spp.'), adj = c(0, 0.5), cex = 1)
}

makeFig2  <-  function (dest, ...) {
    toPdf(fig2(...), dest, width = 9, height = 4.5)
    extrafont::embed_fonts(dest)
}

fig2  <-  function (filteredData, allModels) {
    data            <-  filteredData$data
    eggBestModel    <-  allModels$model4
    fixedEstimates  <-  brms::fixef(eggBestModel, estimate = 'mean')
    intercept       <-  fixedEstimates['Intercept', 'Estimate']
    sstSlope        <-  fixedEstimates['sst_average', 'Estimate']
    chlSlope        <-  fixedEstimates['chl_average', 'Estimate']
    sstSeasonSlope  <-  fixedEstimates['sst_seasonality', 'Estimate']
    chlSeasonSlope  <-  fixedEstimates['chl_seasonality', 'Estimate']
    chlColourSlope  <-  fixedEstimates['chl_environmentalColor', 'Estimate']
    
    # random model estimates
    averageAnimalRandom  <-  brms::ranef(eggBestModel, estimate = 'mean')$animal[, , 'Intercept'][as.character(data$animal), 'Estimate']
    averageSppRandom     <-  brms::ranef(eggBestModel, estimate = 'mean')$otlSpecies[, , 'Intercept'][data$otlSpecies, 'Estimate']
    
    meanEffects  <-  chlSlope * mean(data$chl_average) + sstSeasonSlope * mean(data$sst_seasonality) + chlSeasonSlope * mean(data$chl_seasonality) + chlColourSlope * mean(data$chl_environmentalColor)

    # correct response
    data$lnChlCorrectedEggSize   <-  data$lnEggSize - chlSlope * data$chl_average - sstSeasonSlope * data$sst_seasonality - chlSeasonSlope * data$chl_seasonality - chlColourSlope * data$chl_environmentalColor - averageSppRandom - averageAnimalRandom + meanEffects
    
    par(mfrow = c(1, 2), mai = c(1.22, 1.3, 0.62, 0.12), cex = 1, cex.lab = 1.1)
    plot(NA, xlab = expression(paste('Temperature (' * degree, 'C)', sep = '')), ylab = 'Egg diameter (mm)', xlim = c(0, 32), ylim = c(-3, 3), axes = FALSE, xpd = NA)
    usr  <-  par('usr')
    rect(usr[1], usr[3], usr[2], usr[4], col = 'grey90', border = NA)
    LoLinR::whiteGrid()
    box()
    axis(1)
    axis(2, at = log(c(0.05, 0.14, 0.37, 1, 2.72, 7.39, 20)), labels = c(0.05, 0.14, 0.37, '1.00', 2.72, 7.39, '20.0'), las = 1)
    
    iters    <-  brms::posterior_samples(eggBestModel, pars = 'b_')
    allInts  <-  iters[, 'b_Intercept']
    allSlps  <-  iters[, 'b_sst_average']
    
    xs  <-  seq(min(data$sst_average), max(data$sst_average), length.out = 30)
    lw  <-  up  <-  numeric(length = length(xs))
    for (i in seq_along(xs)) {
        vals  <-  allInts + allSlps * xs[i] + meanEffects
        lw[i] <-  quantile(vals, probs = 0.025)
        up[i] <-  quantile(vals, probs = 0.975)
    }
    
    points(data$sst_average, data$lnChlCorrectedEggSize, col = LoLinR::transparentColor('tomato', 0.8), pch = 16, cex = 1.3)
    lines(range(data$sst_average), intercept + sstSlope * range(data$sst_average) + meanEffects, lwd = 1.4, lty = 2)
    lines(xs, lw, lwd = 1, lty = 2, col = 'grey30')
    lines(xs, up, lwd = 1, lty = 2, col = 'grey30')
    
    LoLinR::proportionalLabel(0.05, 0.95, substitute(italic(y) == b%.%e^{a%.%italic(x)}, list(b = round(exp(intercept), 2), a = round(sstSlope, 2))), adj = c(0, 0.5), cex = 0.9)
    LoLinR::proportionalLabel(0.05, 0.18, substitute(italic('n') == a, list(a = niceThousands(nrow(data)))), adj = c(0, 0.5), cex = 0.9)
    LoLinR::proportionalLabel(0.05, 0.08, paste0(length(unique(data$otlSpecies)), ' spp.'), adj = c(0, 0.5), cex = 0.9)
    LoLinR::proportionalLabel(0, 1.08, '(a)', adj = c(0, 0.5), xpd = NA, font = 3)

    meanEffects  <-  sstSlope * mean(data$sst_average) + sstSeasonSlope * mean(data$sst_seasonality) + chlSeasonSlope * mean(data$chl_seasonality) + chlColourSlope * mean(data$chl_environmentalColor)
    
    # correct response
    data$lnChlCorrectedEggSize   <-  data$lnEggSize - sstSlope * data$sst_average - sstSeasonSlope * data$sst_seasonality - chlSeasonSlope * data$chl_seasonality - chlColourSlope * data$chl_environmentalColor - averageSppRandom - averageAnimalRandom + meanEffects
    
    par(mai = c(1.22, 0.3, 0.62, 1.12), cex = 1, cex.lab = 1.1)
    plot(NA, xlab = substitute('Chlorophyll-a (mg m'^3 * ')'), ylab = '', xlim = c(0, 2.5), ylim = c(-3, 3), axes = FALSE, xpd = NA)
    usr  <-  par('usr')
    rect(usr[1], usr[3], usr[2], usr[4], col = 'grey90', border = NA)
    LoLinR::whiteGrid()
    box()
    axis(1)
    axis(2, labels = NA, las = 1)
    
    iters    <-  brms::posterior_samples(eggBestModel, pars = 'b_')
    allInts  <-  iters[, 'b_Intercept']
    allSlps  <-  iters[, 'b_chl_average']
    
    xs  <-  seq(min(data$chl_average), max(data$chl_average), length.out = 30)
    lw  <-  up  <-  numeric(length = length(xs))
    for (i in seq_along(xs)) {
        vals  <-  allInts + allSlps * xs[i] + meanEffects
        lw[i] <-  quantile(vals, probs = 0.025)
        up[i] <-  quantile(vals, probs = 0.975)
    }
    
    points(data$chl_average, data$lnChlCorrectedEggSize, col = LoLinR::transparentColor('seagreen3', 0.8), pch = 16, cex = 1.3)
    lines(range(data$chl_average), intercept + chlSlope * range(data$chl_average) + meanEffects, lwd = 1.4, lty = 2)
    lines(xs, lw, lwd = 1, lty = 2, col = 'grey30')
    lines(xs, up, lwd = 1, lty = 2, col = 'grey30')
    
    LoLinR::proportionalLabel(0.05, 0.95, substitute(italic(y) == b%.%e^{a%.%italic(x)}, list(b = round(exp(intercept), 2), a = round(chlSlope, 2))), adj = c(0, 0.5), cex = 0.9)
    LoLinR::proportionalLabel(0, 1.08, '(b)', adj = c(0, 0.5), xpd = NA, font = 3)
}

makeFig3  <-  function (dest, ...) {
    toPdf(fig3(...), dest, width = 6.5, height = 6)
    extrafont::embed_fonts(dest)
}

fig3  <-  function (filteredData, allModels) {
    data            <-  filteredData$data
    eggBestModel    <-  allModels$model4
    fixedEstimates  <-  brms::fixef(eggBestModel, estimate = 'mean')
    intercept       <-  fixedEstimates['Intercept', 'Estimate']
    sstSlope        <-  fixedEstimates['sst_average', 'Estimate']
    chlSlope        <-  fixedEstimates['chl_average', 'Estimate']
    sstSeasonSlope  <-  fixedEstimates['sst_seasonality', 'Estimate']
    chlSeasonSlope  <-  fixedEstimates['chl_seasonality', 'Estimate']
    chlColourSlope  <-  fixedEstimates['chl_environmentalColor', 'Estimate']
    otherEffects    <-  sstSlope * mean(data$sst_average) + chlSlope * mean(data$chl_average) + sstSeasonSlope * mean(data$sst_seasonality)
    
    par(omi = c(0.5, 1, 0.5, 0.5), cex = 1, cex.lab = 1.1)
    plot(NA, xlab = 'Chlorophyll-a Seasonality', ylab = 'Chlorophyll-a Colour', xlim = range(data$chl_seasonality), ylim = range(data$chl_environmentalColor), axes = FALSE, xpd = NA)

    cols    <-  rev(colorRampPalette(RColorBrewer::brewer.pal(11, 'RdBu'))(500))
    vals    <-  exp(intercept + c(par('usr')[1] * chlSeasonSlope + par('usr')[3] * chlColourSlope, par('usr')[1] * chlSeasonSlope + par('usr')[4] * chlColourSlope, par('usr')[2] * chlSeasonSlope + par('usr')[3] * chlColourSlope, par('usr')[2] * chlSeasonSlope + par('usr')[4] * chlColourSlope) + otherEffects)
    minVal  <-  min(vals)
    maxVal  <-  max(vals)
    valVec  <-  round(seq(minVal, maxVal, length.out = length(cols)), 2)
    for (i in seq(par('usr')[1], par('usr')[2], length.out = 500)) {
        for (j in seq(par('usr')[3], par('usr')[4], length.out = 500)) {
            calcVal  <-  round(exp(intercept + i * chlSeasonSlope + j * chlColourSlope + otherEffects), 2)
            points(i, j, col = cols[match(calcVal, valVec)], pch = 16, cex = 0.3)
        }
    }
    points(data$chl_seasonality, data$chl_environmentalColor, pch = 16, cex = 0.8)
    box()
    axis(1)
    axis(2, las = 1)
    seqs  <-  seq(0.25, 0.75, length.out = 500)
    for (i in seq_along(cols)) {
        LoLinR::proportionalLabel(rep(seqs[i], 2), c(1.2, 1.3), text = FALSE, type = 'l', col = cols[i], xpd = NA)
    }
    LoLinR::proportionalLabel(c(0.25, 0.75, 0.75, 0.25, 0.25), c(1.2, 1.2, 1.3, 1.3, 1.2), text = FALSE, type = 'l', xpd = NA)
    LoLinR::proportionalLabel(0.5, 1.35, 'Egg diameter (mm)', adj = c(0.5, 0.5), xpd = NA, cex = 1.1)
    LoLinR::proportionalLabel(0.25, 1.18, round(minVal, 2), xpd = NA, adj = c(0.5, 1), cex = 1.1)
    LoLinR::proportionalLabel(0.75, 1.18, round(maxVal, 2), xpd = NA, adj = c(0.5, 1), cex = 1.1)
}
