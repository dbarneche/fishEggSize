tableRounding  <-  function (x, rounding = 2) {
    format(round(x, rounding), nsmall = rounding)
}

makeTable1  <-  function (filteredData) {
	data    <-  filteredData$data
	table1  <-  Hmisc::rcorr(as.matrix(data[, c('sst_seasonality', 'sst_environmentalColor', 'sst_average', 'chl_seasonality', 'chl_environmentalColor', 'chl_average')]), type = 'pearson')
	significant  <-  table1$P < 0.001
	table1  <-  matrix(as.character(round(table1$r, 2)), nrow(table1$r), ncol(table1$r))
	table1  <-  ifelse(significant, paste0('\\textbf{', table1, '}'), table1)
	table1[is.na(table1)]  <-  '-'
	table1
}

writeTable1  <-  function (dest, table1) {
	write.csv(format(table1, trim = TRUE, digits = 2), dest, row.names = TRUE)
}

makeTable2  <-  function (allModels) {
    bestModel       <-  allModels$model4
	fixedEstimates  <-  summary(bestModel)$fixed
	sdEstimates     <-  do.call(rbind.data.frame, summary(bestModel)$random)
	fixedEstimates[, 'Eff.Sample']  <-  round(fixedEstimates[, 'Eff.Sample'])
	sdEstimates[, 'Eff.Sample']     <-  round(sdEstimates[, 'Eff.Sample'])
	cbind(
	rbind(as.matrix(tableRounding(sdEstimates[2:1, c(1, 3, 4)], 2)), matrix(rep('', 6), 2, 3), as.matrix(tableRounding(fixedEstimates[, c(1, 3, 4)], 2))),
	rbind(as.matrix(sdEstimates[2:1, 5]), matrix(rep('', 2), 2, 1), as.matrix(fixedEstimates[, 5]))
	)
}

writeTable2  <-  function (dest, table2) {
	write.csv(format(table2, trim = TRUE, digits = 2), dest, row.names = TRUE)
}

makeTableS1  <-  function (spawningCheckModel) {
	fixedEstimates  <-  summary(spawningCheckModel)$fixed
	sdEstimates     <-  do.call(rbind.data.frame, summary(spawningCheckModel)$random)
	fixedEstimates[, 'Eff.Sample']  <-  round(fixedEstimates[, 'Eff.Sample'])
	sdEstimates[, 'Eff.Sample']     <-  round(sdEstimates[, 'Eff.Sample'])
	cbind(
	rbind(as.matrix(tableRounding(sdEstimates[2:1, c(1, 3, 4)], 2)), matrix(rep('', 6), 2, 3), as.matrix(tableRounding(fixedEstimates[, c(1, 3, 4)], 2))),
	rbind(as.matrix(sdEstimates[2:1, 5]), matrix(rep('', 2), 2, 1), as.matrix(fixedEstimates[, 5]))
	)
}

writeTableS1  <-  function (dest, tableS1) {
	write.csv(format(tableS1, trim = TRUE, digits = 2), dest, row.names = TRUE)
}
