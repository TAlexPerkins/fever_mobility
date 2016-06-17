# histograms of age distributions by sex
pdf(file = '../output/participantsAgeBySex.pdf', width = 6.5, height = 4)

par(mar = c(2, 1, 2, 1), oma = c(3, 4, 2, 3))

layout(rbind(c(1, 3, 5), c(2, 4, 6)))

age.breaks = seq(0, 90, by = 15)

# home, all
ageBySex = rbind(
  hist(as.numeric(
    partHome$age[partHome$sex == 'F' & partHome$pathogen == 'NA'][
      !is.null(partHome$age[partHome$sex == 'F' & partHome$pathogen == 'NA'])]),
    age.breaks, plot = FALSE)$density,
  hist(as.numeric(
    partHome$age[partHome$sex == 'M' & partHome$pathogen == 'NA'][
      !is.null(partHome$age[partHome$sex == 'M' & partHome$pathogen == 'NA'])]),
    age.breaks, plot = FALSE)$density)
ageBySex = ageBySex / rowSums(ageBySex)
barplot(ageBySex,
        names.arg = paste(head(age.breaks, -1), tail(age.breaks, -1), sep = '-'),
        beside = TRUE,
        col = c(rgb(1, 0, 1, .8), rgb(0, 0, 1, .8)),
        cex.axis = .65, cex.names = .65,
        ylim = c(0, .45))
mtext('Frequency', 2, 3, at = -.12)
mtext('Afebrile', 3, 2)

# away, afebrile
ageBySex = rbind(
  hist(as.numeric(
    participants.notHome[,'age'][participants.notHome[,'sex'] == 'F' & participants.notHome[,'pathogen'] == 'NA'][
      !is.null(participants.notHome[,'age'][participants.notHome[,'sex'] == 'F' & participants.notHome[,'pathogen'] == 'NA'])]),
    age.breaks, plot = FALSE)$density,
  hist(as.numeric(
    participants.notHome[,'age'][participants.notHome[,'sex'] == 'M' & participants.notHome[,'pathogen'] == 'NA'][
      !is.null(participants.notHome[,'age'][participants.notHome[,'sex'] == 'M' & participants.notHome[,'pathogen'] == 'NA'])]),
    age.breaks, plot = FALSE)$density)
ageBySex = ageBySex / rowSums(ageBySex)
barplot(ageBySex,
        names.arg = paste(head(age.breaks, -1), tail(age.breaks, -1), sep = '-'),
        beside = TRUE,
        col = c(rgb(1, 0, 1, .8), rgb(0, 0, 1, .8)),
        cex.axis = .65, cex.names = .65,
        ylim = c(0, .45))

# home, febrile & denv+
ageBySex = rbind(
  hist(as.numeric(
    partHome$age[partHome$sex == 'F' & partHome$pathogen == 'DEN'][
      !is.null(partHome$age[partHome$sex == 'F' & partHome$pathogen == 'DEN'])]),
    age.breaks, plot = FALSE)$density,
  hist(as.numeric(
    partHome$age[partHome$sex == 'M' & partHome$pathogen == 'DEN'][
      !is.null(partHome$age[partHome$sex == 'M' & partHome$pathogen == 'DEN'])]),
    age.breaks, plot = FALSE)$density)
ageBySex = ageBySex / rowSums(ageBySex)
barplot(ageBySex,
        names.arg = paste(head(age.breaks, -1), tail(age.breaks, -1), sep = '-'),
        beside = TRUE,
        col = c(rgb(1, 0, 1, .8), rgb(0, 0, 1, .8)),
        cex.axis = .65, cex.names = .65,
        ylim = c(0, .45))
mtext('Febrile, DENV+', 3, 2)

# away, febrile & denv+
ageBySex = rbind(
  hist(as.numeric(
    participants.notHome[,'age'][participants.notHome[,'sex'] == 'F' & participants.notHome[,'pathogen'] == 'DEN'][
      !is.null(participants.notHome[,'age'][participants.notHome[,'sex'] == 'F' & participants.notHome[,'pathogen'] == 'DEN'])]),
    age.breaks, plot = FALSE)$density,
  hist(as.numeric(
    participants.notHome[,'age'][participants.notHome[,'sex'] == 'M' & participants.notHome[,'pathogen'] == 'DEN'][
      !is.null(participants.notHome[,'age'][participants.notHome[,'sex'] == 'M' & participants.notHome[,'pathogen'] == 'DEN'])]),
    age.breaks, plot = FALSE)$density)
ageBySex = ageBySex / rowSums(ageBySex)
barplot(ageBySex,
        names.arg = paste(head(age.breaks, -1), tail(age.breaks, -1), sep = '-'),
        beside = TRUE,
        col = c(rgb(1, 0, 1, .8), rgb(0, 0, 1, .8)),
        cex.axis = .65, cex.names = .65,
        ylim = c(0, .45))
mtext('Age', 1, 3)

# home, febrile & denv-
ageBySex = rbind(
  hist(as.numeric(
    partHome$age[partHome$sex == 'F' & partHome$pathogen == 'DEN'][
      !is.null(partHome$age[partHome$sex == 'F' & partHome$pathogen == 'DEN'])]),
    age.breaks, plot = FALSE)$density,
  hist(as.numeric(
    partHome$age[partHome$sex == 'M' & partHome$pathogen == 'DEN'][
      !is.null(partHome$age[partHome$sex == 'M' & partHome$pathogen == 'DEN'])]),
    age.breaks, plot = FALSE)$density)
ageBySex = ageBySex / rowSums(ageBySex)
barplot(ageBySex,
        names.arg = paste(head(age.breaks, -1), tail(age.breaks, -1), sep = '-'),
        beside = TRUE,
        col = c(rgb(1, 0, 1, .8), rgb(0, 0, 1, .8)),
        cex.axis = .65, cex.names = .65,
        ylim = c(0, .45))
mtext('Home', 4, 2)
mtext('Febrile, DENV-', 3, 2)

# away, febrile & denv-
ageBySex = rbind(
  hist(as.numeric(
    participants.notHome[,'age'][participants.notHome[,'sex'] == 'F' & participants.notHome[,'pathogen'] == 'FEV'][
      !is.null(participants.notHome[,'age'][participants.notHome[,'sex'] == 'F' & participants.notHome[,'pathogen'] == 'FEV'])]),
    age.breaks, plot = FALSE)$density,
  hist(as.numeric(
    participants.notHome[,'age'][participants.notHome[,'sex'] == 'M' & participants.notHome[,'pathogen'] == 'FEV'][
      !is.null(participants.notHome[,'age'][participants.notHome[,'sex'] == 'M' & participants.notHome[,'pathogen'] == 'FEV'])]),
    age.breaks, plot = FALSE)$density)
ageBySex = ageBySex / rowSums(ageBySex)
barplot(ageBySex,
        names.arg = paste(head(age.breaks, -1), tail(age.breaks, -1), sep = '-'),
        beside = TRUE,
        col = c(rgb(1, 0, 1, .8), rgb(0, 0, 1, .8)),
        cex.axis = .65, cex.names = .65,
        ylim = c(0, .45))
mtext('Away', 4, 2)

dev.off()
