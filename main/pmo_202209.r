pkg_files <- list.files("R", full.names = TRUE)
for(pf in pkg_files) source(pf)

# PRELIMINAR ---------------------------------------------------------------------------------------

# MELHOR -----------------------------------------------------------------------

dirbase <- "../pmos/simul_CFS/pmo_202209/preliminar/melhor"
nomebase <- sub(".*pmos/", "", dirbase)
dir.create(file.path("out", nomebase), recursive = TRUE)

dd_melhor <- le_diretorio(dirbase, "sin", pmo = "pmo_202209")

rank_melhor_full <- rank_cenarios(dd_melhor, rf_earm_gter_sin, NA)
rank_melhor_full <- rank_melhor_full$pmo_202209
rank_melhor_full[, ecmwf := "melhor"]
fwrite(rank_melhor_full, file.path("out", nomebase, "rank_melhor.csv"))

# PIOR -----------------------------------------------------------------------

dirbase <- "../pmos/simul_CFS/pmo_202209/preliminar/pior"
nomebase <- sub(".*pmos/", "", dirbase)
dir.create(file.path("out", nomebase), recursive = TRUE)

dd_pior <- le_diretorio(dirbase, "sin", pmo = "pmo_202209")
dd_pior$simul[, cenario := cenario + attr(dd_pior, "ncen")]

rank_pior_full <- rank_cenarios(dd_pior, rf_earm_gter_sin, NA)
rank_pior_full <- rank_pior_full$pmo_202209
rank_pior_full[, ecmwf := "pior"]
fwrite(rank_pior_full, file.path("out", nomebase, "rank_pior.csv"))

# PLOTS ------------------------------------------------------------------------

dd_prelim <- rbind(dd_melhor, dd_pior)

rank_melhor <- rank_cenarios(dd_melhor, rf_earm_gter_sin, .75, 3)
rank_pior   <- rank_cenarios(dd_pior,   rf_earm_gter_sin, .1,  3)

rankk <- list(pmo_202209 = list(j0 = c("10.00%" = 111, "75.00%" = 6)))

gg <- plot(rbind(dd_pior, dd_melhor), highlight = rankk)
ggsave("out/simul_CFS/pmo_202209/preliminar/selected.jpeg", gg, width = 9, height = 6)

##############################

rankk2 <- list(pmo_202209 = list(
    j0 = c("10.00%" = 111, "75.00%" = 6),
    j1 = c("10.00%" = 89, "75.00%" = 8)
))
gg2 <- plot(rbind(dd_pior, dd_melhor), highlight = rankk2) +
    scale_linetype_discrete(name = "Métrica", labels = c("EAR_GT", "ENA")) +
    scale_y_continuous(limits = c(0, 100)) +
    scale_x_date(date_labels = "%b/%Y", date_breaks = "1 month")
ggsave("out/simul_CFS/pmo_202209/preliminar/selected_com_ena.jpeg", gg2, width = 9, height = 6)

##############################

dplot <- rbind(
    cbind(dd_melhor$simul, ecmwf = "melhor"),
    cbind(dd_pior$simul, ecmwf = "pior")
)

ggplot() +
    geom_line(data = dplot[variavel == "EARP"], aes(data, valor, group = cenario, color = ecmwf), alpha = .5)

# DEFINITIVO ---------------------------------------------------------------------------------------

# MELHOR -----------------------------------------------------------------------

dirbase <- "../pmos/simul_CFS/pmo_202209/definitivo/melhor"
nomebase <- sub(".*pmos/", "", dirbase)
dir.create(file.path("out", nomebase), recursive = TRUE)

dd_melhor <- le_diretorio(dirbase, "sin", pmo = "pmo_202209")

rank_melhor_full <- rank_cenarios(dd_melhor, rf_earm_gter_sin, NA)
rank_melhor_full <- rank_melhor_full$pmo_202209
rank_melhor_full[, ecmwf := "melhor"]
fwrite(rank_melhor_full, file.path("out", nomebase, "rank_melhor.csv"))

# PIOR -----------------------------------------------------------------------

dirbase <- "../pmos/simul_CFS/pmo_202209/definitivo/pior"
nomebase <- sub(".*pmos/", "", dirbase)
dir.create(file.path("out", nomebase), recursive = TRUE)

dd_pior <- le_diretorio(dirbase, "sin", pmo = "pmo_202209")
dd_pior$simul[, cenario := cenario + attr(dd_pior, "ncen")]

rank_pior_full <- rank_cenarios(dd_pior, rf_earm_gter_sin, NA)
rank_pior_full <- rank_pior_full$pmo_202209
rank_pior_full[, ecmwf := "pior"]
fwrite(rank_pior_full, file.path("out", nomebase, "rank_pior.csv"))

# PLOTS ------------------------------------------------------------------------

rankk <- list(pmo_202209 = list(j0 = c("10.00%" = 107, "75.00%" = 47)))

gg <- plot(rbind(dd_pior, dd_melhor), highlight = rankk) +
    scale_x_date(date_labels = "%b/%Y", date_breaks = "1 month")
ggsave("out/simul_CFS/pmo_202209/selected.jpeg", gg, width = 9, height = 6)

##############################

rankk <- list(pmo_202209 = list(j0 = c("10.00%" = 107, "75.00%" = 47)))

dd_prelim_plot <- dd_prelim$simul[(cenario %in% c(111, 6)) & (variavel == "EARP")]
dd_prelim_plot[, quantil := rep(c("75.00%", "10.00%"), each = .N / 2)]
dd_prelim_plot[, jitter := "j1"]

gg <- plot(rbind(dd_pior, dd_melhor), highlight = rankk) +
    geom_line(data = dd_prelim_plot, aes(data, valor, group = cenario, linetype = jitter, color = quantil), lwd = 1) +
    scale_linetype_discrete(name = "Seleção", labels = c("Definitivo", "Preliminar")) +
    scale_x_date(date_labels = "%b/%Y", date_breaks = "1 month")
ggsave("out/simul_CFS/pmo_202209/selected2.jpeg", gg, width = 9, height = 6)
