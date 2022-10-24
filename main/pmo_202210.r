pkg_files <- list.files("R", full.names = TRUE)
for(pf in pkg_files) source(pf)

# Caminhos relevantes
root_prelim <- "../pmos/simul_CFS/pmo_202210/preliminar"
dir_prelim_pior   <- file.path(root_prelim, "pior")
dir_prelim_melhor <- file.path(root_prelim, "melhor")

root_definitivo <- "../pmos/simul_CFS/pmo_202210/definitivo"
dir_definitivo_pior   <- file.path(root_definitivo, "pior")
dir_definitivo_melhor <- file.path(root_definitivo, "melhor")

# PRELIMINAR ---------------------------------------------------------------------------------------

# MELHOR -----------------------------------------------------------------------

nomebase <- sub(".*pmos/", "", dir_prelim_melhor)
dir.create(file.path("out", nomebase), recursive = TRUE)

dd_prelim_melhor <- le_diretorio(dir_prelim_melhor, "sin", pmo = "pmo_202210")

rank_prelim_melhor_full <- rank_cenarios(dd_prelim_melhor, rf_earm_gter_sin, NA)
rank_prelim_melhor_full <- rank_prelim_melhor_full$pmo_202210
rank_prelim_melhor_full[, ecmwf := "melhor"]
fwrite(rank_prelim_melhor_full, file.path("out", nomebase, "rank_melhor.csv"))

# PIOR -------------------------------------------------------------------------

nomebase <- sub(".*pmos/", "", dir_prelim_pior)
dir.create(file.path("out", nomebase), recursive = TRUE)

dd_prelim_pior <- le_diretorio(dir_prelim_melhor, "sin", pmo = "pmo_202210")
dd_prelim_pior$simul[, cenario := cenario + attr(dd_prelim_pior, "ncen")]

rank_prelim_pior_full <- rank_cenarios(dd_prelim_pior, rf_earm_gter_sin, NA)
rank_prelim_pior_full <- rank_prelim_pior_full$pmo_202210
rank_prelim_pior_full[, ecmwf := "pior"]
fwrite(rank_prelim_pior_full, file.path("out", nomebase, "rank_prelim_pior.csv"))

# PLOTS ------------------------------------------------------------------------

ddplot_prelim <- rbind(dd_prelim_melhor, dd_prelim_pior)

# O cenario selecionado do pior deve ser o indice dele dentro de dd_pior + nrow(dd_melhor)
selec_cens_prelim <- list(pmo_202210 = list(j0 = c("10.00%" = 111, "75.00%" = 6)))
gg <- plot(ddplot_prelim, highlight = selec_cens_prelim, vars = "EARMF")

outarq <- file.path(sub(".*pmos", "out", root_prelim), "selected.jpeg")
ggsave(outarq, gg, width = 9, height = 6)

# DEFINITIVO ---------------------------------------------------------------------------------------

# MELHOR -----------------------------------------------------------------------

nomebase <- sub(".*pmos/", "", dir_definitivo_melhor)
dir.create(file.path("out", nomebase), recursive = TRUE)

dd_definitivo_melhor <- le_diretorio(dir_definitivo_melhor, "sin", pmo = "pmo_202210")

rank_definitivo_melhor_full <- rank_cenarios(dd_definitivo_melhor, rf_earm_gter_sin, NA)
rank_definitivo_melhor_full <- rank_definitivo_melhor_full$pmo_202210
rank_definitivo_melhor_full[, ecmwf := "melhor"]
fwrite(rank_definitivo_melhor_full, file.path("out", nomebase, "rank_definitivo_melhor.csv"))

# PIOR -------------------------------------------------------------------------

nomebase <- sub(".*pmos/", "", dir_definitivo_pior)
dir.create(file.path("out", nomebase), recursive = TRUE)

dd_definitivo_pior <- le_diretorio(dir_definitivo_pior, "sin", pmo = "pmo_202210")
dd_definitivo_pior$simul[, cenario := cenario + attr(dd_definitivo_pior, "ncen")]

rank_definitivo_pior_full <- rank_cenarios(dd_definitivo_pior, rf_earm_gter_sin, NA)
rank_definitivo_pior_full <- rank_definitivo_pior_full$pmo_202210
rank_definitivo_pior_full[, ecmwf := "pior"]
fwrite(rank_definitivo_pior_full, file.path("out", nomebase, "rank_definitivo_pior.csv"))

# PLOTS ------------------------------------------------------------------------

ddplot_definitivo <- rbind(dd_definitivo_pior, dd_definitivo_melhor)

# O cenario selecionado do pior deve ser o indice dele dentro de dd_pior + nrow(dd_melhor)
selec_cens_definitivo <- list(pmo_202210 = list(j0 = c("10.00%" = 107, "75.00%" = 47)))

aux_prelim <- ddplot_prelim$simul[
    (cenario %in% unname(selec_cens_prelim[[1]][[1]])) & (variavel == "EARMF")]
aux_prelim[, quantil := rep(c("75.00%", "10.00%"), each = .N / 2)]
aux_prelim[, jitter := "j1"]

gg <- plot(ddplot_definitivo, highlight = selec_cens_definitivo, vars = "EARMF") +
    geom_line(data = aux_prelim, aes(data, valor, group = cenario, linetype = jitter, color = quantil), lwd = 1) +
    scale_linetype_discrete(name = "Seleção", labels = c("Definitivo", "Preliminar")) +
    scale_x_date(date_labels = "%b/%Y", date_breaks = "1 month")

outarq <- file.path(sub(".*pmos", "out", root_prelim), "selected.jpeg")
ggsave(outarq, gg, width = 9, height = 6)