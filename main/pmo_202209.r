pkg_files <- list.files("R", full.names = TRUE)
for(pf in pkg_files) source(pf)

# MELHOR -----------------------------------------------------------------------

dirbase <- "../pmos/simul_CFS/pmo_202209/melhor"
nomebase <- sub(".*pmos/", "", dirbase)
dir.create(file.path("out", nomebase), recursive = TRUE)

dd_melhor <- le_diretorio(dirbase, "sin", pmo = "pmo_202209")

rank_melhor_full <- rank_cenarios(dd_sin, rf_earm_gter_sin, NA)
rank_melhor_full <- rank_melhor_full$pmo_202209
rank_melhor_full[, ecmwf := "melhor"]
fwrite(rank_melhor_full, file.path("out", nomebase, "rank_melhor.csv"))

# PIOR -----------------------------------------------------------------------

dirbase <- "../pmos/simul_CFS/pmo_202209/pior"
nomebase <- sub(".*pmos/", "", dirbase)
dir.create(file.path("out", nomebase), recursive = TRUE)

dd_pior <- le_diretorio(dirbase, "sin", pmo = "pmo_202209")
dd_pior$simul[, cenario := cenario + attr(dd_pior, "ncen")]

rank_pior_full <- rank_cenarios(dd_pior, rf_earm_gter_sin, NA)
rank_pior_full <- rank_pior_full$pmo_202209
rank_pior_full[, ecmwf := "pior"]
fwrite(rank_pior_full, file.path("out", nomebase, "rank_pior.csv"))

# PLOTS ------------------------------------------------------------------------

rank_melhor <- rank_cenarios(dd_melhor, rf_earm_gter_sin, .75, 3)
rank_pior   <- rank_cenarios(dd_pior,   rf_earm_gter_sin, .1,  3)

rankk <- list(pmo_202209 = list(j0 = c("10.00%" = 111, "75.00%" = 6)))

gg <- plot(rbind(dd_pior, dd_melhor), highlight = rankk)
ggsave("out/simul_CFS/pmo_202209/selected.jpeg", gg, width = 9, height = 6)

##############################

dplot <- rbind(
    cbind(dd_melhor$simul, ecmwf = "melhor"),
    cbind(dd_pior$simul, ecmwf = "pior")
)

ggplot() +
    geom_line(data = dplot[variavel == "EARP"], aes(data, valor, group = cenario, color = ecmwf), alpha = .5)
