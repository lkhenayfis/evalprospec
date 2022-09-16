pkg_files <- list.files("R", full.names = TRUE)
for(pf in pkg_files) source(pf)

dirbase <- "../pmos/simul_CFS"
nomebase <- tail(strsplit(dirbase, "/")[[1]], 1)

dir.create(file.path("out", nomebase))

dirs <- list.dirs(dirbase, recursive = FALSE)
dirs <- dirs[grep("pmo_[[:digit:]]", dirs)]

dd_sin <- lapply(dirs, le_diretorio, tipo = "sin")
dd_sin <- do.call(rbind, dd_sin)

dd_ree <- lapply(dirs, le_diretorio, tipo = "ree")
dd_ree <- do.call(rbind, dd_ree)

# ESCOLHA POR METRICA DA CANDIDA -----------------------------------------------

selec_cens <- rank_cenarios(dd_sin, rf_earm_gter_sin, c(.25, .5, .9), jitter = 1)

pp <- plot(dd_sin, highlight = selec_cens)
ggsave("out/earp_sin_RF_EARM_GTER_SIN_jitter1.jpeg", pp, width = 16, height = 12)
pp <- plot(dd_ree, highlight = selec_cens)
ggsave("out/earp_ree_RF_EARM_GTER_SIN_jitter1.jpeg", pp, width = 16, height = 12)


selec_cens <- rank_cenarios(dd_sin, rf_earm_gter_sin, c(.25, .5, .9), jitter = c(-2, 0, 2))

pp <- plot(dd_sin, highlight = selec_cens)
ggsave("out/earp_sin_RF_EARM_GTER_SIN_jitter2.jpeg", pp, width = 16, height = 12)
pp <- plot(dd_ree, highlight = selec_cens)
ggsave("out/earp_ree_RF_EARM_GTER_SIN_jitter2.jpeg", pp, width = 16, height = 12)

