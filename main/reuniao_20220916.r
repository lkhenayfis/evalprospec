pkg_files <- list.files("R", full.names = TRUE)
for(pf in pkg_files) source(pf)

source("informals/dados_verif_hugo.r")

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

selec_cens <- rank_cenarios(dd_sin, rf_earm_gter_sin, c(.1, .75), jitter = 1)

pp <- plot(dd_sin, highlight = selec_cens) + scale_y_continuous(limits = c(25, 90))
ggsave("out/earp_sin_RF_EARM_GTER_SIN_jitter1.jpeg", pp, width = 16, height = 9)
pp <- plot(dd_ree, highlight = selec_cens) + scale_y_continuous(limits = c(10, 100))
ggsave("out/earp_ree_RF_EARM_GTER_SIN_jitter1.jpeg", pp, width = 16, height = 9)


selec_cens <- rank_cenarios(dd_sin, rf_earm_gter_sin, c(.1, .75), jitter = c(-2, 0, 2))

pp <- plot(dd_sin, highlight = selec_cens) + scale_y_continuous(limits = c(25, 90))
ggsave("out/earp_sin_RF_EARM_GTER_SIN_jitter2.jpeg", pp, width = 16, height = 9)
pp <- plot(dd_ree, highlight = selec_cens) + scale_y_continuous(limits = c(10, 100))
ggsave("out/earp_ree_RF_EARM_GTER_SIN_jitter2.jpeg", pp, width = 16, height = 9)

# ESCOLHA POR METRICA DA CANDIDA -----------------------------------------------
# ADICIONANDO O VERIFICADO

selec_cens <- rank_cenarios(dd_sin, rf_earm_gter_sin, c(.1, .75), jitter = 1)

pp <- plot(dd_sin, highlight = selec_cens) + scale_y_continuous(limits = c(25, 90)) +
    geom_line(data = verif_sin, aes(data2, Verificado), color = 1)
ggsave("out/earp_sin_RF_EARM_GTER_SIN_jitter1_com_verif.jpeg", pp, width = 16, height = 9)
pp <- plot(dd_ree, highlight = selec_cens) + scale_y_continuous(limits = c(10, 100)) +
    geom_line(data = verif_ree, aes(data2, Verificado), color = 1)
ggsave("out/earp_ree_RF_EARM_GTER_SIN_jitter1_com_verif.jpeg", pp, width = 16, height = 9)


selec_cens <- rank_cenarios(dd_sin, rf_earm_gter_sin, c(.1, .75), jitter = c(-2, 0, 2))

pp <- plot(dd_sin, highlight = selec_cens) + scale_y_continuous(limits = c(25, 90)) +
    geom_line(data = verif_sin, aes(data2, Verificado), color = 1)
ggsave("out/earp_sin_RF_EARM_GTER_SIN_jitter2_com_verif.jpeg", pp, width = 16, height = 9)
pp <- plot(dd_ree, highlight = selec_cens) + scale_y_continuous(limits = c(10, 100)) +
    geom_line(data = verif_ree, aes(data2, Verificado), color = 1)
ggsave("out/earp_ree_RF_EARM_GTER_SIN_jitter2_com_verif.jpeg", pp, width = 16, height = 9)

# METRICA ENA ------------------------------------------------------------------

selec_cens <- rank_cenarios(dd_sin, rf_ena_sin, c(.1, .75), jitter = 1)

pp <- plot(dd_sin, highlight = selec_cens) + scale_y_continuous(limits = c(25, 90)) +
    geom_line(data = verif_sin, aes(data2, Verificado), color = 1)
ggsave("out/earp_sin_RF_ENA_SIN_jitter1_com_verif.jpeg", pp, width = 16, height = 9)
pp <- plot(dd_ree, highlight = selec_cens) + scale_y_continuous(limits = c(10, 100)) +
    geom_line(data = verif_ree, aes(data2, Verificado), color = 1)
ggsave("out/earp_ree_RF_ENA_SIN_jitter1_com_verif.jpeg", pp, width = 16, height = 9)


selec_cens <- rank_cenarios(dd_sin, rf_ena_sin, c(.1, .75), jitter = c(-2, 0, 2))

pp <- plot(dd_sin, highlight = selec_cens) + scale_y_continuous(limits = c(25, 90)) +
    geom_line(data = verif_sin, aes(data2, Verificado), color = 1)
ggsave("out/earp_sin_RF_ENA_SIN_jitter2_com_verif.jpeg", pp, width = 16, height = 9)
pp <- plot(dd_ree, highlight = selec_cens) + scale_y_continuous(limits = c(10, 100)) +
    geom_line(data = verif_ree, aes(data2, Verificado), color = 1)
ggsave("out/earp_ree_RF_ENA_SIN_jitter2_com_verif.jpeg", pp, width = 16, height = 9)

# ENA x EARM -------------------------------------------------------------------

dplot <- dcast(dd_ree$simul, pmo + REE + cenario + data ~ variavel, value.var = "valor")

ggplot(dplot[data %between% c("2022-03-01", "2022-07-01")],
    aes(ENA, EARP, color = REE)) + geom_point() +
    facet_grid(data ~ REE, scales = "free_x") +
    theme_bw()
ggsave("out/scatter.jpeg", width = 16, height = 9)
