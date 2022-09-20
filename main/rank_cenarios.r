
parseconf <- function(CONF) {
    CONF$RANKING <- lapply(CONF$RANKING, function(ll) {
        ll <- c(list(quote(rank_cenarios)), ll)
        ll$rankfun <- str2lang(ll$rankfun)
        as.call(ll)
    })

    if(CONF$PMO == "") {
        pmos <- list.dirs(CONF$ROOT_SIMUL, recursive = FALSE)
        pmos <- pmos[grep("pmo_[[:digit:]]{6}", pmos)]
        pmos <- sapply(strsplit(pmos, "/"), tail, 1)
        CONF$PMO <- tail(pmos, 1)
    }

    if(CONF$OUT_DIR == "") CONF$OUT_DIR <- CONF$ROOT_SIMUL
    CONF$DIRSIMUL <- file.path(CONF$ROOT_SIMUL, CONF$PMO)
    CONF$OUT_DIR  <- file.path(CONF$OUT_DIR,    CONF$PMO)

    if(CONF$MELHORPIOR == "") CONF$MELHORPIOR <- dir.exists(file.path(CONF$DIRSIMUL, "melhor"))
    if(CONF$MELHORPIOR) {
        CONF$DIRSIMUL <- file.path(CONF$DIRSIMUL, c("pior", "melhor"))
        CONF$OUT_DIR <- file.path(CONF$OUT_DIR, c("pior", "melhor"))

        names(CONF$DIRSIMUL) <- c("pior", "melhor")
        names(CONF$OUT_DIR) <- c("pior", "melhor")
    } else {
        names(CONF$DIRSIMUL) <- "default"
    }

    if(CONF$SINTESE == "") CONF$SINTESE <- dir.exists(file.path(CONF$DIRSIMUL[1], "sintese"))
    if(CONF$SINTESE) CONF$DIRSIMUL <- file.path(CONF$DIRSIMUL, "sintese")

    CONF$OUT_DIR <- structure(file.path(CONF$OUT_DIR, "evalsimul_out"), names = names(CONF$OUT_DIR))

    return(CONF)
}

main <- function(arq_conf, activate = TRUE) {

    # RENV ---------------------------------------------------------------------

    # identificacao do root para ativacao do ambiente virtual. Isso e necessario pois o renv nao
    # pode ser ativado "por fora" como em python
    # INSTALLDIR e uma variavel de ambiente definida dentro do script de uso do programa
    root <- Sys.getenv("INSTALLDIR", getwd())

    if(activate) {
        wd0 <- getwd()
        setwd(root)
        arq <- list.files("renv", "activate", full.names = TRUE)
        source(arq)
        setwd(wd0)
    }

    # SOURCE -------------------------------------------------------------------

    sourcefiles <- list.files(file.path(root, "R"), full.names = TRUE)
    for(sf in sourcefiles) source(sf)

    # CONFIG -------------------------------------------------------------------

    if(missing("arq_conf")) {
        arq_conf <- commandArgs(trailingOnly = TRUE)
        arq_conf <- arq_conf[grep("jsonc?$", arq_conf)]
    }
    if(length(arq_conf) == 0) arq_conf <- file.path(root, "conf", "default", "rank_cenarios_default.jsonc")

    CONF <- jsonlite::read_json(arq_conf, TRUE)
    CONF <- parseconf(CONF)

    for(outdir in CONF$OUT_DIR) if(!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

    # EXECUTA RANKEAMENTO ------------------------------------------------------

    for(tiposimul in names(CONF$DIRSIMUL)) {

        dirsimul <- CONF$DIRSIMUL[tiposimul]
        outdir   <- CONF$OUT_DIR[tiposimul]
        ranking  <- CONF$RANKING[[tiposimul]]

        tipodados <- ranking$rankfun
        tipodados <- eval(as.call(list(tipodados, ask = TRUE)))

        datsimul <- le_diretorio(dirsimul, tipodados, pmo = CONF$PMO)

        # Rankeamento completo ----------------------------

        ranking_full         <- ranking
        ranking_full$dat     <- datsimul
        ranking_full$quantis <- NA
        ranking_full <- eval(ranking_full)
        ranking_full <- cbind(ranking_full[[1]], ecmwf = tiposimul)
        fwrite(ranking_full, file.path(outdir, "ranking_full.csv"))

        # Selecao de quantis ------------------------------

        ranking$dat <- datsimul
        ranking <- eval(ranking)
        cor <- switch(tiposimul, "default" = 1, "melhor" = 4, "pior" = 2)
        gg <- plot(datsimul, c("EARP", "ENA", "GTER"), highlight = ranking) +
            scale_color_manual(values = cor)
        ggsave(file.path(outdir, "selec_quant.jpeg"), gg, width = 12, height = 9)
    }
}