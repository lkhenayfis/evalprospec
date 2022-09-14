############################## CLASSE DE DADOS RETORNADOS DA SIMULACAO #############################

#' Construtor Interno De \code{dtsimul} E Subclasses
#' 
#' Funcao interna para estruturacao das saidas de simulacao
#' 
#' @param dat um data.table com as colunas 
#' @param subcls string da subclasse. Um de \code{c("SIN", "REE", "MERC")}

new_dtsimul <- function(dat, subcls = NULL) {

    subcls <- toupper(subcls)
    veccls <- c(paste0("dtsimul", subcls), "dtsimul")

    pmos   <- unique(dat$pmo)
    ncen   <- max(dat$cenario)
    datas  <- sort(unique(dat$data))
    variaveis <- unique(dat$variavel)

    out <- list(simul = dat)
    class(out) <- unique(veccls)
    attr(out, "pmos")   <- pmos
    attr(out, "ncen")   <- ncen
    attr(out, "datas")  <- datas
    attr(out, "variaveis") <- variaveis

    return(out)
}

# METODOS ------------------------------------------------------------------------------------------

#' \code{rbind} De Objetos \code{dtsimul}
#' 
#' Empilha um numero de objetos da classe \code{dtsimul}
#' 
#' @param ... numero arbitrario de objetos \code{dtsimul}
#' 
#' @return objeto \code{dtsimul} combinando todos os passados em \code{...}

rbind.dtsimul <- function(...) {

    dts <- list(...)
    tipos <- lapply(dts, class)
    tipos <- sub("dtsimul", "", sapply(tipos, "[[", 1))

    if(!all(tipos == tipos[1])) stop("Tentando juntar dados de tipos diferentes")

    out <- lapply(dts, "[[", "simul")
    out <- rbindlist(out)

    new_dtsimul(out, tipos[1])
}
