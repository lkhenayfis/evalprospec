############################### FUNCOES PARA RANKEAMENTO DE CENARIOS ###############################

#' Rankeamento De Cenarios
#' 
#' Realiza o rankeamento de N cenarios segundo uma metrica arbitraria \code{rankfun}
#' 
#' \code{rank_cenarios} separa o data.table de resultados da simulacao, que pode conter varios pmos, 
#' em uma lista de data.tables contendo um unico pmo cada. As funcoes de rankeamento \code{rankfun}
#' devem receber estes data.tables individuais e retorna-los ordenados por criticidade, com o mais
#' critico na primeira linha e o menos critico na ultima. Por exemplo, para a metrica EARM_SIN_FIM,
#' a primeira linha corresponde ao cenario com menor EARM e a ultima ao com maior.
#' 
#' @param dat objeto do tipo \code{dtsimul} contendo resultados da simulacao dos cenarios a rankear
#' @param rankfun funcao que recebe data.table de resultados da simulacao para um unico pmo e retorna
#'     o mesmo data.table ordenado em ordem crescente da metrica. Ver Detalhes
#' @param quantis opcional, vetor de quantis a selecionar do rankeamento. Se \code{NULL}, retorna o 
#'     rankeamento completo
#' @param jitter um inteiro indicando quantos cenarios acima e abaixo daqueles associados aos 
#'     quantis devem ser retornados, para analise de sensibilidade. Default 0
#' 
#' @return se \code{quantis != NULL}, uma lista de dois niveis: primeiro nivel indicando o pmo, 
#'     segundo nivel os leveis de jitter. Se \code{quantis == NULL}, uma lista de um nivel contendo
#'     o rankeamento completo para cada pmo

rank_cenarios <- function(dat, rankfun = rf_earm_sin, quantis = c(.25, .5, .9), jitter = 0, ...) {

    if(length(jitter) == 1) jitter <- seq(-jitter, jitter)
    qnames <- paste0(formatC(quantis * 100, format = "f", digits = 2), "%")

    dat  <- dat$simul
    ldat <- split(dat, dat$pmo)
    rank <- lapply(ldat, rankfun, ...)
    rank <- lapply(rank, function(r) r[, prob := seq_len(.N) / .N])

    if(is.null(quantis)) return(rank)

    rank <- lapply(rank, function(r) {
        inds <- valmaisprox(quantis, r$prob)
        inds <- lapply(jitter, function(j) inds + j)
        names(inds) <- paste0("j", jitter)
        out <- lapply(inds, function(i) structure(r[i, cenario], names = qnames))
        out
    })

    return(rank)
}

# FUNCOES DE RANK ----------------------------------------------------------------------------------

rf_ena_sin <- function(d, ask = FALSE, ...) {
    if(ask) return("sin")

    d <- d[variavel == "ENA", sum(valor), by = cenario]
    setorder(d, V1)
    return(d)
}

rf_earm_sin <- function(d, ask = FALSE, ...) {
    if(ask) return("sin")

    d <- d[variavel == "EARM", tail(valor, 1), by = cenario]
    setorder(d, V1)
    return(d)
}

rf_earm_gter_sin <- function(d, pesos = c(0, 0, 0, 0, 0, 1), ask = FALSE, ...) {
    if(ask) return("sin")

    d <- dcast(d, pmo + cenario + data ~ variavel, value.var = "valor")
    d <- d[, tail(EARM, 1) - sum(GTER), by = cenario]
    setorder(d, V1)
    return(d)
}

rf_earm_parana <- function(d, pesos = c(0, 0, 0, 0, 0, 1), ask = FALSE, ...) {
    if(ask) return("ree")

    d <- d[(REE == "PARANA") & (variavel == "EARM"), weighted.mean(valor, pesos), by = cenario]
    setorder(d, V1)
    return(d)
}

# HELPERS ------------------------------------------------------------------------------------------

evalsubsets <- function(dat, subsets) {
    subs <- lapply(names(subsets), function(var) dat[[var]] %in% subsets[[var]])
    subs <- Reduce("&", subs)
    out <- dat[subs]
    return(out)
}

valmaisprox <- function(x, v) {
    dist <- sapply(x, function(xi) which.min(abs(v - xi)))
    return(dist)
}