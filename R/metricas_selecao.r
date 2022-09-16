############################### FUNCOES PARA RANKEAMENTO DE CENARIOS ###############################

rank_cenarios <- function(dat, rankfun = rf_earm_sin, quantis = c(.25, .5, .9), jitter = 0, ...) {

    if(length(jitter) == 1) jitter <- seq(-jitter, jitter)
    qnames <- paste0(formatC(quantis * 100, format = "f", digits = 2), "%")

    dat  <- dat$simul
    ldat <- split(dat, dat$pmo)
    rank <- lapply(ldat, rankfun, ...)
    rank <- lapply(rank, function(r) r[, prob := seq_len(.N) / .N])
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

rf_ena_sin <- function(d, ...) {
    d <- d[variavel == "ENA", sum(valor), by = cenario]
    setorder(d, V1)
    return(d)
}

rf_earm_sin <- function(d, ...) {
    d <- d[variavel == "EARM", tail(valor, 1), by = cenario]
    setorder(d, V1)
    return(d)
}

rf_earm_gter_sin <- function(d, pesos = c(0, 0, 0, 0, 0, 1), ...) {
    d <- dcast(d, pmo + cenario + data ~ variavel, value.var = "valor")
    d <- d[, tail(EARM, 1) - sum(GTER), by = cenario]
    setorder(d, V1)
    return(d)
}

rf_earm_parana <- function(d, pesos = c(0, 0, 0, 0, 0, 1), ...) {
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