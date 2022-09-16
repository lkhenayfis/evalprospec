################################ FUNCOES PARA VISUALIZACAO DE DADOS ################################

library(ggplot2)

plot.dtsimulREE <- function(x, vars = "EARP", rees = c("PARANA", "SUL", "NORDESTE"), pmos,
    highlight, ...) {

    if(missing("pmos")) pmos <- attr(x, "pmos")
    dplot <- x$simul[(variavel %in% vars) & (REE %in% rees) & (pmo %in% pmos)]
    dplot[, quadro := paste0(REE, " --- ", variavel, " --- ", pmo)]

    gg <- ggplot() +
        geom_line(data = dplot, aes(data, valor, group = cenario), color = "grey80") +
        labs(x = "Data", y = "Valor") +
        facet_wrap(~ quadro, scales = "free_y", ncol = length(vars) * length(rees), dir = "v") +
        theme_bw()

    if(!missing("highlight")) {
        highcens <- montahighlight(highlight)
        dplot2   <- merge(dplot, highcens)

        gg <- gg +
            geom_line(data = dplot2, aes(data, valor, group = cenario,
                linetype = jitter, color = quantil), lwd = 1) +
            scale_color_discrete(name = "Quantil") +
            scale_linetype_discrete(name = "Offset")
    }

    return(gg)
}

plot.dtsimulSIN <- function(x, vars = "EARP", pmos, highlight, ...) {

    if(missing("pmos")) pmos <- attr(x, "pmos")
    dplot <- x$simul[(variavel %in% vars) & (pmo %in% pmos)]
    dplot[, quadro := paste0(variavel, " --- ", pmo)]

    gg <- ggplot() +
        geom_line(data = dplot, aes(data, valor, group = cenario), color = "grey80") +
        labs(x = "Data", y = "Valor") +
        facet_wrap(~ quadro, scales = "free_y", ncol = length(vars), dir = "v") +
        theme_bw()

    if(!missing("highlight")) {
        highcens <- montahighlight(highlight)
        dplot2   <- merge(dplot, highcens)

        gg <- gg +
            geom_line(data = dplot2, aes(data, valor, group = cenario,
                linetype = jitter, color = quantil), lwd = 1) +
            scale_color_discrete(name = "Quantil") +
            scale_linetype_discrete(name = "Offset")
    }

    return(gg)
}

# HELPERS ------------------------------------------------------------------------------------------

montahighlight <- function(cens) {
    cens <- lapply(names(cens), function(pmo) {
        lpmo <- cens[[pmo]]
        lout <- lapply(names(lpmo), function(j) {
            data.table(
                pmo = pmo,
                cenario = unname(lpmo[[j]]),
                quantil = names(lpmo[[j]]),
                jitter = j
            )
        })
        rbindlist(lout)
    })
    cens <- rbindlist(cens)

    lvs <- cens[, as.numeric(sub("j?", "", unique(jitter)))]
    lbs <- formatC(lvs, format = "d", width = 2, flag = "+")
    lbs <- sub("\\+0", "0", lbs)
    ord <- order(abs(lvs))
    cens[, jitter := factor(jitter, levels = unique(jitter)[ord], labels = lbs[ord])]
    cens

    return(cens)
}