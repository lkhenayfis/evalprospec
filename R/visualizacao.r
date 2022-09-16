################################ FUNCOES PARA VISUALIZACAO DE DADOS ################################

plot.dtsimulREE <- function(x, vars = "EARP", rees = c("PARANA", "SUL", "NORDESTE"), pmos,
    highlight, ...) {

    if(missing("pmos")) pmos <- attr(x, "pmos")
    dplot <- x$simul[(variavel %in% vars) & (REE %in% rees) & (pmo %in% pmos)]

    gg <- ggplot() +
        geom_line(data = dplot, aes(data, valor, group = cenario), color = "grey80") +
        facet_grid(pmo ~ REE + variavel, scales = "free") +
        theme_bw()

    if(!missing("highlight")) {
        highcens <- montahighlight(highlight)
        dplot2   <- merge(dplot, highcens)

        gg + geom_line(data = dplot2, aes(data, valor, group = cenario, color = quantil), lwd = 1) +
            scale_color_viridis_d()
    }

    return(gg)
}

plot.dtsimulSIN <- function(x, vars = "EARP", pmos, highlight, ...) {

    if(missing("pmos")) pmos <- attr(x, "pmos")
    dplot <- x$simul[(variavel %in% vars) & (pmo %in% pmos)]

    gg <- ggplot() +
        geom_line(data = dplot, aes(data, valor, group = cenario), color = "grey80") +
        facet_grid(pmo ~ variavel, scales = "free") +
        theme_bw()

    if(!missing("highlight")) {
        highcens <- montahighlight(highlight)
        dplot2   <- merge(dplot, highcens)

        gg + geom_line(data = dplot2, aes(data, valor, group = cenario, color = quantil), lwd = 1) +
            scale_color_viridis_d()
    }

    return(gg)
}

# HELPERS ------------------------------------------------------------------------------------------

montahighlight <- function(cens) {
    cens <- lapply(names(cens), function(pmo) {
        data.table(pmo = pmo, cenario = unname(cens[[pmo]]), quantil = names(cens[[pmo]]))
    })
    cens <- rbindlist(cens)
    return(cens)
}