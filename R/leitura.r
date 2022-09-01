################################## FUNCOES PARA LEITURA DAS SAIDAS #################################


le_arquivoREE <- function(path) {
    dat <- fread(path)
    ncens <- ncol(dat) - 2
    colnames(dat) <- c("data", paste0("cen", seq_len(ncens)), "REE")
    dat <- dat[-1, ]
    return(dat)
}

le_arquivoSIN <- function(path) {
    dat <- fread(path)
    ncens <- ncol(dat) - 1
    colnames(dat) <- c("data", paste0("cen", seq_len(ncens)))
    dat <- dat[-1, ]
    return(dat)
}

guess_var <- function(arq) {
    guess <- sub(".*/", "", arq)
    guess <- strsplit(guess, "_")[[1]][1]
    return(guess)
}

guess_pmo <- function(arq) {
    guess <- strsplit(arq, "/")[[1]]
    guess <- tail(guess, 2)[1]
    return(guess)
}