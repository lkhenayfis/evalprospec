############################## FUNCOES UTILITARIAS PARA OUTRAS PARTES ##############################

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