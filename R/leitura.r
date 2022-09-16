################################## FUNCOES PARA LEITURA DAS SAIDAS #################################

library(data.table)

# LEITURA DE ARQUIVOS INDIVIDUAIS ------------------------------------------------------------------

#' Leitores De Arquivos
#' 
#' Funcoes auxiliares para leitura dos arquivos de saida e formatacao padronizada
#' 
#' \code{variavel} e \code{pmo} podem receber valores string diretamente ou NA. Neste caso, a funcao
#' tenta adivinhar com base no nome do arquivo de qual variavel se trata. 
#' 
#' \code{variavel} e advinhada buscando uma sequencia de caracteres seguida de "_", que e o formato
#' padrao pelo qual os arquivos sao escritos. Caso sua taxonomia seja diferente, valores espurios
#' podem aparecer.
#' 
#' \code{pmo} e adivinhada olhando o nome do diretorio onde \code{arq} se encontra. Tipicamente os 
#' resultados sao salvos em diretorios nomeados por pmo. Em estruturas de arquivo diferentes, 
#' valores espurios podem aparecer.
#' 
#' @param arq caminho do arquivo a ser lido
#' @param variavel opcional, a variavel contida no arquivo. Ver Detalhes
#' @param pmo opcional, o pmo de referencia (string, numerico etc). Ver Detalhes
#' 
#' @return data.table padronizado com as colunas
#' \describe{
#' \item{pmo}{pmo da simulacao}
#' \item{REE}{REE do valor simulado}
#' \item{cenario}{indice do cenario simulado}
#' \item{data}{data associada ao valor simulado}
#' \item{variavel}{variavel simulada}
#' \item{valor}{valor simulado}
#' }

le_arquivoREE <- function(arq, variavel = NA_character_, pmo = NA_character_) {

    dat   <- fread(arq)
    ncens <- ncol(dat) - 2
    colnames(dat) <- c("REE", "data", paste0("cen", seq_len(ncens)))

    dat <- melt(dat, id.vars = c("data", "REE"), variable.name = "cenario", value.name = "valor")
    dat[, cenario := as.numeric(sub("[[:alpha:]]*", "", cenario))]

    if(is.na(variavel)) variavel <- guess_var(arq)
    if(is.na(pmo)) pmo <- guess_pmo(arq)
    dat[, c("variavel", "pmo") := list(variavel, pmo)]

    setcolorder(dat, c("pmo", "REE", "cenario", "data", "variavel", "valor"))

    new_dtsimul(dat, "REE")
}

le_arquivoSIN <- function(arq, variavel = NA_character_, pmo = NA_character_) {

    dat <- fread(arq)
    ncens <- ncol(dat) - 1
    colnames(dat) <- c("data", paste0("cen", seq_len(ncens)))

    dat <- melt(dat, id.vars = c("data"), variable.name = "cenario", value.name = "valor")
    dat[, cenario := as.numeric(sub("[[:alpha:]]*", "", cenario))]

    if(is.na(variavel)) variavel <- guess_var(arq)
    if(is.na(pmo)) pmo <- guess_pmo(arq)
    dat[, c("variavel", "pmo") := list(variavel, pmo)]

    setcolorder(dat, c("pmo", "cenario", "data", "variavel", "valor"))

    new_dtsimul(dat, "SIN")
}

# LEITURA COMPLETA DE ARQUIVOS DE UM DIRETORIO -----------------------------------------------------

#' Leitura De Diretorios
#' 
#' Le e organiza num unico data.table todos os arquivos de simulacao num diretorio
#' 
#' \code{pmo} e adivinhada olhando o nome do diretorio onde \code{arq} se encontra. Tipicamente os 
#' resultados sao salvos em diretorios nomeados por pmo. Em estruturas de arquivo diferentes, 
#' valores espurios podem aparecer.
#' 
#' @param dir diretorio onde se encontram as saidas de simulacao
#' @param tipos os tipos de agregacao dos arquivo a serem lidos. Um vetor se strings cujos elementos
#'     estao entre \code{c("ree", "sin")}
#' @param pmo opcional, o pmo de referencia (string, numerico etc). Ver Detalhes

le_diretorio <- function(dir, tipos = c("ree", "sin"), pmo = NA_character_) {

    arqs <- list.files(dir, full.names = TRUE)
    arqs <- lapply(tipos, function(tipo) arqs[grep(paste0("_", toupper(tipo), "_"), arqs)])

    dados <- mapply(tipos, arqs, FUN = function(tipo, vec) {
        leitor <- selec_leitor(tipo)
        dat <- lapply(vec, leitor)
        dat <- do.call(rbind, dat)

        return(dat)
    }, SIMPLIFY = FALSE)

    if(length(tipos) == 1) dados <- dados[[1]]

    return(dados)
}
