### exporta gráfico estático ou interativo dependendo se documento é .html ou .pdf

try(namespace::registerNamespace('kn',loadNamespace('knitr')), silent=TRUE)

leitura_grafico <- function(ggplot2_grafico){
    
    if (kn::is_html_output()) {
        
        ggplot2_grafico_plotly <-
            readRDS(file = paste0(
                endereco_graficos_interativos,
                substitute(ggplot2_grafico),
                "_plotly.rds"
            ))
        ggplot2_grafico_plotly
        
    } else {
        
        ggplot2_grafico_n_plotly <-
            readRDS(file = paste0(endereco_graficos_estaticos,
                                        substitute(ggplot2_grafico),".rds"))
        ggplot2_grafico_n_plotly
    }
    
}