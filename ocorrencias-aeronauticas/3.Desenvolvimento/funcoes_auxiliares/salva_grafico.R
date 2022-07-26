salva_grafico <- function(ggplot2_grafico, ...){
    
    ### gráfico estático
    ggplot2_grafico_n_plotly <- saveRDS(ggplot2_grafico,
                                           file = paste0(
                                               endereco_graficos_estaticos,
                                               substitute(ggplot2_grafico),
                                               ".rds"
                                           ))
    
    ### gráfico interativo
    source('funcoes_auxiliares/arruma_grafico_plotly.R', encoding = "UTF-8")
    ggplot2_grafico_plotly <- arruma_grafico_plotly(ggplot2_grafico,...)
    saveRDS(
        ggplot2_grafico_plotly,
        file = paste0(
            endereco_graficos_interativos,
            substitute(ggplot2_grafico),
            "_plotly.rds"
        )
    )
}
