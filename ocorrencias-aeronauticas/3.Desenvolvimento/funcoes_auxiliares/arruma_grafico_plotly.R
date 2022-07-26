arruma_grafico_plotly <- function(ggplot2_grafico, subir_eixo_y = F, x=0,y=1){
    try(library(magrittr, include.only = c("%>%")), silent=TRUE)
    try(namespace::registerNamespace('pl',loadNamespace('plotly')), silent=TRUE)
    try(namespace::registerNamespace('stg', loadNamespace('stringr')), silent=TRUE)
    
    p <- pl::ggplotly(ggplot2_grafico)
    fig <- pl::plotly_build(p)
    
    
    for (i in seq_along(fig$x$data)){
        if(fig$x$data[[i]]$text %>% grepl(pattern="<br") %>% any()){
            
            text <- stg::str_split(fig$x$data[[i]]$text, ":")
            position <- text %>%
                lapply(FUN=function(x) c(grep(x,pattern="<br"),length(x)))
            fig$x$data[[i]]$text <- lapply(text, FUN = function(x) x[position[[1]]]) %>%
                lapply(FUN = function(y) stg::word(y,sep = "<br", 1)) %>%
                lapply(FUN = function(z) trimws(z,which = "both"))
            
            
            text_aux <- fig$x$data[[i]]$text[[1]]
            if(text_aux[length(text_aux)] == text_aux[length(text_aux) - 1]){
                fig$x$data[[i]]$text <- lapply(fig$x$data[[i]]$text,FUN = function(b) b[1:(length(b) - 1)])
            }
            
            fig$x$data[[i]]$text <- fig$x$data[[i]]$text %>% 
                lapply(FUN = function(k) stg::str_c("(",stg::str_c(k,collapse = ","),")")) %>%
                unlist()
        }               
        
        
        if(fig$x$data[[i]]$hovertext %>% grepl(pattern="<br") %>% any()){
            fig$x$data[[i]]$hovertext <- NULL
        }
        
    }
    
    if(subir_eixo_y == T){
        y <- 1.055
        fig <- fig %>%
            pl::layout(legend = list(
                orientation = "h",
                title=list(text=ggplot2_grafico$labels$colour),
                x = x,
                y = y)
            )
    } else {
        fig <- fig %>%
            pl::layout(legend = list(
                orientation = "h",
                title=list(text=ggplot2_grafico$labels$colour),
                x = x,
                y = y)
            )
    }

    
    return(fig)
}

