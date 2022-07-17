arruma_grafico_plotly <- function(ggplot2_grafico, subir_eixo_y = F, x=0,y=1){
    box::use("mg" = magrittr); library(magrittr)
    box::use("pl" = plotly)
    
    p <- pl$ggplotly(ggplot2_grafico)
    fig <- pl$plotly_build(p)
    
    
    for (i in bs$seq_along(fig$x$data)){
        if(fig$x$data[[i]]$text %>% bs$grepl(pattern="<br") %>% bs$any()){
            
            text <- st$str_split(fig$x$data[[i]]$text, ":")
            position <- text %>%
                lapply(FUN=function(x) c(bs$grep(x,pattern="<br"),length(x)))
            fig$x$data[[i]]$text <- lapply(text, FUN = function(x) x[position[[1]]]) %>%
                lapply(FUN = function(y) st$word(y,sep = "<br", 1)) %>%
                lapply(FUN = function(z) bs$trimws(z,which = "both"))
            
            
            text_aux <- fig$x$data[[i]]$text[[1]]
            if(text_aux[length(text_aux)] == text_aux[length(text_aux) - 1]){
                fig$x$data[[i]]$text <- lapply(fig$x$data[[i]]$text,FUN = function(b) b[1:(length(b) - 1)])
            }
            
            fig$x$data[[i]]$text <- fig$x$data[[i]]$text %>% 
                lapply(FUN = function(k) st$str_c("(",st$str_c(k,collapse = ","),")")) %>%
                bs$unlist()
        }               
        
        
        if(fig$x$data[[i]]$hovertext %>% bs$grepl(pattern="<br") %>% bs$any()){
            fig$x$data[[i]]$hovertext <- NULL
        }
        
    }
    
    if(subir_eixo_y == T){
        y <- 1.055
        fig <- fig %>%
            pl$layout(legend = list(
                orientation = "h",
                title=list(text=ggplot2_grafico$labels$colour),
                x = x,
                y = y)
            )
    } else {
        fig <- fig %>%
            pl$layout(legend = list(
                orientation = "h",
                title=list(text=ggplot2_grafico$labels$colour),
                x = x,
                y = y)
            )
    }

    
    return(fig)
}

