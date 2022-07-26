
#------------------------------------------------------------------------------------------------------
try(library(magrittr, include.only = c("%>%")), silent=TRUE)
try(namespace::registerNamespace('dp', loadNamespace('dplyr')), silent=TRUE)
try(namespace::registerNamespace('fc', loadNamespace('forcats')), silent=TRUE)
try(namespace::registerNamespace('gg',loadNamespace('ggplot2')), silent=TRUE)

# Variável Qualitativa
calcula_tab_frequencia <- function(tabela_de_entrada,nome_variavel,decreasing=FALSE){
    tabela_freq <- data.frame(table(tabela_de_entrada[nome_variavel]))
    names(tabela_freq) <- c('Category','Frequency')
    tabela_freq['Percentage'] <- prop.table(table(tabela_de_entrada[nome_variavel]))*100
    tabela_freq <- tabela_freq[order(tabela_freq[['Frequency']],decreasing=decreasing),]
    tabela_freq['Cumulative.Frequency'] <- cumsum(tabela_freq['Frequency'])
    tabela_freq['Cumulative.Percentage'] <- cumsum(tabela_freq['Percentage'])
    tabela_freq['Percentage'] <- round(tabela_freq['Percentage'],2)
    tabela_freq['Cumulative.Percentage'] <- round(tabela_freq['Cumulative.Percentage'],2)
    
    
    
    # assign(x=paste0('table_count_prop',nome_variavel),value=tabela_freq,envir=.GlobalEnv)
    # 
    # return( 
    #     list(
    #         paste0('Nome da tabela abaixo: table_count_prop',nome_variavel), 
    #         tabela_freq 
    #         )
    #     )
    
    return(tabela_freq)
}

calcula_tab_frequencia_dplyr <- function(tabela_de_entrada,nome_variavel,
                                         specific_order = NULL,decreasing = NULL){

    
    tabela_saida <- tabela_de_entrada %>% 
        dp::select({{nome_variavel}}) %>% 
        dp::group_by({{nome_variavel}}) %>%
        dp::summarise(`Frequency` = dplyr::n()) %>%
        dp::rename(`Category` = {{nome_variavel}}) %>% 
        as.data.frame()
    
        if((!is.null(specific_order)) && (!is.null(decreasing)) ) {
            stop("Duas condições de ordenação foram determinadas. Revise o código!")
            
            
        } else {
            
            if (!is.null(specific_order)) {
                tabela_saida <- tabela_saida %>%
                    dp::arrange(match(`Category`, specific_order))
            }
            
            if ((!is.null(decreasing)) && (decreasing == TRUE)) {
                tabela_saida <-
                    tabela_saida %>% dp::arrange(dp::desc(`Frequency`))
            } else if ((!is.null(decreasing)) &&
                       (decreasing == FALSE)) {
                tabela_saida <- tabela_saida %>% dp::arrange(`Frequency`)
            }
            
            tabela_saida <- tabela_saida %>%
                dp::mutate(`Percentage` = `Frequency` / sum(`Frequency`) * 100,
                          `Cumulative.Frequency` = cumsum(`Frequency`),
                          `Cumulative.Percentage` = cumsum(`Percentage`)) %>%
                as.data.frame()
            
            
            tabela_saida[,"Frequency"] <- as.numeric(tabela_saida[,"Frequency"])
            tabela_saida[,"Cumulative.Frequency"] <- as.numeric(tabela_saida[,"Cumulative.Frequency"])
            # if(!is.numeric(tabela_saida[,"Category"])){
            #     tabela_saida[,"Category"] <- fc$fct_inorder(tabela_saida[,"Category"])
            # }
            

        }
        
        
    return(tabela_saida)
}
    

## Frequency and Percentage
grafico_tab_frequencia <- function(table_count_prop,nome_variavel,cor_grafico="#77C6D9",
                                   hjust_e=-0.17,hjust_d=0.15,size=4.5,tx1=0.05,tx2=1.05,
                                   xlab,ylab){
    
    gg_count_prop <- gg::ggplot(gg::aes(x=Category,y=Frequency),
              data=table_count_prop) +
        gg::theme_classic() + gg::geom_col(fill=cor_grafico) +
        gg::geom_col(fill=cor_grafico) +
        gg::geom_text(gg::aes(label=Frequency,
                            y=Frequency+tx1*mean(Frequency)),
                     position = gg::position_nudge(x=hjust_e,y=0.9),
                     size=size) +
        gg::geom_text(gg::aes(label=paste0('(',round(Percentage,2),'%)'),
                            y=Frequency+tx1*mean(Frequency)),
                     position = gg::position_nudge(x=hjust_d,y=0.9),
                     size=size) +
        gg::expand_limits(y=max(table_count_prop['Frequency'])*tx2) +
        gg::xlab(xlab) +
        gg::ylab(ylab)
    
    return(gg_count_prop)
        
}


## Only Percentage
grafico_tab_frequencia2 <- function(table_count_prop,nome_variavel,cor_grafico,size,tx1=0.06,tx2=1.07){
    gg_count_prop <-  gg::ggplot(gg::aes(x=Category,y=Frequency),data=table_count_prop) +  gg::theme_classic() + 
        gg::geom_col(fill=cor_grafico) +
        gg::geom_text(gg::aes(label=paste0(Percentage,'%'),y=Frequency+tx1*mean(Frequency)),
                                                                    position = gg::position_nudge(y=0.9),
                                                                    size=size) +
        gg::expand_limits(y=max(table_count_prop['Frequency'])*tx2)+
        gg::coord_flip()

    # assign(x=paste0('gg_count_prop',nome_variavel),value=gg_count_prop,envir=.GlobalEnv)
    # return( 
    #     list(paste0('Nome do gráfico: gg_count_prop',nome_variavel)#, 
    #         #gg_count_prop 
    #         )
    #     )

    return(gg_count_prop)
}

## Frequency and Percentage (with geom_label) e axis with angle
grafico_tab_frequencia3 <- function(table_count_prop,nome_variavel,cor_grafico,size=12,angle=45){
    gg_count_prop <- ggplot2::ggplot(aes(x=Category,y=Frequency),data=table_count_prop) +  ggplot2::theme_classic() + 
                                                        ggplot2::geom_col(fill=cor_grafico) + 
                                                        ggplot2::geom_text(aes(label=Frequency),
                                                                position = position_dodge(0.9),
                                                                vjust=-0.1,
                                                                size=size) +
                                                        ggplot2::geom_label(aes(label=paste0(Percentage,'%')),
                                                                position = position_dodge(0.9),
                                                                vjust=1.2,
                                                                size=size) +
                                                        theme(axis.text.x = element_text(angle = angle, hjust=1)) +
                                                        expand_limits(y=max(table_count_prop['Frequency'])*1.05)
    return(gg_count_prop)
}

#------------------------------------------------------------------------------------------------------
# Variável Quantitativa

apply_binning_by_limites_intervalos <- function(limites_intervalos,dados_entrada,nome_variavel){
        dados_saida <- cut(x=dados_entrada[[nome_variavel]],
                            breaks=limites_intervalos,
                            include.lowest=TRUE)
        k <- length(levels(dados_saida))
        dados_saida <- addNA(dados_saida)
        lista <- list(k=k,limites_intervalos=limites_intervalos,dados_saida=dados_saida)
        return(lista)
}

apply_binning_by_k <- function(k,dados_entrada,nome_variavel){
        dados_saida <- cut(x=dados_entrada[[nome_variavel]],breaks=k,include.lowest=TRUE)
        levels_saida <- levels(dados_saida)
        limites_intervalos <- c(-Inf,
                                as.numeric( sub("\\((.+),.*", "\\1", levels_saida[-1]) ),
                                Inf)
                                
        levels(dados_saida)[1] <- gsub('.*,','(-Inf,',levels(dados_saida)[1])
        levels(dados_saida)[length(levels(dados_saida))] <- gsub(',.*',',Inf),',levels(dados_saida)[length(levels(dados_saida))])

        lista <- list(k=k,limites_intervalos=limites_intervalos,dados_saida=dados_saida) 
        return(lista)
}

binning_quantities <- function(dados_entrada,nome_variavel,method='Percentis'){

    if (method == 'Percentis'){
        percentis <- seq(0.1,0.9,by=.1)
        limites_intervalos <- unname(c(-Inf,
                                    quantile(dados_entrada[[nome_variavel]],probs=percentis,na.rm=TRUE),
                                    Inf))

        apply_binning_by_limites_intervalos(limites_intervalos,dados_entrada,nome_variavel)
    }
    else if(method == 'Sturges'){
        k <- nclass.Sturges(na.omit(dados_entrada[[nome_variavel]]))

        apply_binning_by_k(k,dados_entrada,nome_variavel)

    }
    else if (method == 'Scott') {
        k <- nclass.scott(na.omit(dados_entrada[[nome_variavel]]))

        apply_binning_by_k(k,dados_entrada,nome_variavel)

    }
    else if (method == 'Friedman-Diaconis') {
       k <- nclass.FD(na.omit(dados_entrada[[nome_variavel]]))
       
       apply_binning_by_k(k,dados_entrada,nome_variavel)
    }

    else if (method == 'RegraPratica') {
        if(length(na.omit(dados_entrada[[nome_variavel]])) <= 25){
            k <- 5
        }
        else {
            k <- sqrt(length(na.omit(dados_entrada[[nome_variavel]])))
        }

        apply_binning_by_k(k,dados_entrada,nome_variavel)

    }

    else {
        print('This method does not exist!')
    }

}

