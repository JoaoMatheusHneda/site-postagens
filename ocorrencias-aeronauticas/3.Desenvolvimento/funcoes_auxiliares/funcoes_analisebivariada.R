#------------------------------------------------------------------------------------------------------
try(library(magrittr, include.only = c("%>%")), silent=TRUE)
try(namespace::registerNamespace('dp', loadNamespace('dplyr')), silent=TRUE)
try(namespace::registerNamespace('fc', loadNamespace('forcats')), silent=TRUE)
try(namespace::registerNamespace('gg',loadNamespace('ggplot2')), silent=TRUE)

# Variável Qualitativa X Variável Qualitativa
calcula_tab_frequencia_bivariada <- function(tabela_de_entrada,nome_variavel1,nome_variavel2){
    tabela_freq <- as.data.frame(table(tabela_de_entrada[[nome_variavel1]],tabela_de_entrada[[nome_variavel2]]))
    names(tabela_freq) <- c(nome_variavel1,nome_variavel2,'Frequency')
    
    Sumgroup <- tapply(tabela_freq[['Frequency']],
                       tabela_freq[[nome_variavel2]],
                       FUN=sum)
    Sumgroup <- tibble::enframe(Sumgroup)
    colnames(Sumgroup) <- c(nome_variavel2,'sum_x')
    Sumgroup[['cumsum_sum_x']] <- cumsum(Sumgroup[['sum_x']]) 
    Sumgroup[['percentage_sum_x']] <- round((Sumgroup[['sum_x']]/sum(Sumgroup[['sum_x']]))*100,1)
    
    tabela_freq <- left_join(x=tabela_freq,
                             y=Sumgroup,
                             by=nome_variavel2)
    tabela_freq[[nome_variavel2]] <- factor(tabela_freq[[nome_variavel2]],
                                               levels=levels(tabela_de_entrada[[nome_variavel2]]))
    tabela_freq[['Frequency_div_sum_x']] <- tabela_freq[['Frequency']]/tabela_freq[['sum_x']]
    tabela_freq[['percentage_Frequency_div_sum_x']] <- round((tabela_freq[['Frequency']]/tabela_freq[['sum_x']])*100,1)
    
    # assign(x=paste0('table_count_prop',nome_variavel1,'_',nome_variavel2),value=tabela_freq,envir=.GlobalEnv)
    # 
    # return( 
    #     list(
    #         paste0('Nome da tabela abaixo: table_count_prop',nome_variavel1,'_',nome_variavel2), 
    #         tabela_freq 
    #     )
    # )
    
    return(tabela_freq)
}


calcula_tab_frequencia_bivariada_dplyr <- function(tabela_de_entrada,nome_variavel1,nome_variavel2){
    tabela_freq <- tabela_de_entrada %>% 
        dp::select({{nome_variavel1}},{{nome_variavel2}}) %>% 
        dp::group_by({{nome_variavel1}}, {{nome_variavel2}}) %>% 
        dp::summarise(`Frequency` = dplyr::n()) %>% 
        as.data.frame()
    
    tabela_freq_aux <- (
        dp::group_by(tabela_freq, {{nome_variavel1}}) %>%
            dp::summarise(sum_Frequency = sum(`Frequency`)) %>% 
            as.data.frame()
    )
    
    tabela_saida <- dp::left_join(tabela_freq, tabela_freq_aux, by = as.character(substitute(nome_variavel1))) %>% 
        dp::arrange({{nome_variavel1}}) %>%
        dp::mutate(`Percentage`=(`Frequency`/`sum_Frequency`)*100) %>% 
        as.data.frame()
    
    tabela_saida[,"Frequency"] <- as.numeric(tabela_saida[,"Frequency"])
    tabela_saida[,"sum_Frequency"] <- as.numeric(tabela_saida[,"sum_Frequency"])
    
    return(tabela_saida)

}


grafico_tab_frequencia_bivariada2 <- function(table_count_prop,nome_variavel1,nome_variavel2,tx1=0.8,tx2=1.15,size=4,legend=FALSE){
    if(size != 0){
        print('Para retirar as porcentagens, use size = 0!')
    } else if(size == 0){
        print('Para adicionar as porcentagens, use size > 0!')
    }
    
    gg_count_prop <- gg::ggplot(gg::aes_string(x=nome_variavel2,y='Frequency',fill=nome_variavel1),
                           data=table_count_prop) +
        gg::theme_classic() +
        gg::geom_col(position="dodge") #+
    if(legend==TRUE){
        gg_count_prop <- gg_count_prop +
            gg::geom_text(gg::aes(label=paste0(Percentage,'%'),
                          y=Frequency+tx1*mean(Frequency)),
                      position = gg::position_dodge(width = 0.9),
                      size=size) + 
            gg::expand_limits(y=max(table_count_prop['Frequency'])*tx2)#+
        # coord_cartesian(xlim=c(table_count_prop[[nome_variavel2]][1],
        #                       table_count_prop[[nome_variavel2]][15]))
        # coord_flip(xlim=c(rev(table_count_prop[[nome_variavel2]])[15],
        #                   rev(table_count_prop[[nome_variavel2]])[1]))
    }
    
    # assign(x=paste0('gg_count_prop',nome_variavel1,'_',nome_variavel2),value=gg_count_prop,envir=.GlobalEnv)
    # return( 
    #     list(paste0('Nome do gráfico: gg_count_prop',nome_variavel1,'_',nome_variavel2))#, 
    #          #gg_count_prop 
    #     )
    
    return(gg_count_prop)
}





grafico_tab_frequencia_bivariada3 <- function(table_count_prop,nome_variavel1,nome_variavel2,size=2,tx1=0.025,tx2=0.00025,legend=FALSE){
    table_count_prop <- table_count_prop %>% 
        dp::mutate(`PercentageFormat` = paste0(`Percentage`,'%'))
    
    
    gg_count_prop <- gg::ggplot(gg::aes_string(x=nome_variavel2,
                                       y='Frequency',
                                       fill=nome_variavel1,
                                       label="PercentageFormat"),
                            data=table_count_prop) +
        gg::theme_classic() +
        gg::geom_col(position='fill') +
        gg::theme(legend.position = "bottom") +
        gg::expand_limits(y=max(table_count_prop['Frequency'])*tx2)
    if(legend==TRUE){
        gg_count_prop <- gg_count_prop +
            gg::geom_text(gg::aes(label=paste0(Percentage,'%'),
                              y=Frequency+tx1*Frequency
                              ),
                          position = 'fill',
                          size=size)
    }
    # assign(x=paste0('gg_count_prop',nome_variavel1,'_',nome_variavel2),value=gg_count_prop,envir=.GlobalEnv)
    # return( 
    #     list(paste0('Nome do gráfico: gg_count_prop',nome_variavel1,'_',nome_variavel2))#, 
    #     #gg_count_prop 
    # )
        
    return(gg_count_prop)
}




