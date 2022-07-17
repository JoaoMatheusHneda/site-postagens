
endereco_relatorio <- "../4.Relatorio/"
endereco_apresentacao <- "../5.Apresentacao/"
endereco_imagens <- "imagens/"

#----------------------------------------------------
# relatório pdf
rmarkdown::render(
    "1.desenvolvimento_relatorios.Rmd", params = list(
    ),
    output_file = paste0(endereco_relatorio,"pdf/","index",".pdf"),
    output_format = 'pdf_document'
)

#----------------------------------------------------
# relatório html
rmarkdown::render(
    "1.desenvolvimento_relatorios.Rmd", params = list(
    ),
    output_file = paste0(endereco_relatorio,"html/","index",".html"),
    output_format = 'html_document'
)

#----------------------------------------------------
# apresentação

fs::dir_copy(endereco_imagens, paste0(endereco_apresentacao,endereco_imagens),overwrite = T)
fs::file_copy("xaringan-themer.css", paste0(endereco_apresentacao,"xaringan-themer.css"),overwrite = T)

rmarkdown::render(
    "2.desenvolvimento_apresentacao.Rmd", params = list(
    ),
    output_file = paste0(endereco_apresentacao,"index",".html"),
    output_format = 'xaringan::moon_reader'
)




