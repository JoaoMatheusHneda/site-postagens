library(magrittr)
library(highcharter)

highchart() %>% 
  # add the module
  hc_add_dependency("modules/pattern-fill.js") %>% 
  hc_size(heigh = 350) %>% 
  hc_xAxis(type = 'category') %>% 
  hc_tooltip(
    borderColor = "#CACACA",
    pointFormat = 'The height for <b>{point.name}</b> is <b>{point.y}</b>'
    ) %>% 
  # hc_chart(
  #   plotBackgroundColor = list(
  #     linearGradient = c(0, 0, 0, 500),
  #     stops = list(
  #       list(0.0, 'rgb(240, 178, 79)'),
  #       list(0.5, 'rgb(202, 108, 70)'),
  #       list(0.9, 'rgb(12, 5, 36)')
  #       )
  #     )
  # ) %>% 
  # hc_xAxis(gridLineColor = 'transparent') %>% 
  # hc_yAxis(gridLineColor = 'transparent') %>% 
  hc_add_series(
    type = "column",
    showInLegend = FALSE,
    pointWidth = 110,
    pointPadding = 0.25,
    borderColor = "transparent",
    borderWidth = 0,
    data = list(
      list(
        name = "Petronas",
        y = 100,
        color = list(
          pattern = list(
             image = 'https://www.svgrepo.com/show/27082/petronas-towers.svg',
             aspectRatio = 1.3
          )
        )
      ),
      list(
        name = 'Pisa',
        y = 150,
        color = list(
          pattern = list(
            image = 'https://www.svgrepo.com/show/1171/tower-of-pisa.svg',
            aspectRatio = 1
          )
        )
      ),
      list(
        name = 'Eiffel tower',
        y = 200,
        color = list(
          pattern = list(
            image = 'https://www.svgrepo.com/show/19456/tokyo-tower.svg',
            aspectRatio = 0.8
            )
          )
      ),
      list(
        name = 'Ahu-tongariki',
        y = 250,
        color = list(
          pattern = list(
            image = 'https://www.svgrepo.com/show/27081/ahu-tongariki.svg',
            aspectRatio = 0.75
          )
        )
      )
    )
  )