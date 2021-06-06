library(shiny)
library(sf)
library(tidyverse)
library(scales)

hki <- readRDS("hki_data.RDS") 
k_osat <- readRDS("data_kosat_p.RDS")

stats <- unique(names(hki)[2:ncol(hki)])

ui <- fluidPage(
  
    sidebarPanel(
      selectInput(inputId = "stats",
                    label = "Varanto",
                    choices = stats,
                    multiple = FALSE,
                    selected = "karayht"),
        htmlOutput(outputId = "text"),
      width = 3),
    
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", 
                 plotOutput("plot", height = 800)
                 )
      ),
      width = 9
    )
)

server <- function(input, output, session) {
  
  hkiData <- reactive({
    req(input$stats)
    hki %>% 
      dplyr::select(tunnus, input$stats)
  })
  
  
  output$plot <- renderPlot({
    
    g <- function(df, col) {
      df %>% 
        dplyr::select(tunnus, all_of(col)) %>%
        dplyr::group_by(tunnus) %>%
        dplyr::mutate(this_sum = sum(!! sym(col))) %>%
        dplyr::select(-!!col) %>%
        dplyr::distinct_at(vars(tunnus), .keep_all = TRUE)
    }
    
    res <- g(hkiData(), input$stats)
    
    merged <- sf::st_as_sf(merge(k_osat, res))
    
    ggplot(merged) +
      geom_sf(aes(fill = this_sum)) +
      geom_sf_label(data = sf::st_point_on_surface(merged), aes(label = nimi_fi), 
                    size = 2.5) +
      scale_fill_viridis_c(label = comma, option = "inferno") +      
      guides(fill = guide_legend(title = paste0(input$stats, " (m2)"))) +
      theme_void() 
    
  })
  
  output$text <- renderUI({
    
    HTML("<ul>
<li>kala = <br>rakennusoikeus, kuntarekisteriin merkitty kerrosala.<br>Ei sisällä kaavamääräyksiin mahdollisesti <br>sisältyvää lisärakennusoikeutta.</li>
<li>karayht = <br>käyttöönotettu kerrosala yhteensä <br>(sisältää rakenteilla olevan kerrosalan)</li>
<li>karaas = <br>käyttöönotettu asuinkerrosala <br>(sisältää rakenteilla olevan kerrosalan)</li>
<li>karamu = <br>käyttöönotettu muu kuin asuinkerrosala <br>(sisältää rakenteilla olevan kerrosalan)</li>
<li>laskvar_ak = <br>laskennallinen kerrostalovaranto</li>
<li>laskvar_ap = <br>laskennallinen pientalovaranto</li>
<li>laskvar_k = <br>laskennallinen liike- ja toimistotilavaranto</li>
<li>laskvar_t = <br>laskennallinen teollisuus- ja varastotilavaranto</li>
<li>laskvar_y = <br>laskennallinen julkisen rakentamisen varanto</li>
<li>laskvar_nn = <br>laskennallinen muu kuin yllä mainittujen käyttötarkoitusten <br>varanto</li>
<li>laskvar_yh = <br>laskennallinen varanto yhteensä</li>
<li>rakeraas = <br>rakenteilla oleva asuinkerrosala</li>
<li>rakeramu = <br>rakenteilla oleva muu kuin asuinkerrosala</li>
<li>rakerayht = <br>rakenteilla oleva kerrosala yhteensä </li>
</ul>")
  })
  
  
}


shinyApp(ui = ui, server = server)
