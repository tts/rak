library(shiny)
library(sf)
library(tidyverse)
library(scales)
library(DT)


hki <- readRDS("hki_data.RDS") 
k_osat <- readRDS("data_kosat_p.RDS")

stats <- unique(names(hki)[2:ncol(hki)])

ui <- fluidPage(
  
  title = "Helsingin tonttivaranto summattuna kaupunginosittain",
  
  titlePanel("Helsingin tonttivaranto summattuna kaupunginosittain"),

  sidebarPanel(
    selectInput(inputId = "stats",
                  label = "Varanto",
                  choices = stats,
                  multiple = FALSE,
                  selected = "kala"),
    htmlOutput(outputId = "text"),
    tags$div(class="form-group shiny-input-container", 
             HTML("<p>Data ja selitykset: <a href='https://hri.fi/data/fi/dataset/paakaupunkiseudun-tonttivaranto-kortteleittain-seuturamava'>HRI</a>. Haettu 2021-06-06</p>
                  <p>Applikaatio: Tuija Sonkkila @ttso</p>
                  <p><a href='https://github.com/tts/rak'>Koodi</a></p>")),
    width = 3),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Kartta", plotOutput("plot", height = 800)),
      tabPanel("Data", DT::dataTableOutput("table", height = 800))
      ),
      width = 9
    )
)

server <- function(input, output, session) {
  
  hkiData <- reactive({
    hki %>% 
      dplyr::select(tunnus, input$stats)
  })
  
  toPlot <- reactive({
  
    g <- function(df, col) {
      df %>% 
        dplyr::select(tunnus, all_of(col)) %>%
        dplyr::group_by(tunnus) %>%
        dplyr::mutate(this_sum = sum(!! sym(col))) %>%
        dplyr::select(-!!col) %>%
        dplyr::distinct_at(vars(tunnus), .keep_all = TRUE)
    }
    
    res <- g(hkiData(), input$stats)
    merged <- merge(k_osat, res)

  })
  
  output$plot <- renderPlot({
    
    ggplot() +
      geom_sf(data = k_osat, fill = "white") +
      geom_sf(data = toPlot(), aes(fill = this_sum)) +
      geom_sf_label(data = sf::st_point_on_surface(k_osat), aes(label = nimi_fi), 
                    size = 3.5, position = position_jitter(width = 1, height = 1)) +
      scale_fill_viridis_c(label = comma, option = "inferno") +   
      guides(fill = guide_legend(title = paste0(input$stats, " (m2)"))) +
      theme_void() 
    
  })
  
  output$text <- renderUI({
    
    HTML("<ul>
<li><b>kala</b> = <br>rakennusoikeus, kuntarekisteriin merkitty kerrosala.<br>Ei sisällä kaavamääräyksiin mahdollisesti <br>sisältyvää lisärakennusoikeutta.</li>
<li><b>karayht</b> = <br>käyttöönotettu kerrosala yhteensä <br>(sisältää rakenteilla olevan kerrosalan)</li>
<li><b>karaas</b> = <br>käyttöönotettu asuinkerrosala <br>(sisältää rakenteilla olevan kerrosalan)</li>
<li><b>karamu</b> = <br>käyttöönotettu muu kuin asuinkerrosala <br>(sisältää rakenteilla olevan kerrosalan)</li>
<li><b>laskvar_ak</b> = <br>laskennallinen kerrostalovaranto</li>
<li><b>laskvar_ap</b> = <br>laskennallinen pientalovaranto</li>
<li><b>laskvar_k</b> = <br>laskennallinen liike- ja toimistotilavaranto</li>
<li><b>laskvar_t</b> = <br>laskennallinen teollisuus- ja varastotilavaranto</li>
<li><b>laskvar_y</b> = <br>laskennallinen julkisen rakentamisen varanto</li>
<li><b>laskvar_nn</b> = <br>laskennallinen muu kuin yllä mainittujen käyttötarkoitusten <br>varanto</li>
<li><b>laskvar_yh</b> = <br>laskennallinen varanto yhteensä</li>
<li><b>rakeraas</b> = <br>rakenteilla oleva asuinkerrosala</li>
<li><b>rakeramu</b> = <br>rakenteilla oleva muu kuin asuinkerrosala</li>
<li><b>rakerayht</b> = <br>rakenteilla oleva kerrosala yhteensä </li>
</ul>")
  })
  
  
  output$table <- DT::renderDataTable({
    
    totable <- toPlot() %>% 
      sf::st_drop_geometry() %>% 
      dplyr::rename(varanto_m2 = this_sum) %>% 
      dplyr::arrange(desc(varanto_m2)) %>% 
      dplyr::select(-gml_id, -id, -aluejako, -kunta, -starts_with("tunnus"), -starts_with("yht"), -paivitetty_tietopalveluun)
    
    dat <- DT::datatable(totable, 
                         rownames = FALSE,
                         filter = "top",
                         options(list(pageLength = 100)))

  })
  
  
}


shinyApp(ui = ui, server = server)
