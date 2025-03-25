# Bibliotecas
library(shiny)
library(bslib)
library(tidyverse)
library(DBI)
library(RSQLite)
library(highcharter)
library(DT)
library(writexl)
library(leaflet)
library(leaflet.extras)
library(leafgl)
library(sf)

# Locale highcharter
lang <- getOption("highcharter.lang")
lang$decimalPoint <- ","
lang$thousandsSep <- "."
lang$months <- c(
  "Janeiro",
  "Fevereiro",
  "Março",
  "Abril",
  "Maio",
  "Junho",
  "Julho",
  "Agosto",
  "Setembro",
  "Outubro",
  "Novembro",
  "Dezembro"
)
lang$shortMonths <- c(
  "Jan",
  "Fev",
  "Mar",
  "Abr",
  "Mai",
  "Jun",
  "Jul",
  "Ago",
  "Set",
  "Out",
  "Nov",
  "Dez"
)
lang$weekdays <- c(
  "Domingo",
  "Segunda",
  "Terça",
  "Quarta",
  "Quinta",
  "Sexta",
  "Sábado"
)
options(highcharter.lang = lang)
rm(lang)

# Opções gráficos
lista_opcoes_grafico <- c(
  "downloadPNG",
  "downloadJPEG",
  "downloadPDF",
  "downloadSVG"
)

# Conexão com o banco de dados
conn <- dbConnect(SQLite(), "data/db_indicadores_pns.db")

# Indicadores dicotômicos
tb_dicotomicos <- tbl(conn, "tb_dicotomicos") %>%
  collect() %>%
  # Patch para indicadores repetidos
  distinct() %>%
  mutate(
    valor = as.numeric(stringr::str_replace(
      string = valor,
      pattern = ",",
      replacement = "."
    )),
    interv_inf = as.numeric(stringr::str_replace(
      string = interv_inf,
      pattern = ",",
      replacement = "."
    )),
    interv_sup = as.numeric(stringr::str_replace(
      string = interv_sup,
      pattern = ",",
      replacement = "."
    )),
    cv = as.numeric(stringr::str_replace(
      string = cv,
      pattern = ",",
      replacement = "."
    ))
  ) %>%
  mutate(
    abr_tipo = recode(
      abr_tipo,
      "uf" = "Unidades da Federação",
      "região" = "Grandes Regiões",
      "urb_rur" = "Situação urbano/rural",
      "gescol_resp_max" = "Escolaridade do responsável pelo domicílio",
      "rend_per_capita" = "Rendimento domiciliar per capita",
      "sexo" = "Sexo",
      "raça" = "Raça/Cor",
      "fx_idade" = "Faixa de idade",
      "fx_idade_18" = "Faixa de idade (18 anos ou mais)",
      "fx_idade_S" = "Faixa de idade (18 anos ou mais)",
      "fx_idade_s" = "Faixa de idade (18 anos ou mais)",
      "fx_idade_25" = "Faixa de idade (25 anos ou mais)",
      "fx_idade_60" = "Faixa de idade (60 anos ou mais)",
      "fx_idade_acid" = "Faixa de idade (18 anos ou mais)",
      "fx_idade_def" = "Faixa de idade (2 anos ou mais)",
      "fx_idade_4049" = "Faixa de idade (40 a 49 anos)",
      "fx_idade_2564" = "Faixa de idade (25 a 64 anos)",
      "fx_idade_5069" = "Faixa de idade (50 a 69 anos)",
      "capital" = "Capitais",
      "Capital" = "Capitais",
      "gescol" = "Escolaridade",
      "total" = "Total"
    ),
    abr_nome = recode(
      abr_nome,
      "capital" = "Total Capitais",
      "brasil" = "Brasil",
      "Capital" = "Total Capitais",
      "Brasil" = "Brasil",
      "urbano" = "Urbano",
      "rural" = "Rural"
    )
  ) %>%
  filter(!(abr_tipo == "Total" & abr_nome == "Capital"))

# Indicadores tb_politomicos
tb_politomicos <- tbl(conn, "tb_politomicos") %>%
  collect() %>%
  # Patch para indicadores repetidos
  distinct() %>%
  mutate(
    valor = as.numeric(stringr::str_replace(
      string = valor,
      pattern = ",",
      replacement = "."
    )),
    interv_inf = as.numeric(stringr::str_replace(
      string = interv_inf,
      pattern = ",",
      replacement = "."
    )),
    interv_sup = as.numeric(stringr::str_replace(
      string = interv_sup,
      pattern = ",",
      replacement = "."
    )),
    cv = as.numeric(stringr::str_replace(
      string = cv,
      pattern = ",",
      replacement = "."
    ))
  ) %>%
  mutate(
    abr_tipo = recode(
      abr_tipo,
      "uf" = "Unidades da Federação",
      "região" = "Grandes Regiões",
      "urb_rur" = "Situação urbano/rural",
      "gescol_resp_max" = "Escolaridade do responsável pelo domicílio",
      "rend_per_capita" = "Rendimento domiciliar per capita",
      "sexo" = "Sexo",
      "raça" = "Raça/Cor",
      "fx_idade" = "Faixa de idade",
      "fx_idade_18" = "Faixa de idade (18 anos ou mais)",
      "fx_idade_S" = "Faixa de idade (18 anos ou mais)",
      "fx_idade_s" = "Faixa de idade (18 anos ou mais)",
      "fx_idade_25" = "Faixa de idade (25 anos ou mais)",
      "fx_idade_60" = "Faixa de idade (60 anos ou mais)",
      "fx_idade_acid" = "Faixa de idade (18 anos ou mais)",
      "fx_idade_def" = "Faixa de idade (2 anos ou mais)",
      "fx_idade_4049" = "Faixa de idade (40 a 49 anos)",
      "fx_idade_2564" = "Faixa de idade (25 a 64 anos)",
      "fx_idade_5069" = "Faixa de idade (50 a 69 anos)",
      "capital" = "Capitais",
      "Capital" = "Capitais",
      "gescol" = "Escolaridade",
      "total" = "Total"
    ),
    abr_nome = recode(
      abr_nome,
      "capital" = "Total Capitais",
      "brasil" = "Brasil",
      "Capital" = "Total Capitais",
      "Brasil" = "Brasil",
      "urbano" = "Urbano",
      "rural" = "Rural"
    )
  ) %>%
  filter(!(abr_tipo == "Total" & abr_nome == "Capital"))

# Dicionário
dic <- tbl(conn, "tb_dicionario") %>%
  collect() %>%
  mutate(unidade = case_when(unidade == "Proporção" ~ "Percentual")) %>%
  filter(!(cod %in% c("P025P", "P008P")))
# # Patch, aguardando correção no dicionário
# mutate(modulo = if_else(cod == "O011P", "O - Acidentes", modulo))

# Base mapa UFs
uf_shp <- readRDS(file = "data/uf_shp.RDS")


# UI
ui <- navbarPage(
  title = "Selecione o tipo de consulta do painel PNS",

  tabPanel(
    title = "Consulta por abrangência",
    sidebarLayout(
      sidebarPanel(
        width = 12,
        fluidRow(
          column(
            width = 12,
            selectInput(
              inputId = "sel_modulo",
              label = "Módulo",
              choices = c(
                "Q - Doenças Crônicas",
                "N - Percepção do Estado de Saúde",
                "P - Estilos de vida / Alimentação",
                "P - Estilos de vida / Consumo de álcool",
                "P - Estilos de vida / Prática de atividade física",
                "P - Estilos de vida / Tabagismo",
                "W - Antropometria",
                "U - Saúde Bucal",
                "R - Saúde da Mulher",
                "O - Acidentes",
                "V - Violências",
                "I - Cobertura de plano de saúde",
                "J - Utilização de Serviços de Saúde ",
                "K - Saúde dos idosos",
                "G - Deficiências",
                "S - Saúde da Mulher / Pré-natal",
                "S - Saúde da Mulher / Assistência ao parto",
                "B - Visitas domiciliares de Equipe de Saúde da Família e Agentes de Endemias",
                "Y - Atividade Sexual"
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 8,
            uiOutput(outputId = "sel_indicador_UI")
          ),
          column(
            width = 4,
            actionButton(
              inputId = "indi_def",
              label = "Ver definição do indicador",
              icon = icon("info"),
              class = "btn-primary"
            ),
            tags$style(
              type = 'text/css',
              "#indi_def { width:100%; margin-top: 25px;}"
            )
          )
        ),
        fluidRow(
          column(
            width = 2,
            uiOutput(outputId = "sel_unidade_UI")
          ),
          column(
            width = 4,
            uiOutput(outputId = "sel_abr_tipo_UI")
          ),
          column(
            width = 2,
            uiOutput(outputId = "sel_ano_UI")
          ),
          column(
            width = 2,
            uiOutput(outputId = "sel_grafico_UI")
          ),
          column(
            width = 2,
            uiOutput(outputId = "sel_eixo_x_UI")
          )
        )
      ),

      mainPanel(
        width = 12,
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Visualização",
            br(),
            uiOutput(outputId = "grafico_UI"),
            br()
          ),
          tabPanel("Tabela", br(), dataTableOutput("tabela"), br()),
          tabPanel(
            "Download",
            br(),
            downloadButton(
              outputId = "download_csv",
              label = "Formato CSV separado por ponto-e-virgula"
            ),
            br(),
            br(),
            downloadButton(outputId = "download_xlsx", label = "Formato Excel"),
            br()
          )
        )
      )
    )
  ),
  tabPanel(
    title = "Consulta por indicadores",
    sidebarLayout(
      sidebarPanel(
        width = 12,
        fluidRow(
          column(
            width = 12,
            selectInput(
              inputId = "comp_sel_modulo",
              label = "Módulo",
              choices = c(
                "Q - Doenças Crônicas",
                "N - Percepção do Estado de Saúde",
                "P - Estilos de vida / Alimentação",
                "P - Estilos de vida / Consumo de álcool",
                "P - Estilos de vida / Prática de atividade física",
                "P - Estilos de vida / Tabagismo",
                "W - Antropometria",
                "U - Saúde Bucal",
                "R - Saúde da Mulher",
                "O - Acidentes",
                "V - Violências",
                "I - Cobertura de plano de saúde",
                "J - Utilização de Serviços de Saúde ",
                "K - Saúde dos idosos",
                "G - Deficiências",
                "S - Saúde da Mulher / Pré-natal",
                "B - Visitas domiciliares de Equipe de Saúde da Família e Agentes de Endemias",
                "Y - Atividade Sexual"
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 4,
            uiOutput(outputId = "comp_sel_abr_UI")
          ),
          column(
            width = 6,
            uiOutput(outputId = "comp_sel_abr_elemento_UI")
          ),
          column(
            width = 2,
            uiOutput(outputId = "comp_sel_eixo_x_UI")
          )
        )
      ),

      mainPanel(
        width = 12,
        highchartOutput(outputId = "comp_grafico", height = 800)
      )
    )
  )
)

# Server
server <- function(input, output) {
  # Aba Indicadores

  output$sel_indicador_UI <- renderUI({
    req(input$sel_modulo)

    if (input$sel_modulo == "O - Acidentes") {
      ops <- c(
        "Usa sempre cinto quando dirige automóvel",
        "Usa sempre cinto quando anda na frente como passageiro de automóvel",
        "Usa sempre cinto quando anda no banco de trás de automóvel",
        "Usa sempre capacete quando dirige motocicleta",
        "Usa sempre capacete quando está como passageiro de motocicleta",
        "Manuseio de celular durante a condução de veículo",
        "Acidente de trânsito com lesão corporal nos últimos 12 meses"
      )
    } else {
      ops <- dic %>%
        filter(modulo == !!input$sel_modulo) %>%
        select(nome) %>%
        distinct(nome) %>%
        pull(nome)
    }

    selectInput(inputId = "sel_indicador", label = "Indicador", choices = ops)
  })

  output$sel_unidade_UI <- renderUI({
    req(input$sel_indicador)

    ops <- dic %>%
      filter(nome == !!input$sel_indicador) %>%
      select(unidade) %>%
      distinct(unidade) %>%
      pull(unidade)

    selectInput(inputId = "sel_unidade", label = "Unidade", choices = ops)
  })

  tab_indicador <- reactive({
    req(input$sel_indicador)
    req(input$sel_unidade)

    dic %>%
      filter(nome == !!input$sel_indicador) %>%
      filter(unidade == !!input$sel_unidade) %>%
      select(tabela_indicador) %>%
      distinct(tabela_indicador) %>%
      arrange(tabela_indicador) %>%
      pull(tabela_indicador)
  })

  cod_indicador_selecionado <- reactive({
    req(input$sel_indicador)
    req(input$sel_unidade)

    dic %>%
      filter(nome == !!input$sel_indicador) %>%
      filter(unidade == !!input$sel_unidade) %>%
      select(cod) %>%
      distinct(cod) %>%
      arrange(cod) %>%
      pull(cod)
  })

  output$sel_abr_tipo_UI <- renderUI({
    req(cod_indicador_selecionado())

    ops <- get(tab_indicador()) %>%
      filter(indicador == cod_indicador_selecionado()) %>%
      select(abr_tipo) %>%
      distinct(abr_tipo) %>%
      arrange(factor(
        abr_tipo,
        levels = c(
          "Total",
          "Grandes Regiões",
          "Unidades da Federação",
          "Capitais",
          "Situação urbano/rural",
          "Sexo",
          "Faixa de idade",
          "Faixa de idade (18 anos ou mais)",
          "Raça/Cor",
          "Escolaridade",
          "Rendimento domiciliar per capita"
        )
      )) %>%
      pull(abr_tipo)

    selectInput(inputId = "sel_abr_tipo", label = "Abrangência", choices = ops)
  })

  output$sel_ano_UI <- renderUI({
    req(cod_indicador_selecionado())
    req(input$sel_abr_tipo)

    ops <- get(tab_indicador()) %>%
      filter(indicador == cod_indicador_selecionado()) %>%
      filter(abr_tipo == !!input$sel_abr_tipo) %>%
      select(ano) %>%
      distinct(ano) %>%
      arrange(ano) %>%
      pull(ano)

    ops <- as.character(ops)

    if (2013 %in% ops & 2019 %in% ops) {
      ops <- c(ops, "2013 - 2019")
    }

    selectInput(inputId = "sel_ano", label = "Ano", choices = ops)
  })

  output$sel_grafico_UI <- renderUI({
    req(input$sel_abr_tipo)
    req(input$sel_ano)

    if (
      input$sel_abr_tipo == "Unidades da Federação" &
        input$sel_ano %in% c("2013", "2019")
    ) {
      ops <- c("Barras", "Mapa")
    } else {
      ops <- c("Barras")
    }

    selectInput(inputId = "sel_grafico", label = "Visualização", choices = ops)
  })

  output$sel_eixo_x_UI <- renderUI({
    radioButtons(
      "sel_eixo_x",
      label = "Escala do gráfico",
      choices = list("Fixo em 100%" = 1, "Adaptativo" = 2),
      selected = 2
    )
  })

  output$grafico_UI <- renderUI({
    req(input$sel_grafico)

    if (input$sel_grafico == "Barras") {
      highchartOutput(outputId = "distPlot", height = 550)
    } else if (input$sel_grafico == "Mapa") {
      leafglOutput(outputId = "distMap", height = 550)
    }
  })

  observeEvent(input$indi_def, {
    req(input$sel_indicador)

    res <- dic %>%
      filter(nome == !!input$sel_indicador)

    res_notas <- ifelse(is.na(res$notas), "", res$notas)

    showModal(
      modalDialog(
        easyClose = TRUE,
        footer = modalButton("Fechar"),
        title = res$nome,
        h4("Definição"),
        HTML(res$definicao),
        h4("Método de cálculo"),
        HTML(res$metodo_calculo),
        h4("Notas"),
        HTML(res_notas)
      )
    )
  })

  dados <- reactive({
    req(input$sel_indicador)
    req(input$sel_abr_tipo)
    req(input$sel_unidade)
    req(input$sel_ano)

    if (length(cod_indicador_selecionado()) > 0) {
      res <- get(tab_indicador()) %>%
        filter(indicador == cod_indicador_selecionado()) %>%
        filter(abr_tipo == !!input$sel_abr_tipo)

      if (input$sel_ano == "2013 - 2019") {
        res <- res %>%
          filter(ano %in% c(2013, 2019))
      } else {
        res <- res %>%
          filter(ano == !!input$sel_ano)
      }

      if (input$sel_unidade == "Percentual") {
        res <- res %>%
          mutate(
            valor = round(valor * 100, 1),
            interv_inf = round(interv_inf * 100, 1),
            interv_sup = round(interv_sup * 100, 1),
            cv = round(cv * 100, 1),
          )
      }

      # if(input$sel_abr_tipo == "Unidades da Federação"){
      #     res <- res %>%
      #         mutate(abr_nome = fct_relevel(abr_nome, c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", "Tocantins", "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia", "Minas Gerais", "Espírito Santo", "Rio de Janeiro", "São Paulo", "Paraná", "Santa Catarina", "Rio Grande do Sul", "Mato Grosso do Sul", "Mato Grosso", "Goiás", "Distrito Federal")))
      # }

      res
    }
  })

  output$distPlot <- renderHighchart({
    req(input$sel_abr_tipo)
    req(input$sel_unidade)
    req(input$sel_ano)

    titulo <- paste0(
      input$sel_indicador,
      " - ",
      input$sel_unidade,
      " - ",
      input$sel_ano
    )

    if (input$sel_abr_tipo == "Unidades da Federação") {
      cor_barra <- "steelblue"
    } else if (input$sel_abr_tipo == "Grandes Regiões") {
      cor_barra <- "mediumpurple"
    } else if (input$sel_abr_tipo == "Situação urbano/rural") {
      cor_barra <- "orchid"
    } else if (
      input$sel_abr_tipo == "Escolaridade do responsável pelo domicílio"
    ) {
      cor_barra <- "violet"
    } else if (input$sel_abr_tipo == "Rendimento domiciliar per capita") {
      cor_barra <- "orange"
    } else if (input$sel_abr_tipo == "Sexo") {
      cor_barra <- "purple"
    } else if (input$sel_abr_tipo == "Raça/Cor") {
      cor_barra <- "mediumblue"
    } else if (input$sel_abr_tipo == "Faixa de idade") {
      cor_barra <- "forestgreen"
    } else if (input$sel_abr_tipo == "Capitais") {
      cor_barra <- "coral"
    } else if (
      input$sel_abr_tipo == "Escolaridade dos indivíduos de 18 anos ou mais"
    ) {
      cor_barra <- "cyan"
    } else if (input$sel_abr_tipo == "Total") {
      cor_barra <- "green"
    } else {
      cor_barra <- "red"
    }

    if (length(cod_indicador_selecionado()) > 0) {
      if (nrow(dados()) > 0) {
        if (input$sel_ano == "2013 - 2019") {
          res <- dados() %>%
            mutate(label_error_bar = paste("Intervalo de confiança", ano))

          indi_chart <- highchart() %>%
            hc_chart(
              events = list(
                load = JS(
                  "function () {
                            this.series[0].update({
                              id: 'secondColumnSeries'
                            }, false);
                            this.series[1].update({
                              id: 'firstColumnSeries'
                            }, false);
                            this.series[2].update({
                              linkedTo: 'secondColumnSeries'
                            }, false);
                            this.series[3].update({
                              linkedTo: 'firstColumnSeries'
                            });
                          }"
                )
              )
            ) %>%
            hc_xAxis(categories = res$abr_nome) %>%
            hc_legend(enabled = TRUE) %>%
            hc_title(text = titulo) %>%
            hc_tooltip(crosshairs = TRUE, shared = TRUE, valueDecimals = 2) %>%
            hc_exporting(
              enabled = TRUE,
              buttons = list(
                contextButton = list(menuItems = lista_opcoes_grafico)
              )
            ) %>%
            hc_credits(
              enabled = TRUE,
              text = "Fiocruz | ICICT | LIS | PCDaS | IBGE",
              href = "https://bigdata.icict.fiocruz.br"
            ) %>%
            hc_add_series(
              type = "bar",
              data = res,
              hcaes(y = valor, x = abr_nome, group = ano),
              color = c("purple", "orange")
            ) %>%
            hc_add_series(
              data = res,
              type = "errorbar",
              hcaes(
                x = abr_nome,
                low = interv_inf,
                high = interv_sup,
                group = label_error_bar,
                grouping = TRUE
              )
            )

          # Proporção, eixo X até 100
          if (input$sel_unidade == "Percentual" & input$sel_eixo_x == 1) {
            indi_chart <- indi_chart %>%
              hc_yAxis(max = 100)
          }
        } else {
          indi_chart <- highchart() %>%
            hc_xAxis(categories = dados()$abr_nome) %>%
            hc_legend(enabled = FALSE) %>%
            hc_title(text = titulo) %>%
            hc_tooltip(crosshairs = TRUE, shared = TRUE, valueDecimals = 2) %>%
            hc_exporting(
              enabled = TRUE,
              buttons = list(
                contextButton = list(menuItems = lista_opcoes_grafico)
              )
            ) %>%
            hc_credits(
              enabled = TRUE,
              text = "Fiocruz | ICICT | LIS | PCDaS | IBGE",
              href = "https://bigdata.icict.fiocruz.br"
            )

          # Cores condicionais para as barras
          if (input$sel_abr_tipo == "Unidades da Federação") {
            indi_chart <- indi_chart %>%
              hc_add_series(
                type = "bar",
                data = dados(),
                hcaes(
                  y = valor,
                  x = abr_nome,
                  color = c(
                    rep("#377EB8", 7),
                    rep("#4DAF4A", 9),
                    rep("#984EA3", 4),
                    rep("#FF7F00", 3),
                    rep("#00CCCC", 4)
                  )
                ),
                name = "Valor"
              )
          } else if (input$sel_abr_tipo == "Grandes Regiões") {
            indi_chart <- indi_chart %>%
              hc_add_series(
                type = "bar",
                data = dados(),
                hcaes(
                  y = valor,
                  x = abr_nome,
                  color = c(
                    "#377EB8",
                    "#4DAF4A",
                    "#984EA3",
                    "#FF7F00",
                    "#00CCCC"
                  )
                ),
                name = "Valor"
              )
          } else {
            indi_chart <- indi_chart %>%
              hc_add_series(
                type = "bar",
                data = dados(),
                hcaes(y = valor, x = abr_nome),
                color = cor_barra,
                name = "Valor"
              )
          }

          # Adiciona intervalo de confiança
          indi_chart <- indi_chart %>%
            hc_add_series(
              data = list_parse(mutate(
                dados(),
                low = interv_inf,
                high = interv_sup
              )),
              type = "errorbar",
              color = "black",
              name = "Intervalo de confiança"
            )

          # Proporção, eixo X até 100
          if (input$sel_unidade == "Percentual" & input$sel_eixo_x == 1) {
            indi_chart <- indi_chart %>%
              hc_yAxis(max = 100)
          }

          # Observação sobre CVs
          lista_cv <- dados() %>%
            filter(cv > 30) %>%
            distinct(abr_nome) %>%
            pull(abr_nome)

          if (length(lista_cv) > 0) {
            indi_chart <- indi_chart %>%
              hc_caption(
                text = paste0(
                  "<b>CVs maiores que 30%.</b> (",
                  paste(lista_cv, collapse = ", "),
                  ")"
                )
              )
          }
        }

        # Return
        indi_chart
      }
    }
  })

  paleta_mapa <- reactive({
    req(input$sel_indicador)

    dic %>%
      filter(nome == !!input$sel_indicador) %>%
      pull(paleta_mapa)
  })

  output$distMap <- renderLeaflet({
    req(input$sel_abr_tipo)
    req(input$sel_unidade)

    res_dados <- dados()

    res <- uf_shp %>%
      st_cast(to = "POLYGON") %>%
      inner_join(res_dados, by = c("name_state" = "abr_nome"))

    if (input$sel_eixo_x == 1) {
      if (paleta_mapa() == "escuro_claro") {
        pal <- colorNumeric(
          palette = "Oranges",
          reverse = TRUE,
          domain = c(0, 100)
        )
      } else {
        pal <- colorNumeric(
          palette = "Oranges",
          reverse = FALSE,
          domain = c(0, 100)
        )
      }
    } else {
      if (paleta_mapa() == "escuro_claro") {
        pal <- colorNumeric(
          palette = "Oranges",
          reverse = TRUE,
          domain = res$valor
        )
      } else {
        pal <- colorNumeric(
          palette = "Oranges",
          reverse = FALSE,
          domain = res$valor
        )
      }
    }

    map <- leaflet() %>%
      addTiles(group = "OSM") %>%
      addProviderTiles(
        provider = providers$CartoDB.DarkMatter,
        group = "Dark"
      ) %>%
      addProviderTiles(
        provider = providers$CartoDB.Voyager,
        group = "Estradas"
      ) %>%
      addGlPolygons(
        data = res,
        group = "Indicador",
        color = ~ pal(valor),
        popup = paste0(
          "<b>",
          res$name_state,
          "</b>",
          "<br>",
          format(res$valor, decimal = ","),
          " (",
          format(res$interv_inf, decimal = ","),
          " - ",
          format(res$interv_sup, decimal = ","),
          ")"
        )
      ) %>%
      setView(lng = -54.92972, lat = -15.77972, zoom = 4) %>%
      addLayersControl(
        baseGroups = c("OSM", "Dark", "Estradas"),
        overlayGroups = c("Indicador")
      )

    if (input$sel_eixo_x == 1) {
      map <- map %>%
        addLegend(
          title = NULL,
          pal = pal,
          values = c(0, 100),
          position = "bottomright",
          opacity = 80,
          labFormat = labelFormat(digits = 2, big.mark = "")
        )
    } else {
      map <- map %>%
        addLegend(
          title = NULL,
          pal = pal,
          values = res$valor,
          position = "bottomright",
          opacity = 80,
          labFormat = labelFormat(digits = 2, big.mark = "")
        )
    }

    map
  })

  output$tabela <- renderDataTable({
    req(input$sel_indicador)
    req(input$sel_abr_tipo)

    res <- dados() %>%
      select(
        `Abrangência` = abr_tipo,
        Ano = ano,
        Nome = abr_nome,
        Valor = valor,
        `Limite inferior` = interv_inf,
        `Limite superior` = interv_sup,
        CV = cv
      )

    datatable(
      res,
      options = list(
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
        )
      )
    ) %>%
      formatRound(
        digits = 1,
        dec.mark = ",",
        columns = c("Valor", "Limite inferior", "Limite superior")
      )
  })

  output$download_csv <- downloadHandler(
    filename = function() {
      paste("dados_pns", ".csv", sep = "")
    },
    content = function(file) {
      res <- dados() %>%
        select(
          `Abrangência` = abr_tipo,
          Ano = ano,
          Nome = abr_nome,
          Valor = valor,
          `Intervalor inferior` = interv_inf,
          `Intervalor superior` = interv_sup,
          CV = cv
        )

      write_excel_csv2(x = res, path = file, na = "")
    }
  )

  output$download_xlsx <- downloadHandler(
    filename = function() {
      paste("dados_pns", ".xlsx", sep = "")
    },
    content = function(file) {
      res <- dados() %>%
        select(
          `Abrangência` = abr_tipo,
          Ano = ano,
          Nome = abr_nome,
          Valor = valor,
          `Intervalor inferior` = interv_inf,
          `Intervalor superior` = interv_sup
        )
      write_xlsx(x = res, path = file)
    }
  )

  # Aba comparação

  output$comp_sel_abr_UI <- renderUI({
    req(input$comp_sel_modulo)

    res1 <- dic %>%
      filter(modulo == !!input$comp_sel_modulo) %>%
      distinct(cod) %>%
      pull(cod)

    res2 <- get(tab_indicador()) %>%
      filter(indicador %in% res1) %>%
      distinct(abr_tipo) %>%
      filter(
        abr_tipo %in%
          c("Total", "Grandes Regiões", "Unidades da Federação", "Capitais")
      ) %>%
      pull(abr_tipo)

    res2 <- sort(factor(
      res2,
      levels = c(
        "Total",
        "Grandes Regiões",
        "Unidades da Federação",
        "Capitais"
      ),
      ordered = TRUE
    ))

    selectInput(inputId = "comp_sel_abr", label = "Abrangência", choices = res2)
  })

  output$comp_sel_abr_elemento_UI <- renderUI({
    req(input$comp_sel_abr)

    res <- get(tab_indicador()) %>%
      filter(abr_tipo == !!input$comp_sel_abr) %>%
      distinct(abr_nome) %>%
      pull(abr_nome)

    selectInput(
      inputId = "comp_sel_abr_elemento",
      label = "Unidade",
      choices = res
    )
  })

  output$comp_sel_eixo_x_UI <- renderUI({
    radioButtons(
      "comp_sel_eixo_x",
      label = "Escala do gráfico",
      choices = list("Fixo em 100%" = 1, "Adaptativo" = 2),
      selected = 2
    )
  })

  output$comp_grafico <- renderHighchart({
    req(input$comp_sel_abr_elemento)

    lista_indi <- dic %>%
      filter(modulo == !!input$comp_sel_modulo) %>%
      select(cod, nome)

    indi_grafico <- get(tab_indicador()) %>%
      filter(abr_tipo == !!input$comp_sel_abr) %>%
      filter(abr_nome == !!input$comp_sel_abr_elemento) %>%
      filter(ano %in% c(2013, 2019)) %>%
      filter(indicador %in% lista_indi$cod) %>%
      mutate(
        valor = round(valor * 100, 1),
        interv_inf = round(interv_inf * 100, 1),
        interv_sup = round(interv_sup * 100, 1)
      ) %>%
      left_join(lista_indi, by = c("indicador" = "cod")) %>%
      mutate(label_error_bar = paste("Intervalo de confiança", ano)) %>%
      arrange(nome) %>%
      group_by(nome) %>%
      mutate(freq = n()) %>%
      ungroup() %>%
      arrange(desc(freq)) %>%
      select(-freq)

    if (nrow(indi_grafico) > 0) {
      if (length(unique(indi_grafico$ano)) == 2) {
        titulo_grafico <- paste0(
          input$comp_sel_modulo,
          " - ",
          input$comp_sel_abr_elemento,
          " - 2013 - 2019"
        )

        res <- highchart() %>%
          hc_chart(
            events = list(
              load = JS(
                "function () {
                            this.series[0].update({
                              id: 'secondColumnSeries'
                            }, false);
                            this.series[1].update({
                              id: 'firstColumnSeries'
                            }, false);
                            this.series[2].update({
                              linkedTo: 'secondColumnSeries'
                            }, false);
                            this.series[3].update({
                              linkedTo: 'firstColumnSeries'
                            });
                          }"
              )
            )
          ) %>%
          hc_xAxis(categories = unique(indi_grafico$nome)) %>%
          hc_legend(enabled = TRUE) %>%
          hc_title(text = titulo_grafico) %>%
          hc_tooltip(crosshairs = TRUE, shared = TRUE, valueDecimals = 2) %>%
          hc_exporting(
            enabled = TRUE,
            buttons = list(
              contextButton = list(menuItems = lista_opcoes_grafico)
            )
          ) %>%
          hc_credits(
            enabled = TRUE,
            text = "Fiocruz | ICICT | LIS | PCDaS | IBGE",
            href = "https://bigdata.icict.fiocruz.br"
          ) %>%
          hc_add_series(
            type = "bar",
            data = indi_grafico,
            hcaes(y = valor, x = nome, group = ano),
            color = c("purple", "orange")
          ) %>%
          hc_add_series(
            data = indi_grafico,
            type = "errorbar",
            hcaes(
              x = nome,
              low = interv_inf,
              high = interv_sup,
              group = label_error_bar,
              grouping = TRUE
            )
          )

        if (input$comp_sel_eixo_x == 1) {
          res <- res %>% hc_yAxis(max = 100)
        }

        res
      } else {
        titulo_grafico <- paste0(
          input$comp_sel_modulo,
          " - ",
          input$comp_sel_abr_elemento,
          " - ",
          unique(indi_grafico$ano)[1]
        )

        res <- highchart() %>%
          hc_xAxis(categories = unique(indi_grafico$nome)) %>%
          hc_legend(enabled = FALSE) %>%
          hc_title(text = titulo_grafico) %>%
          hc_tooltip(crosshairs = TRUE, shared = TRUE, valueDecimals = 2) %>%
          hc_exporting(
            enabled = TRUE,
            buttons = list(
              contextButton = list(menuItems = lista_opcoes_grafico)
            )
          ) %>%
          hc_credits(
            enabled = TRUE,
            text = "Fiocruz | ICICT | LIS | PCDaS | IBGE",
            href = "https://bigdata.icict.fiocruz.br"
          ) %>%
          hc_add_series(
            type = "bar",
            data = indi_grafico,
            hcaes(y = valor, x = nome),
            color = ifelse(
              unique(indi_grafico$ano)[1] == "2019",
              "purple",
              "orange"
            ),
            name = "Valor"
          ) %>%
          hc_add_series(
            data = list_parse(mutate(
              indi_grafico,
              low = interv_inf,
              high = interv_sup
            )),
            type = "errorbar",
            color = "black",
            name = "Intervalo de confiança"
          )

        if (input$comp_sel_eixo_x == 1) {
          res <- res %>% hc_yAxis(max = 100)
        }

        res
      }
    }
  })
}

# Run
shinyApp(ui = ui, server = server)
