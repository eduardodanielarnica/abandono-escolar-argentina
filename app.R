
# =============================================================================
# 0. LIBRERÍAS
# =============================================================================

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)
library(plotly)
library(DT)
library(readxl)


# =============================================================================
# 1. CARGA DE DATOS
# =============================================================================

if (!file.exists("tasa_abandono_limpia.xlsx")) {
  stop("¡Error crítico! El archivo 'tasa_abandono_limpia.xlsx' no se encuentra.")
}

df_app <- read_excel("tasa_abandono_limpia.xlsx")


# Buscar columna de años (formato YYYY-YYYY)

columna_anios <- which(
  sapply(df_app, function(x) {
    any(str_detect(as.character(x), "^\\d{4}-\\d{4}"))
  })
)[1]


# Buscar columna de tasas (numérica < 100)

columna_tasas <- which(
  sapply(df_app, function(x) {
    is.numeric(x) && max(x, na.rm = TRUE) < 100
  })
)[1]


if (!is.na(columna_anios) && !is.na(columna_tasas)) {
  
  nombres <- names(df_app)
  
  nombres[columna_tasas] <- "Tasa"
  nombres[columna_anios] <- "Anio_Lectivo"
  
  names(df_app) <- nombres
}


# Nombres correctos para primeras 3 columnas

if (ncol(df_app) >= 3) {
  names(df_app)[1:3] <- c(
    "Provincia",
    "Nivel",
    "Anio_Estudio"
  )
}


# Convertir Tasa a numérico

if (!"Tasa" %in% names(df_app)) {
  
  col_num <- which(sapply(df_app, is.numeric))[1]
  
  if (!is.na(col_num)) {
    names(df_app)[col_num] <- "Tasa"
  }
}

df_app$Tasa <- as.numeric(df_app$Tasa)


# Asegurar Anio_Lectivo

if (!"Anio_Lectivo" %in% names(df_app)) {
  
  col_anio <- which(
    sapply(df_app, function(x) {
      any(str_detect(as.character(x), "\\d{4}-\\d{4}"))
    })
  )[1]
  
  if (!is.na(col_anio)) {
    names(df_app)[col_anio] <- "Anio_Lectivo"
  }
}

df_app$Anio_Lectivo <- as.character(df_app$Anio_Lectivo)


# Reordenar columnas

columnas_deseadas <- c(
  "Provincia",
  "Nivel",
  "Anio_Estudio",
  "Tasa",
  "Anio_Lectivo"
)

columnas_existentes <- columnas_deseadas[
  columnas_deseadas %in% names(df_app)
]

df_app <- df_app %>%
  select(all_of(columnas_existentes))


# Filtrar filas no deseadas

df_app <- df_app %>%
  filter(!is.na(Tasa)) %>%
  filter(
    !str_detect(
      tolower(Provincia),
      "realizado|nota|fuente|ley|educación|ministerio"
    )
  )


# Eliminar duplicados

df_app <- df_app %>%
  group_by(
    Provincia,
    Nivel,
    Anio_Estudio,
    Anio_Lectivo
  ) %>%
  summarise(
    Tasa = mean(Tasa, na.rm = TRUE),
    .groups = "drop"
  )


# Redondear Tasa

df_app$Tasa <- round(df_app$Tasa, 1)


# Verificación

cat("=== VERIFICACIÓN FINAL ===\n")
cat("Filas:", nrow(df_app), "\n")

cat(
  "Columnas:",
  paste(names(df_app), collapse = ", "),
  "\n"
)

cat(
  "Años únicos:",
  paste(
    sort(unique(df_app$Anio_Lectivo)),
    collapse = ", "
  ),
  "\n"
)

cat(
  "Valores de Anio_Estudio:",
  paste(
    unique(df_app$Anio_Estudio),
    collapse = ", "
  ),
  "\n"
)


# =============================================================================
# 2. INTERFAZ
# =============================================================================

ui <- dashboardPage(
  
  skin = "blue",
  
  
  # ---------------------------------------------------------------------------
  # ENCABEZADO
  # ---------------------------------------------------------------------------
  
  dashboardHeader(
    title = "Abandono Escolar"
  ),
  
  
  # ---------------------------------------------------------------------------
  # PANEL LATERAL
  # ---------------------------------------------------------------------------
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Comparación por provincia",
        tabName = "comparativa",
        icon = icon("chart-bar")
      ),
      
      menuItem(
        "Evolución por provincia",
        tabName = "evolucion_prov",
        icon = icon("chart-line")
      ),
      
      menuItem(
        "Evolución por nivel",
        tabName = "evolucion_nivel",
        icon = icon("graduation-cap")
      ),
      
      menuItem(
        "Tabla interactiva",
        tabName = "tabla_tab",
        icon = icon("table")
      ),
      
      menuItem(
        "Marco Metodológico",
        tabName = "info",
        icon = icon("university")
      )
    ),
    
    hr(),
    
    tags$div(
      
      style = "padding: 15px; color: white;",
      
      selectInput(
        "select_provincia",
        "Seleccionar provincia:",
        choices = sort(unique(df_app$Provincia)),
        selected = "Total País"
      ),
      
      radioButtons(
        "radio_nivel",
        "Nivel educativo:",
        choices = unique(df_app$Nivel),
        selected = unique(df_app$Nivel)[1]
      ),
      
      selectInput(
        "select_anio",
        "Año lectivo:",
        choices = sort(unique(df_app$Anio_Lectivo)),
        selected = max(df_app$Anio_Lectivo)
      )
    )
  ),
  
  
  # ---------------------------------------------------------------------------
  # CUERPO DEL TABLERO
  # ---------------------------------------------------------------------------
  
  dashboardBody(
    
    tags$head(
      
      tags$style(
        
        HTML("
        
        .academic-header {
          background: linear-gradient(
            135deg,
            #f8fafc 0%,
            #e2e8f0 100%
          );
          
          padding: 35px 20px;
          border-bottom: 4px solid #003366;
          border-top: 4px solid #003366;
          margin-bottom: 30px;
          border-radius: 15px;
          box-shadow: 0 10px 15px -3px rgba(0,0,0,0.1);
          text-align: center;
          width: 100%;
        }
        
        
        .academic-title {
          color: #003366;
          font-family: Arial, sans-serif;
          font-weight: bold;
          margin: 15px 0 10px 0;
          font-size: 28px;
          text-align: center;
          letter-spacing: -0.5px;
        }
        
        
        .academic-subtitle {
          color: #1e4a76;
          font-weight: 600;
          margin: 10px 0;
          text-align: center;
          font-size: 18px;
        }
        
        
        .academic-university {
          color: #2d3748;
          text-align: center;
          margin: 10px 0;
          font-size: 16px;
        }
        
        ")
      )
    ),
    
    
    # -------------------------------------------------------------------------
    # PORTADA
    # -------------------------------------------------------------------------
    
    tags$div(
      
      class = "academic-header",
      
      tags$h2(
        class = "academic-title",
        "Abandono Escolar en Argentina"
      ),
      
      tags$h4(
        class = "academic-subtitle",
        "Exploración y visualización interactiva de indicadores educativos"
      ),
      
      tags$p(
        class = "academic-university",
        "R · Shiny · Análisis de Datos · Visualización"
      ),
      
      tags$div(
        "Eduardo Árnica"
      )
    ),
    
    
    # -------------------------------------------------------------------------
    # PESTAÑAS
    # -------------------------------------------------------------------------
    
    tabItems(
      
      
      # =======================================================================
      # COMPARACIÓN PROVINCIAL
      # =======================================================================
      
      tabItem(
        
        tabName = "comparativa",
        
        fluidRow(
          
          box(
            
            title = "Comparativa provincial",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            plotlyOutput(
              "plot_barras",
              height = "500px"
            ),
            
            tags$p(
              
              style = paste0(
                "font-size: 13px;",
                "color: #555;",
                "margin: 8px 15px 5px 15px;"
              ),
              
              tags$strong("Nota: "),
              
              paste(
                "Los valores negativos pueden responder a movimientos",
                "interanuales de matrícula entre jurisdicciones,",
                "ámbitos o sectores de gestión."
              )
            )
          )
        )
      ),
      
      
      # =======================================================================
      # EVOLUCIÓN POR PROVINCIA
      # =======================================================================
      
      tabItem(
        
        tabName = "evolucion_prov",
        
        fluidRow(
          
          box(
            
            title = "Evolución temporal por provincia",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            
            plotlyOutput(
              "plot_lineas",
              height = "500px"
            )
          )
        )
      ),
      
      
      # =======================================================================
      # EVOLUCIÓN POR NIVEL
      # =======================================================================
      
      tabItem(
        
        tabName = "evolucion_nivel",
        
        fluidRow(
          
          box(
            
            title = "Comparación por nivel educativo",
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            
            plotlyOutput(
              "plot_niveles",
              height = "500px"
            )
          )
        )
      ),
      
      
      # =======================================================================
      # TABLA INTERACTIVA
      # =======================================================================
      
      tabItem(
        
        tabName = "tabla_tab",
        
        fluidRow(
          
          box(
            
            title = "Explorador de datos",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            
            DTOutput(
              "tabla_dt"
            )
          )
        )
      ),
      
      
      # =======================================================================
      # MARCO METODOLÓGICO
      # =======================================================================
      
      tabItem(
        
        tabName = "info",
        
        fluidRow(
          
          box(
            
            title = "Ficha Técnica",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
            tags$div(
              
              tags$h4(
                "📊 Fuente de datos"
              ),
              
              tags$p(
                "Datos abiertos de la Secretaría de Educación de Argentina"
              ),
              
              tags$a(
                
                href = paste0(
                  "https://www.argentina.gob.ar/educacion/",
                  "evaluacion-e-informacion-educativa/",
                  "datos-abiertos-de-la-secretaria-de-educacion"
                ),
                
                "Repositorio Oficial",
                
                target = "_blank"
              ),
              
              tags$br(),
              tags$br(),
              
              
              tags$h4(
                "📐 Metodología"
              ),
              
              
              tags$h5(
                tags$strong(
                  "Tasa de Abandono Interanual"
                )
              ),
              
              
              tags$p(
                paste(
                  "La Tasa de Abandono Interanual permite analizar",
                  "el flujo de estudiantes entre dos años lectivos",
                  "consecutivos. Representa la proporción de alumnos",
                  "que, habiendo estado matriculados en un año",
                  "lectivo determinado, no vuelven a matricularse",
                  "en el sistema educativo al año siguiente."
                )
              ),
              
              
              tags$p(
                paste(
                  "El indicador se construye a partir de las tasas",
                  "de promoción efectiva, repitencia y reinscripción,",
                  "calculándose como la diferencia entre el 100 %",
                  "y la suma de dichas tasas."
                )
              ),
              
              
              tags$h5(
                tags$strong(
                  "Interpretación de valores negativos"
                )
              ),
              
              
              tags$p(
                paste(
                  "En determinadas jurisdicciones pueden observarse",
                  "valores negativos. Estos valores no necesariamente",
                  "representan un error en los datos, ya que pueden",
                  "originarse por movimientos de matrícula entre",
                  "jurisdicciones, ámbitos o sectores de gestión."
                )
              ),
              
              
              tags$p(
                paste(
                  "Por lo tanto, los valores negativos deben",
                  "interpretarse con cautela. La tasa correspondiente",
                  "a un nivel educativo refleja la situación entre",
                  "dos años lectivos consecutivos y no debe",
                  "interpretarse como una tasa acumulada de abandono",
                  "a lo largo de toda la trayectoria educativa."
                )
              ),
              
              
              tags$ul(
                
                tags$li(
                  paste(
                    "Período analizado:",
                    "ciclos lectivos 2003-2004 a 2023-2024"
                  )
                ),
                
                tags$li(
                  "Cobertura: 24 jurisdicciones + Total País"
                )
              )
            )
          )
        )
      )
    )
  )
)


# =============================================================================
# 3. SERVER
# =============================================================================

server <- function(input, output, session) {
  
  
  # ===========================================================================
  # GRÁFICO DE BARRAS: COMPARACIÓN ENTRE PROVINCIAS
  # ===========================================================================
  
  output$plot_barras <- renderPlotly({
    
    req(
      input$radio_nivel,
      input$select_anio
    )
    
    
    df_barras <- df_app %>%
      
      filter(
        Provincia != "Total País",
        Nivel == input$radio_nivel,
        Anio_Lectivo == input$select_anio,
        Anio_Estudio == "Total"
      ) %>%
      
      mutate(
        Provincia = fct_reorder(
          Provincia,
          Tasa,
          .desc = TRUE
        )
      )
    
    
    if (nrow(df_barras) == 0) {
      
      return(
        plotly_empty(
          type = "bar",
          title = "No hay datos disponibles"
        )
      )
    }
    
    
    promedio_nacional <- mean(
      df_barras$Tasa,
      na.rm = TRUE
    )
    
    
    p <- df_barras %>%
      
      ggplot(
        
        aes(
          
          x = Provincia,
          y = Tasa,
          fill = Tasa,
          
          text = paste0(
            
            "Provincia: ",
            Provincia,
            
            "\nTasa: ",
            round(Tasa, 1),
            "%",
            
            "\nPromedio: ",
            round(promedio_nacional, 1),
            "%"
          )
        )
      ) +
      
      geom_col() +
      
      scale_fill_gradient(
        low = "#a8d1ff",
        high = "#003366",
        name = "Tasa (%)"
      ) +
      
      geom_hline(
        yintercept = promedio_nacional,
        color = "#e74c3c",
        linetype = "dashed",
        linewidth = 0.8
      ) +
      
      annotate(
        
        "text",
        
        x = 5,
        
        y = promedio_nacional + 0.7,
        
        label = paste0(
          "Promedio nacional: ",
          round(promedio_nacional, 1),
          "%"
        ),
        
        color = "#e74c3c",
        size = 3,
        hjust = 0
      ) +
      
      theme_minimal() +
      
      labs(
        x = "Jurisdicciones",
        y = "Tasa de abandono (%)"
      ) +
      
      theme(
        
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          size = 8
        ),
        
        legend.position = "right"
      )
    
    
    ggplotly(
      p,
      tooltip = "text"
    ) %>%
      
      layout(
        
        margin = list(
          b = 120,
          l = 60
        )
      )
  })
  
  
  
  # ===========================================================================
  # GRÁFICO DE LÍNEAS: EVOLUCIÓN TEMPORAL
  # ===========================================================================
  
  output$plot_lineas <- renderPlotly({
    
    req(
      input$select_provincia,
      input$radio_nivel
    )
    
    
    df_lineas <- df_app %>%
      
      filter(
        Provincia == input$select_provincia,
        Nivel == input$radio_nivel,
        Anio_Estudio == "Total"
      ) %>%
      
      arrange(
        Anio_Lectivo
      )
    
    
    if (nrow(df_lineas) == 0) {
      
      return(
        
        plotly_empty(
          type = "scatter",
          mode = "markers",
          title = "No hay datos disponibles"
        )
      )
    }
    
    
    p <- df_lineas %>%
      
      ggplot(
        
        aes(
          
          x = as.factor(Anio_Lectivo),
          y = Tasa,
          group = 1,
          
          text = paste0(
            
            "Año: ",
            Anio_Lectivo,
            
            "\nTasa: ",
            round(Tasa, 1),
            "%"
          )
        )
      ) +
      
      geom_line(
        color = "#003366",
        linewidth = 1.2
      ) +
      
      geom_point(
        color = "#e74c3c",
        size = 3
      ) +
      
      theme_minimal() +
      
      labs(
        x = "Año lectivo",
        y = "Tasa de abandono (%)"
      ) +
      
      theme(
        
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
    
    
    ggplotly(
      p,
      tooltip = "text"
    )
  })
  
  
  
  # ===========================================================================
  # COMPARACIÓN PRIMARIA VS SECUNDARIA
  # ===========================================================================
  
  output$plot_niveles <- renderPlotly({
    
    req(
      input$select_provincia
    )
    
    
    df_niveles <- df_app %>%
      
      filter(
        Provincia == input$select_provincia,
        Anio_Estudio == "Total"
      ) %>%
      
      group_by(
        Anio_Lectivo,
        Nivel
      ) %>%
      
      summarise(
        Tasa = mean(Tasa, na.rm = TRUE),
        .groups = "drop"
      )
    
    
    if (nrow(df_niveles) == 0) {
      
      return(
        
        plotly_empty(
          type = "scatter",
          mode = "lines+markers",
          title = "No hay datos disponibles"
        )
      )
    }
    
    
    p <- df_niveles %>%
      
      ggplot(
        
        aes(
          
          x = as.factor(Anio_Lectivo),
          y = Tasa,
          color = Nivel,
          group = Nivel,
          
          text = paste0(
            
            "Año: ",
            Anio_Lectivo,
            
            "\nNivel: ",
            Nivel,
            
            "\nTasa: ",
            round(Tasa, 1),
            "%"
          )
        )
      ) +
      
      geom_line(
        linewidth = 1.2
      ) +
      
      geom_point(
        size = 2.5
      ) +
      
      scale_color_manual(
        values = c(
          "#003366",
          "#e74c3c"
        )
      ) +
      
      theme_minimal() +
      
      labs(
        x = "Año lectivo",
        y = "Tasa de abandono (%)",
        color = "Nivel"
      ) +
      
      theme(
        
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
    
    
    ggplotly(
      p,
      tooltip = "text"
    )
  })
  
  
  
  # ===========================================================================
  # TABLA INTERACTIVA
  # ===========================================================================
  
  output$tabla_dt <- renderDT({
    
    datatable(
      
      df_app,
      
      extensions = c(
        "Buttons",
        "Responsive"
      ),
      
      options = list(
        
        pageLength = 10,
        
        scrollX = TRUE,
        
        dom = "Bfrtip",
        
        buttons = c(
          "copy",
          "csv",
          "excel",
          "pdf",
          "print"
        ),
        
        language = list(
          
          url = paste0(
            "//cdn.datatables.net/plug-ins/",
            "1.10.11/i18n/Spanish.json"
          ),
          
          search = "🔍 Buscar:",
          
          lengthMenu = "Mostrar _MENU_ registros",
          
          info = paste(
            "Mostrando _START_ a _END_",
            "de _TOTAL_ registros"
          )
        )
      ),
      
      filter = "top",
      
      rownames = FALSE,
      
      class = "display compact stripe hover",
      
      colnames = c(
        "Provincia",
        "Nivel",
        "Año de Estudio",
        "Tasa (%)",
        "Año Lectivo"
      )
      
    ) %>%
      
      formatStyle(
        
        "Tasa",
        
        background = styleColorBar(
          
          c(
            0,
            max(df_app$Tasa, na.rm = TRUE)
          ),
          
          "#a8d1ff"
        ),
        
        backgroundSize = "100% 90%",
        
        backgroundRepeat = "no-repeat",
        
        backgroundPosition = "center"
      )
  })
}


# =============================================================================
# 4. EJECUTAR
# =============================================================================

shinyApp(
  ui = ui,
  server = server
)
