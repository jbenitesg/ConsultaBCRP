#' Diccionario de la API
#' 
#' @description
#' Consulta de series de datos disponibles en el API del BCRP según fecha, nombre, conjunto de datos, entre otros
#' @import jsonlite
#' @import tidyverse
#' @import zoo
#' @import haven
#' @import httr
#' @importFrom data.table fread
#' @import janitor
#' @return Base de datos de todas las series disponibles en el API del BCRP
#' @examples

#' diccionario_bcrp() # Retorno de toda la serie
#' @export
diccionario_bcrp <- function() {
  api <- "https://estadisticas.bcrp.gob.pe/estadisticas/series/metadata"
  
  # Descargar los datos y guardarlos temporalmente
  httr::GET(url = api,
            httr::write_disk(
              tf <- tempfile(fileext = ".csv")
              )
            )
  
  # Leer los datos descargados y procesarlos
  data <- data.table::fread(tf, sep = ";", encoding = "Latin-1") %>%
    janitor::clean_names() %>%
    dplyr::select(codigo_de_serie:fecha_de_fin, -area_que_publica)
  return(data)
}

#' Consulta BCRP API
#' @description
#' Permite obtener de forma rápida información del API del BCRP
#' #' 
#' @param series Vector de series a consultar del API. 
#' @param fechaini Periodo inicial de consulta
#' @param fechafin Periodo final de la consulta
#' @param labels Permite incluir etiquetado a los indicadores de c("default", "manual","no"). En la opción "manual", el usuario puede añadir los labels de las variables en argumento complementario labs_opcional. Cuando la opción "no" se selecciona, no se incluyen labels.
#' @import jsonlite
#' @import tidyverse
#' @import zoo
#' @import haven
#' @import httr
#' @import janitor
#' @import lubridate
#' @return Retorna una base de datos con la(s) serie(s) en el periodo escogido.
#' @examples
#' bcrp_data(series = c("PN01728AM"), fechaini = "2004-2", fechafin = "2019-12") # Mensual
#' bcrp_data(series = c("PN02507AQ"), fechaini = "2019-1", fechafin = "2022-4") # Trimestral
#' bcrp_data(series = c("PM04983AA"), fechaini = "2007", fechafin = "2022") # Anual
#' 
#' @export
bcrp_data <- function(series, fechaini, fechafin, labels = "default", labs_opcional = NULL) {
  
  link <- "https://estadisticas.bcrp.gob.pe/estadisticas/series/api"
  k <- 1
  temp <- list()
  for (serie in series) {
    # Mayúsculas
    serie = toupper(serie)
    # Enlace funcional
    full_link <- paste(link, serie, "json", fechaini, fechafin, "ing", sep = "/")
    #Procesamos
    tp <- jsonlite::fromJSON(
      readLines(
        full_link,
        warn = "F",
        encoding = "Latin-1"
      )
    )$periods %>%
      as.data.frame(row.names = NULL)  %>%
      dplyr::mutate(across(
        !contains("name"),
        ~ as.numeric(.x)
      ))
    tp <- tp %>%
      `colnames<-`(c("fecha", serie))
    
    if (labels == "default"){
      lab <- diccionario_bcrp() %>%
        dplyr::filter(codigo_de_serie == serie) %>%
        dplyr::pull(nombre_de_serie)
      tp[[serie]]<- haven::labelled(tp[[serie]], label = lab) 
    }
    else if (labels == "manual"){
      lab <- labs_opcional[k]
      tp[[serie]]<- haven::labelled(tp[[serie]], label = lab) 
    }
    else if (labels == "no") {
      tp <- tp
    }
    
    # Asignación de periodo
    periodo = dplyr::case_when(
      # Anual
      grepl("A$", serie) ~ "A",
      # Trimestral
      grepl("Q$", serie) ~ "T",
      # Mensual
      grepl("M$", serie) ~ "M",
      # Diaria
      grepl("D$", serie) ~ "D"
    )
    
    # Asignación
    if (periodo == "D") {
      # Perido diario
      tp <- tp %>%
        tidyr::separate_wider_delim(fecha,
                                    delim = ".",
                                    names = c("dia", "mes", "anio")
        ) %>%
        dplyr::mutate(
          dia = as.numeric(dia),
          anio = as.numeric(
            dplyr::case_when(
              substr(fechaini, 1, 2) == "19" &
                as.numeric(anio) > 23 ~ paste("19", anio, sep = ""),
              T ~ paste("20", anio, sep = "")
            )
          )
        ) %>%
        dplyr::left_join(meses, by = c("mes")) %>%
        dplyr::mutate(fecha = make_date(
          year = anio,
          month = mesid,
          day = dia
        )) 
    } else if (periodo == "M") {
      # Periodo mensual
      tp <- tp %>%
        dplyr::mutate(fecha = lubridate::my(fecha))
    } else if (periodo == "T") {
      # Periodo trimestral
      tp <- tp %>%
        dplyr::mutate(
          anio = substr(fecha, 4, 5),
          trim = substr(fecha, 2, 2),
          anio = as.numeric(
            dplyr::case_when(
              substr(fechaini, 1, 2) == "19" & as.numeric(anio) > 23 ~ paste("19", anio, sep = ""),
              T ~ paste("20", anio, sep = "")
            )
          ),
          fecha = zoo::as.yearqtr(paste(anio, " Q", trim))
        )
      
    } else if (periodo == "A") {
      # Periodos anuales
      tp <- tp %>%
        dplyr::mutate(fecha = as.integer(fecha))
    } else {
      print("No admitido el periodo")
    }
    
    temp[[k]] <- tp 
    k <- k + 1
  }
  temp <- temp %>%
    purrr::reduce(dplyr::full_join,
                  by = c("fecha")
    ) %>%
    dplyr::select(fecha, all_of(series)) %>%
    `colnames<-`(c("Fecha", series))
  # Resultado final
  return(temp)
}
