library(shiny)
library(leaflet)
library(dplyr)
library(jsonlite)
library(tidyr)
library(httr)
library(tibble)
library(purrr)
library(sf)
library(future.apply)
library(shinyjs)
library(stringr)
library(leaflet.extras)
library(readxl)
library(xml2)

# ========= Funções de Consulta e Processamento =========              

get_sicar_data <- function(car_number) {
  url_gov <- paste0("https://www.car.gov.br/imovel/demonstrativo/", car_number, "/gerarComRecaptcha")
  responseGOV <- GET(url_gov)
  htmlConsultaGOV <- content(responseGOV)
  dados <- as_tibble(modify_if(htmlConsultaGOV[["dados"]][["cabecalho"]], is.null, ~ NA))
  return(dados)
}

# Função para obter o token da API Visipec - agora recebe email e password como argumentos
get_token_visipec <- function(email, password) {
  url_visipec <- "https://api.visipec.com/api/v22/authenticate-user"
  body_visipec <- list(Email = email, 
                       Password = password)
  body_json_visipec <- toJSON(body_visipec, auto_unbox = TRUE)
  headers_visipec <- c("accept" = "text/plain", "Content-Type" = "text/json")
  response_visipec <- POST(url_visipec, body = body_json_visipec, add_headers(.headers = headers_visipec))
  
  if (http_status(response_visipec)$category == "Success") {
    content_visipec <- content(response_visipec, "parsed")
    return(content_visipec$AccessToken)
  } else {
    stop("Erro na autenticação Visipec")
  }
}

# Função para gerar o mapa (adaptada para múltiplos CARs)
gerar_mapa_final <- function(car_number, token_visipec,
                             data_visipec_inicial = Sys.Date(),
                             data_visipec_final = Sys.Date()) {
  
  # Formata as datas para ISO8601
  formatted_date_inicial <- format(as.POSIXct(data_visipec_inicial),
                                   format = "%Y-%m-%dT00:00:00.000Z",
                                   tz = "UTC")
  formatted_date_final <- format(as.POSIXct(data_visipec_final),
                                 format = "%Y-%m-%dT00:00:00.000Z",
                                 tz = "UTC")
  
  # Consulta os dados do CAR para cada número informado
  Faz_list <- lapply(car_number, get_sicar_data)
  Faz <- bind_rows(Faz_list)
  
  # --- BUSCA DOS FORNECEDORES INDIRETOS (VISIPEC) ---
  buscar_dados_visipec <- function(CAR, token_visipec) {
    url <- "https://api.visipec.com/api/v22/general-information/car"
    headers <- c(
      "accept" = "text/plain",
      "AccessToken" = token_visipec,
      "Content-Type" = "application/json-patch+json"
    )
    data <- list("Values" = CAR,
                 "StartDate" = formatted_date_inicial,
                 "EndDate" = formatted_date_final)
    response <- POST(url,
                     add_headers(.headers = headers),
                     body = data,
                     encode = "json")
    content(response)
  }
  
  # Inicializa o mapa vazio
  map_final <- leaflet() %>% addTiles()
  nome_grupo <- NA
  basetotalf2 <- data.frame(matrix(NA, ncol = 20))
  colunaBasef <- c("CAR_Fornecedor_Direto",                      
                   "CAR_Fornecedor_Indireto",                    
                   "Codigo_de_estabelecimento_Federal_Indireto", 
                   "Codigo_de_estabelecimento_Estadual_Indireto",
                   "Estado",                                    
                   "Municipio",                                  
                   "Numero_Transacoes",                          
                   "Ultima_transacao",                          
                   "OCIrregular",                                
                   "BPIrregular",                                
                   "Embargo",                                    
                   "territorio_indigena",                        
                   "ForcedLabor",                                
                   "Unidade_Conservacao",                        
                   "IsOver100ha",                                
                   "DeforestationArea",                          
                   "lat_indireto",                               
                   "long_indireto",                              
                   "lat_direto",                                
                   "long_direto")
  
  colnames(basetotalf2) <- colunaBasef
  
  for (ll in seq_along(car_number)) {
    base <- buscar_dados_visipec(list(car_number[ll]), token_visipec)
    
    if (length(base$DirectSuppliers) != 0) {
      
      process_supplier <- function(direct_supplier) {
        direct_car <- direct_supplier$CarNumber
        indirects <- direct_supplier$IndirectSuppliers
        if (length(indirects) > 0) {
          map_dfr(indirects, function(ind) {
            tibble(
              CAR_Fornecedor_Direto = direct_car,
              CAR_Fornecedor_Indireto = ind$CarNumber,
              Codigo_de_estabelecimento_Federal_Indireto = ind$FederalEstablishmentCode,
              Codigo_de_estabelecimento_Estadual_Indireto = ind$StateEstablishmentCode,
              Estado = ind$State,
              Municipio = ind$Municipality,
              Numero_Transacoes = ind$TransactionCount,
              Ultima_transacao = ind$LastTransactionDate,
              OCIrregular = ind$OCIrregular,
              BPIrregular = ind$BPIrregular,
              Embargo = ind$Embargoed,
              territorio_indigena = ind$IndigenousAreaOverlap,
              ForcedLabor = ind$ForcedLabor,
              Unidade_Conservacao = ind$ConservationAreaOverlap,
              IsOver100ha = ind$IsOver100ha,
              DeforestationArea = list(ind$DeforestationArea)
            )
          })
        } else {
          tibble(
            CAR_Fornecedor_Direto = direct_car,
            CAR_Fornecedor_Indireto = NA_character_,
            Codigo_de_estabelecimento_Federal_Indireto = NA_character_,
            Codigo_de_estabelecimento_Estadual_Indireto = NA_character_,
            Estado = NA_character_,
            Municipio = NA_character_,
            Numero_Transacoes = NA_integer_,
            Ultima_transacao = NA_character_,
            OCIrregular = NA,
            BPIrregular = NA,
            Embargo = NA,
            territorio_indigena = NA,
            ForcedLabor = NA,
            Unidade_Conservacao = NA,
            IsOver100ha = NA,
            DeforestationArea = list(NA)
          )
        }
      }
      
      fornecedores_df <- map_dfr(base$DirectSuppliers, process_supplier)
      
      extract_Prodes <- function(sublist) {
        if (length(sublist) > 0) {
          df <- do.call(rbind, lapply(sublist, as.data.frame))
          return(df)
        } else {
          return(data.frame(Year = NA, Ha = NA, Source = NA, PctAmazonForest = NA))
        }
      }
      
      basetotal <- fornecedores_df
      
      # Obtenção das coordenadas para fornecedores indiretos
      unique_indiretos <- unique(basetotal$CAR_Fornecedor_Indireto)
      coords_indiretos <- future_lapply(unique_indiretos, function(cod) {
        if (!is.na(cod) && cod != 0 && nchar(as.character(cod)) > 0) {
          res <- get_sicar_data(cod)
          if (!is.null(res) && nrow(res) > 0) {
            tibble(
              CAR = cod,
              lat_indireto = res$centroideY,
              long_indireto = res$centroideX
            )
          } else {
            tibble(
              CAR = cod,
              lat_indireto = 0,
              long_indireto = 0
            )
          }
        } else {
          tibble(
            CAR = cod,
            lat_indireto = 0,
            long_indireto = 0
          )
        }
      }, future.seed = TRUE)
      
      coords_indiretos <- bind_rows(coords_indiretos)
      basetotal <- left_join(basetotal,
                             coords_indiretos,
                             by = c("CAR_Fornecedor_Indireto" = "CAR"))
      
      # Obtenção das coordenadas para fornecedores diretos
      unique_diretos <- unique(basetotal$CAR_Fornecedor_Direto)
      coords_diretos <- future_lapply(unique_diretos, function(cod) {
        if (!is.na(cod) && nchar(as.character(cod)) > 0) {
          res <- get_sicar_data(cod)
          if (!is.null(res) && nrow(res) > 0) {
            tibble(
              CAR = cod,
              lat_direto = res$centroideY,
              long_direto = res$centroideX
            )
          } else {
            tibble(CAR = cod,
                   lat_direto = NA_real_,
                   long_direto = NA_real_)
          }
        } else {
          tibble(CAR = cod,
                 lat_direto = NA_real_,
                 long_direto = NA_real_)
        }
      }, future.seed = TRUE)
      
      coords_diretos <- bind_rows(coords_diretos)
      basetotal <- left_join(
        basetotal,
        coords_diretos,
        by = c("CAR_Fornecedor_Direto" = "CAR"),
        suffix = c("", ".dir")
      )
      
      basetotalf <- basetotal
      
      basetotalf2 <- rbind(basetotalf2, basetotalf)
      
      data_map <- basetotalf %>%
        mutate(
          CarNumberInDireto = CAR_Fornecedor_Indireto %in% car_number,
          icon_url_indireto = case_when(
            !OCIrregular & CarNumberInDireto ~ "Blue_251A76 (1).png",
            OCIrregular & CarNumberInDireto ~ "Violet.png",
            !OCIrregular & !CarNumberInDireto ~ "Headquarters_4th_Light_Horse_Brigade.png",
            OCIrregular & !CarNumberInDireto ~ "Very_Red_Triangle.png"
          ),
          icon_url_direto = "Pan_Blue_Circle.png",
          group_name = NA_character_
        )
      
      if (sum(is.na(data_map$CAR_Fornecedor_Indireto)) == 0 &&
          !is.numeric(data_map$CAR_Fornecedor_Indireto[1])) {
        
        data_map <- data_map %>%
          mutate(
            CarNumberInDireto = CAR_Fornecedor_Indireto %in% CAR_Fornecedor_Direto,
            normalized_transaction_count = 1 + 4 * (Numero_Transacoes - min(Numero_Transacoes, na.rm = TRUE)) /
              (max(Numero_Transacoes, na.rm = TRUE) - min(Numero_Transacoes, na.rm = TRUE))
          ) %>%
          filter(lat_indireto != 0)
        
        lines_by_cluster <- data_map %>%
          filter(!is.na(CAR_Fornecedor_Direto)) %>%
          group_by(CAR_Fornecedor_Direto) %>%
          group_split() %>%
          map(function(cluster_data) {
            cluster_data <- cluster_data %>%
              filter(!is.na(long_direto),
                     !is.na(lat_direto),
                     !is.na(long_indireto),
                     !is.na(lat_indireto))
            if (nrow(cluster_data) == 0)
              return(NULL)
            do.call(rbind, lapply(1:nrow(cluster_data), function(i) {
              st_sf(
                geometry = st_sfc(st_linestring(matrix(
                  c(
                    cluster_data$long_direto[i],
                    cluster_data$lat_direto[i],
                    cluster_data$long_indireto[i],
                    cluster_data$lat_indireto[i]
                  ),
                  ncol = 2,
                  byrow = TRUE
                ))),
                Numero_Transacoes = cluster_data$Numero_Transacoes[i],
                normalized_transaction_count = cluster_data$normalized_transaction_count[i],
                CAR_Fornecedor_Direto = cluster_data$CAR_Fornecedor_Direto[i],
                crs = st_crs(4326)
              )
            }))
          }) %>% keep(~ !is.null(.x))
        
        for (dir in unique(data_map$CAR_Fornecedor_Direto)) {
          data_direto <- data_map %>% filter(CAR_Fornecedor_Direto == dir)
          data_indireto <- data_map %>% filter(CAR_Fornecedor_Direto == dir) %>%
            mutate(DeforestationArea_df = lapply(DeforestationArea, extract_Prodes))
          
          cluster_data <- lines_by_cluster[[which(sapply(lines_by_cluster, function(x)
            x$CAR_Fornecedor_Direto[1]) == dir)]]
          OCirregular_in_group <- any(data_indireto$OCIrregular == TRUE)
          group_name <- ifelse(OCirregular_in_group, paste("[IRREGULAR]", dir), dir)
          data_map$group_name[data_map$CAR_Fornecedor_Direto == dir] <- group_name
          
          map_final <- map_final %>%
            addMarkers(
              data = data_direto,
              lng = ~ long_direto,
              lat = ~ lat_direto,
              icon = icons(
                iconUrl = ~ icon_url_direto,
                iconWidth = 25,
                iconHeight = 25
              ),
              popup = ~ paste(
                "<b>CAR Direto:</b>", dir, "<br>",
                "<b>Área da Fazenda (Ha):</b>", Faz$area[1], "<br>",
                "<b>Número de Fornecedores Indiretos:</b>", nrow(fornecedores_df), "<br>"
              ),
              group = group_name
            ) %>%
            addMarkers(
              data = data_indireto,
              lng = ~ long_indireto,
              lat = ~ lat_indireto,
              icon = icons(
                iconUrl = ~ icon_url_indireto,
                iconWidth = 25,
                iconHeight = 25
              ),
              popup = ~ paste(
                "<b>CAR Fornecedor Indireto:</b>", CAR_Fornecedor_Indireto, "<br>",
                "<b>Irregular:</b>", ifelse(OCIrregular, "Sim", "Não"),"<br>",
                "<b>Embargo:</b>", ifelse(Embargo, "Sim", "Não"),"<br>",
                "<b>Sobreposição com TI:</b>", ifelse(territorio_indigena, "Sim", "Não"), "<br>",
                "<b>Lista suja:</b>", ifelse(ForcedLabor, "Sim", "Não"), "<br>",
                "<b>Sobreposição com UC:</b>", ifelse(Unidade_Conservacao, "Sim", "Não"), "<br>",
                "<b>Prodes Área (Ha):</b>", mapply(function(df) paste(round(df$Ha, 2), collapse = " - "), DeforestationArea_df), "<br>",
                "<b>Prodes Ano:</b>", mapply(function(df) paste(df$Year, collapse = " - "), DeforestationArea_df), "<br>",
                "<b>Última Transação:</b>", format(as.Date(Ultima_transacao, format="%Y-%m-%dT%H:%M:%S"), "%d/%m/%Y") 
              ),
              group = group_name
            )
          
          if (!is.null(cluster_data)) {
            map_final <- map_final %>%
              addPolylines(
                data = cluster_data,
                color = "black",
                weight = ~ normalized_transaction_count,
                popup = ~ paste(
                  "<b>CAR Direto:</b>", dir, "<br>",
                  "<b>Número de Transações:</b>", Numero_Transacoes
                ),
                group = group_name
              )
          }
        }
        nome_grupo <- c(nome_grupo, data_map$group_name)
        
      } else {
        data_direto <- data_map %>% filter(CAR_Fornecedor_Direto == CAR_Fornecedor_Direto)
        map_final <- map_final %>%
          addMarkers(
            data = data_direto,
            lng = ~ long_direto,
            lat = ~ lat_direto,
            icon = icons(
              iconUrl = ~ icon_url_direto,
              iconWidth = 25,
              iconHeight = 25
            ),
            popup = ~ paste(
              "<b>CAR Direto:</b>", Faz$codigo[ll], "<br>",
              "<b>Área da Fazenda (Ha):</b>", Faz$area[ll], "<br>",
              "<b>Número de Fornecedores Indiretos:</b>", "0", "<br>"
            ),
            group = Faz$codigo[ll]
          )
        
        nome_grupo <- c(nome_grupo, Faz$codigo[ll])
      }
    } else {
      
      nome_grupo <- c(nome_grupo, Faz$codigo[ll])
      
      map_final <- map_final %>%
        addMarkers(
          data = Faz[ll,],
          lng = ~ centroideX,
          lat = ~ centroideY,
          icon = icons(
            iconUrl = "Eo_circle_grey_blank.svg.png",
            iconWidth = 25,
            iconHeight = 25
          ),
          popup = ~ paste(
            "<b>CAR Direto:</b>", Faz$codigo[ll], "<br>",
            "<b>Área da Fazenda (Ha):</b>", Faz$area[ll], "<br>",
            "<b>Número de Fornecedores Indiretos:</b>", "Não encontrado - Dados indisponíveis", "<br>"
          ),
          group = Faz$codigo[ll]
        )
      
      b <- data.frame(matrix("Não encontrado", ncol = 20))
      colnames(b) <- colunaBasef
      b$CAR_Fornecedor_Direto <- Faz$codigo[ll]
      b$lat_direto <- Faz$centroideY[ll]
      b$long_direto <- Faz$centroideX[ll]
      
      basetotalf2 <- rbind(basetotalf2, b)
      
    }
  }
  
  # Marcadores fixos adicionais
  map_final <- map_final %>%
    addMarkers(
      lng = -48.624285,
      lat = -6.359516,
      icon = makeIcon(
        iconUrl = "Logo-Masterboi.png",
        iconWidth = 35,
        iconHeight = 25
      )
    ) %>%
    addMarkers(
      lng = -48.423989,
      lat = -7.603891,
      icon = makeIcon(
        iconUrl = "Logo-Masterboi.png",
        iconWidth = 35,
        iconHeight = 25
      )
    )
  
  unique_groups <- sort(unique(na.omit(nome_grupo)))
  
  map_final <- map_final %>%
    addLayersControl(
      overlayGroups = unique_groups,
      options = layersControlOptions(collapsed = TRUE)
    )
  attr(map_final, "groups") <- unique_groups
  
  #### BLOCO DE ANALISE #####
  base <- basetotalf2[-1,]
  
  PRODES <- base %>%
    mutate(
      DeforestationArea_df = map(DeforestationArea, ~ {
        if (!is.null(.x) && length(.x) > 0 && !all(is.na(.x))) {
          result <- extract_Prodes(.x)
          if (!is.data.frame(result)) result <- data.frame(Ha = NA_real_, Year = NA_integer_)
          result
        } else {
          data.frame(Ha = NA_real_, Year = NA_integer_)
        }
      }),
      DeforestationArea_Ha = map_chr(DeforestationArea_df, ~ {
        if (!is.null(.x) && nrow(.x) > 0 && "Ha" %in% names(.x) && is.numeric(.x$Ha)) {
          paste(round(.x$Ha, 2), collapse = ", ")
        } else {
          "NA"
        }
      }),
      DeforestationArea_Ano = map_chr(DeforestationArea_df, ~ {
        if (!is.null(.x) && nrow(.x) > 0 && "Year" %in% names(.x)) {
          paste(.x$Year, collapse = ", ")
        } else {
          "NA"
        }
      })
    )
  
  base <- cbind(base, PRODES[,c(22,23)])[,-16]
  
  Num_de_fornecedores_indiretos <- base %>%
    group_by(CAR_Fornecedor_Direto) %>%
    summarise(
      Número_de_Indiretos = if (any(Estado == "Não encontrado", na.rm = TRUE)) {
        "Não encontrado"
      } else {
        as.character(length(na.omit(lat_indireto)))
      },
      Indiretos_Irregulares_OC = sum(as.logical(OCIrregular)),
      Indiretos_Irregulares_BP = sum(as.logical(BPIrregular)),
      .groups = "drop"
    ) %>%
    tibble()
  
  Num_de_fornecedores_Sem_indireto <- base %>%
    group_by(CAR_Fornecedor_Direto) %>%
    filter((is.na(Ultima_transacao))) %>%
    select(CAR_Fornecedor_Direto) %>% tibble()
  
  Num_de_fornecedores_Não_encontratos <- base %>%
    group_by(CAR_Fornecedor_Direto) %>%
    filter(Ultima_transacao == 'Não encontrado') %>%
    select(CAR_Fornecedor_Direto) %>% tibble()
  
  base$PRODES <- ifelse(!base$DeforestationArea_Ha == 'NA', TRUE, FALSE)
  
  Sumario_vinculos_OC <- base %>% 
    filter(!is.na(Ultima_transacao)) %>%  
    select(all_of(colnames(base)[c(9,11:14, 22)])) %>% 
    mutate(across(everything(), as.logical)) %>% 
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    tibble()
  
  percent_row_OC <- Sumario_vinculos_OC / Sumario_vinculos_OC[[1]] * 100
  Sumario_vinculos_OC <- bind_rows(Sumario_vinculos_OC, percent_row_OC) %>% 
    mutate(Row = c("Valores Absolutos", "Porcentagem"))
  Sumario_vinculos_OC <- Sumario_vinculos_OC %>% select(Row, everything())
  
  Sumario_vinculos_BP <- base %>% 
    filter(!is.na(Ultima_transacao)) %>%  
    select(all_of(colnames(base)[c(10:14, 22)])) %>% 
    mutate(across(everything(), as.logical)) %>% 
    summarise(across(everything(), sum, na.rm = TRUE)) %>% 
    tibble()
  
  percent_row_BP <- Sumario_vinculos_BP / Sumario_vinculos_BP[[1]] * 100
  Sumario_vinculos_BP <- bind_rows(Sumario_vinculos_BP, percent_row_BP) %>% 
    mutate(Row = c("Valores Absolutos", "Porcentagem"))
  Sumario_vinculos_BP <- Sumario_vinculos_BP %>% select(Row, everything())
  
  sumario_CAR_BP <- base %>% 
    filter(BPIrregular == TRUE) %>% 
    group_by(CAR_Fornecedor_Indireto, Codigo_de_estabelecimento_Federal_Indireto, Codigo_de_estabelecimento_Estadual_Indireto) %>% 
    summarise(count = n(), .groups = "drop") %>% 
    arrange(desc(count))
  
  sumario_CAR_OC <- base %>% 
    filter(OCIrregular == TRUE) %>% 
    group_by(CAR_Fornecedor_Indireto, Codigo_de_estabelecimento_Federal_Indireto, Codigo_de_estabelecimento_Estadual_Indireto) %>% 
    summarise(count = n(), .groups = "drop") %>% 
    arrange(desc(count))
  
  CAR_Prodes <- base %>% 
    select(CAR_Fornecedor_Indireto, DeforestationArea_Ano, DeforestationArea_Ha) %>% 
    separate_rows(DeforestationArea_Ano, DeforestationArea_Ha, sep = ",\\s*") %>% filter(!is.na(DeforestationArea_Ano)) %>% 
    arrange(desc(CAR_Fornecedor_Indireto))
  
  sumario_prodes_ano <- table(CAR_Prodes$DeforestationArea_Ano, exclude = 'NA')
  
  lista_suja <- base %>% 
    filter(ForcedLabor == TRUE) %>% 
    select(CAR_Fornecedor_Direto, CAR_Fornecedor_Indireto, Codigo_de_estabelecimento_Estadual_Indireto) %>% tibble()
  
  sobreposicao_UC <- base %>% 
    filter(Unidade_Conservacao == TRUE) %>% 
    select(CAR_Fornecedor_Direto, CAR_Fornecedor_Indireto, Codigo_de_estabelecimento_Estadual_Indireto) %>% tibble()
  
  sobreposicao_TI <- base %>% 
    filter(territorio_indigena == TRUE) %>% 
    select(CAR_Fornecedor_Direto, CAR_Fornecedor_Indireto, Codigo_de_estabelecimento_Estadual_Indireto) %>% tibble()
  
  SUMARIO_DIRETOS_COM_INDIRETOS_OC <- base %>%
    group_by(CAR_Fornecedor_Direto) %>%
    filter(!is.na(CAR_Fornecedor_Indireto) & OCIrregular == TRUE) %>% 
    summarise(count = n()) %>% 
    arrange(desc(count))
  
  SUMARIO_DIRETOS_COM_INDIRETOS_BP <- base %>%
    group_by(CAR_Fornecedor_Direto) %>%
    filter(!is.na(CAR_Fornecedor_Indireto) & BPIrregular == TRUE) %>% 
    summarise(count = n()) %>% 
    arrange(desc(count))
  
  return(list(
    map = map_final,
    Num_de_fornecedores_indiretos = Num_de_fornecedores_indiretos,
    Sumario_vinculos_OC = Sumario_vinculos_OC,
    Sumario_vinculos_BP = Sumario_vinculos_BP,
    sumario_CAR_BP = sumario_CAR_BP,
    sumario_CAR_OC = sumario_CAR_OC,
    sumario_prodes_ano = sumario_prodes_ano,
    lista_suja = lista_suja,
    dataf = base,
    sobreposicao_TI = sobreposicao_TI,
    sobreposicao_UC = sobreposicao_UC
  ))
}

#### UI #######
ui <- fluidPage(
  useShinyjs(),
  tags$script(HTML("
        $(document).ready(function(){
          $('#car_input').on('input', function(){
            let lines = $(this).val().split('\\n').length;
            $(this).attr('rows', Math.min(Math.max(2, lines), 10));
          });
        });
        ")),
  tags$style(HTML("
  
  .shiny-notification {
      #position: fixed;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
      width: 400px;
      font-size: 20px;
    }
  
    .invalid-car { background-color: #ffcccc !important; }
    #map-container { position: relative; }
    #loading-overlay {
      position: absolute;
      top: 0;
      left: 0;
      right: 0;
      bottom: 0;
      background: rgba(255,255,255,0.8);
      z-index: 1000;
      display: none;
    }
    #loading-overlay > div {
      position: absolute;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
      text-align: center;
    }
    
    #car_input {
    height: 50px;
    resize: none;
    
    #car_file {
    height: 65px !important;  /* Altura fixa para o input */
    overflow: hidden;
  }
  .shiny-file-input-progress {
    height: 20px !important;  /* Altura fixa para a barra de progresso */
    overflow: hidden;
  }
  .shiny-file-input {
    white-space: nowrap;  /* Impede que o nome do arquivo quebre linhas */
    text-overflow: ellipsis;  /* Evita que o texto se expanda */
  }
    
    #car_file_progress {
    height: 30px;   /* Altura fixa para a barra de progresso */
    overflow: hidden;  /* Evita que o conteúdo expanda a altura */
    text-overflow: ellipsis; /* Garante que o texto fique dentro da área */
    white-space: nowrap; /* Impede que o texto quebre linhas */
    }
  
  /* Contêiner do contador de CARs */
    #car_count {
        height: 35px !important;      /* Altura fixa */
        min-height: 35px !important;
        max-height: 35px !important;
        overflow-y: hidden !important; /* Esconde overflow vertical */
        margin-bottom: 10px !important;
    }
    
    /* Texto dentro do contêiner */
    #car_count.shiny-text-output {
        white-space: nowrap !important;  /* Impede quebra de linha */
        overflow: hidden !important;
        text-overflow: ellipsis !important; /* (...) se texto longo */
        line-height: 35px !important;     /* Centraliza verticalmente */
    }
  
  ")),
  titlePanel("Monitoramento de Indiretos"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        div(style = "display: flex; align-items: center;",
            div(style = "width: 360px; margin-left: 15px;",  
                fileInput("car_file", "Lista de CARs (.csv, .xlsx ou .txt)", 
                          accept = c(".csv", ".xlsx", ".xls", ".txt"))),
            div(style = "margin-left: 10px; margin-top: -15px;",
                actionButton("clear_file", "✖", class = "btn btn-danger", 
                             style = "width: 50px; height: 33px; font-size: 15px;"))
        )
      ),
      div(style = "margin-top: -10px;",
          textAreaInput("car_input", "Ou digite os CARs:", rows = 2)),
      div(style = "height: 30px; display: flex; align-items: center;",
          verbatimTextOutput("car_count")),
      fluidRow(
        column(6, div(style = "margin-top: 10px;",
                      dateInput("start_date", "Data Inicial", value = Sys.Date()))),
        column(6, div(style = "margin-top: 10px;", 
                      dateInput("end_date", "Data Final", value = Sys.Date())))
      ),
      htmlOutput("msg_periodo"),
      fluidRow(
        column(6, div(style = "margin-top: 15px;",
                      actionButton("update", "Atualizar Mapa"))),
        column(6, div(style = "position: relative; left: -90px; margin-top: 15px;",
                      actionButton("toggle_groups", "Mostrar/Esconder Marcadores")))
      ),
      div(style = "display: flex; justify-content: center; align-items: center; margin-top: 30px;",
          img(src = "https://www.santacruzpe.com.br/wp-content/uploads/2018/03/Logo-Masterboi.png", 
              style = "height: 80px; margin-right: 40px; margin-bottom: -15px;"),
          img(src = "logo1.png", 
              style = "height: 80px; margin-left: 40px; margin-bottom: -15px;")
      ),
      br(),
      hr(),
      div(style = "display: flex; justify-content: space-between; align-items: center; margin-top: 10px;",
          div(style = "text-align: center;",
              div(style = "font-weight: bold;", "Base de dados:"),
              div(style = "margin-top: 5px;",
                  img(src = "Visipec_logo_PT__1_-removebg-preview.png", style = "height: 56px; margin-right: 10px;"),
                  img(src = "logo_car.png", style = "height: 28px; margin-top: 10px;")
              )
          ),
          div(style = "text-align: center;",
              div(style = "font-weight: bold;", "Apoio e Agradecimentos:"),
              div(style = "margin-top: 5px;",
                  img(src = "NWF_Primary_H_2C-removebg-preview.png", style = "height: 42px; margin-right: 10px;"),
                  img(src = "LogoGTFI-removebg-preview.png", style = "height: 49px;")
              )
          )
      )
    ),
    mainPanel(
      div(id = "map-container",
          leafletOutput("mapa", height = "716px"),
          div(id = "loading-overlay", 
              div(
                h3("Carregando..."),
                icon("spinner", class = "fa-spin fa-3x")
              )
          )
      )
    )
  ),
  hr(),
  fluidRow(
    div(id = "resultados", 
        column(12,
               tabsetPanel(
                 tabPanel("Resumo Fornecedores",
                          DT::dataTableOutput("tableFornecedores")
                 ),
                 tabPanel("Sumário Vínculos OC",
                          DT::dataTableOutput("tableVinculosOC")
                 ),
                 tabPanel("Sumário Vínculos BP",
                          DT::dataTableOutput("tableVinculosBP")
                 ),
                 tabPanel("Lista Propriedades BP",
                          DT::dataTableOutput("tableListaBP")
                 ),
                 tabPanel("Lista Propriedades OC",
                          DT::dataTableOutput("tableListaOC")
                 ),
                 tabPanel("Sumário Desmatamento",
                          DT::dataTableOutput("tableProdes")
                 ),
                 tabPanel("Lista Suja",
                          DT::dataTableOutput("tableListaSuja")),
                 tabPanel("Sobreposição UC",
                          DT::dataTableOutput("tableSobreUC")),
                 tabPanel("Sobreposição TI",
                          DT::dataTableOutput("tableSobreTI"))
               )
        )
    )
  )
)

#### SERVER #####
server <- function(input, output, session) {
  
  # Variáveis para controle do mapa
  map_groups <- reactiveVal(NULL)
  groupsVisible <- reactiveVal(TRUE)
  
  # Variável reativa para sinalizar se o arquivo foi limpo
  arquivo_limpo <- reactiveVal(FALSE)
  
  observeEvent(input$clear_file, {
    shinyjs::reset("car_file")
    updateTextAreaInput(session, "car_input", value = "")
    arquivo_limpo(TRUE)
  })
  
  observeEvent(input$car_file, {
    if (!is.null(input$car_file) && input$car_file$datapath != "") {
      arquivo_limpo(FALSE)
    }
  })
  
  # Função auxiliar para extrair o texto (arquivo ou campo de texto)
  extrair_texto <- function(texto) {
    car_pattern <- "(?i)\\b([A-Z]{2})\\s*-\\s*(\\d{7})\\s*-\\s*([A-F0-9\\.\\s]+)\\b"
    matches <- stringr::str_match_all(texto, car_pattern)[[1]]
    if(nrow(matches) > 0) {
      car_list <- apply(matches, 1, function(x) {
        uf       <- x[2]
        ibge     <- x[3]
        hash_raw <- x[4]
        hash_clean <- toupper(gsub("[^A-F0-9]", "", hash_raw))
        if(nchar(hash_clean) == 32) {
          paste0(uf, "-", ibge, "-", hash_clean)
        } else {
          NA
        }
      })
      car_list <- na.omit(car_list)
      return(car_list)
    }
    return(character(0))
  }
  
  extrair_cars <- reactive({
    if (arquivo_limpo() || is.null(input$car_file) || input$car_file$datapath == "") {
      req(input$car_input)
      return(extrair_texto(input$car_input))
    }
    
    file <- input$car_file$datapath
    ext <- tools::file_ext(input$car_file$name)
    if (ext == "csv") {
      df <- read.csv(file, stringsAsFactors = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      df <- read_excel(file)
    } else if (ext == "txt") {
      df <- read.delim(file, header = FALSE, stringsAsFactors = FALSE)
    } else {
      showNotification("Formato de arquivo não suportado.", type = "error")
      return(character(0))
    }
    texto <- paste(as.character(df[[1]]), collapse = " ")
    car_list_file <- extrair_texto(texto)
    if (length(car_list_file) > 0) return(car_list_file)
    
    req(input$car_input)
    extrair_texto(input$car_input)
  })
  
  output$car_count <- renderText({
    car_list <- extrair_cars()
    paste("CARs reconhecidos:", length(car_list), "/ 50")
  })
  
  output$msg_periodo <- renderText({
    req(input$start_date)
    data_inicial <- as.Date(input$start_date) - 730
    data_final <- data_inicial + 365
    data_inicio_periodo <- format(data_inicial, "%d/%m/%Y")
    data_fim_periodo <- format(data_final, "%d/%m/%Y")
    
    HTML(paste0("A busca por fornecedores indiretos é definida a partir da data inicial de busca. 
Atualmente sua busca está definida para abranger o período de: <b>",
                data_inicio_periodo, "</b> a <b>", data_fim_periodo, "</b>." ))
  })
  
  # Variável reativa para armazenar o token autenticado
  tokenVisipec <- reactiveVal(NULL)
  
  # Exibe modal de login ao iniciar o app
  showModal(modalDialog(
    title = "Autenticação VISIPEC",
    textInput("login_email", "E-mail:", placeholder = "Digite seu e-mail VISIPEC"),
    passwordInput("login_senha", "Senha:", placeholder = "Digite sua senha VISIPEC"),
    footer = tagList(
      #modalButton("Fechar"),
      actionButton("btn_login", "Entrar")
    ),
    easyClose = FALSE,
    size = "s"
  ))
  
  observeEvent(input$btn_login, {
    req(input$login_email, input$login_senha)
    tryCatch({
      token <- get_token_visipec(input$login_email, input$login_senha)
      tokenVisipec(token)
      removeModal()
      showNotification("Autenticação realizada com sucesso!", type = "message")
    },
    error = function(e) {
      showNotification(paste("Erro:", e$message), type = "error")
    })
  })
  
  observeEvent(input$update, {
    req(tokenVisipec())
    req(input$start_date, input$end_date)
    
    car_list <- extrair_cars()
    if (length(car_list) == 0) {
      showNotification("Nenhum CAR válido encontrado.", type = "error")
      return()
    }
    
    if (length(car_list) > 50) {
      showNotification("Você informou mais de 50 CARs. Por favor, informe no máximo 50.", type = "error")
      return()
    }
    
    shinyjs::show("loading-overlay")
    withProgress(message = "Processando busca, aguarde...", value = 0, {
      incProgress(0.3)
      resultados <- gerar_mapa_final(
        car_number = car_list,
        token_visipec = tokenVisipec(),
        data_visipec_inicial = input$start_date,
        data_visipec_final = input$end_date
      )
      incProgress(0.7)
      output$mapa <- renderLeaflet({ resultados$map })
      map_groups(attr(resultados$map, "groups"))
    })
    shinyjs::hide("loading-overlay")
    
    output$tableFornecedores <- DT::renderDataTable({ resultados$Num_de_fornecedores_indiretos })
    output$tableVinculosOC <- DT::renderDataTable({ resultados$Sumario_vinculos_OC })
    output$tableVinculosBP <- DT::renderDataTable({ resultados$Sumario_vinculos_BP })
    output$tableListaBP <- DT::renderDataTable({ resultados$sumario_CAR_BP })
    output$tableListaOC <- DT::renderDataTable({ resultados$sumario_CAR_OC })
    output$tableProdes <- DT::renderDataTable({ t(as.data.frame(resultados$sumario_prodes_ano, row.names = 1)) })
    output$tableListaSuja <- DT::renderDataTable({ resultados$lista_suja })
    output$tableSobreUC <- DT::renderDataTable({ resultados$sobreposicao_UC })
    output$tableSobreTI <- DT::renderDataTable({ resultados$sobreposicao_TI })
    
    runjs("window.scrollTo({ top: document.getElementById('resultados').offsetTop + 800, behavior: 'smooth' });")
  })
  
  observeEvent(input$toggle_groups, {
    req(map_groups())
    if (groupsVisible()) {
      for (grp in map_groups()) {
        leafletProxy("mapa") %>% hideGroup(grp)
      }
      groupsVisible(FALSE)
    } else {
      for (grp in map_groups()) {
        leafletProxy("mapa") %>% showGroup(grp)
      }
      groupsVisible(TRUE)
    }
  })
}

shinyApp(ui, server)
