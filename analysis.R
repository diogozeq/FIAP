# farmtech_analytics.R
# =============================================================================
# FarmTech Analytics - Sistema de Análise Estatística para a FarmTech Solutions
# Aplicando Melhores Práticas de UX e Arquitetura de Software (Padrão MVC)
# + Integração com API Meteorológica (CPTEC/INPE)
# =============================================================================

# =============================================================================
# MÓDULO 1: CONFIGURAÇÕES E INICIALIZAÇÃO
# =============================================================================

# Ambiente de configuração global (padrão Singleton)
CONFIG <- new.env()
CONFIG$results_dir <- getwd()
CONFIG$default_encoding <- c("UTF-8", "latin1", "CP1252", "macintosh")
CONFIG$weather_api_url <- "http://servicos.cptec.inpe.br/XML/cidade/241/previsao.xml"  # API do CPTEC/INPE

# Função para impressão colorida (feedback visual aprimorado)
print_colored <- function(message, type = c("info", "success", "error", "warning")) {
  type <- match.arg(type)
  color <- switch(type,
                  "info" = "\033[0;36m",    # Cyan
                  "success" = "\033[0;32m", # Verde
                  "error" = "\033[0;31m",   # Vermelho
                  "warning" = "\033[0;33m") # Amarelo
  cat(color, message, "\033[0m\n", sep = "")
}


# =============================================================================
# MÓDULO 2: UI (INTERFACE DO USUÁRIO)
# =============================================================================

# Exibe o banner principal
print_banner <- function() {
  cat("\033[1;36m")
  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║           ANÁLISE DE DADOS AGRÍCOLAS EM R - FARMTECH           ║\n")
  cat("║    + INTEGRAÇÃO COM API METEOROLÓGICA (CPTEC/INPE)     ║\n") # Banner atualizado
  cat("║                  Soluções para a Agricultura Digital         ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n")
  cat("\033[0m")
}

# Exibe o menu principal e retorna a opção escolhida
show_main_menu <- function() {
  cat("\n\033[1;32m=== MENU PRINCIPAL ===\033[0m\n")
  cat("1. Carregar dados (CSV)\n")
  cat("2. Gerar dados fictícios\n")
  cat("3. Consultar dados meteorológicos (CPTEC/INPE)\n") # Nova opção
  cat("4. Sair\n")                                          # Opção de sair atualizada
  choice <- as.integer(readline("Escolha uma opção: "))
  if (is.na(choice) || choice < 1 || choice > 4) {          # Verificação atualizada
    print_colored("Opção inválida. Por favor, escolha entre 1 e 4.", "error")
    return(NULL)
  }
  return(choice)
}


# Exibe o menu de ações (após carregar ou gerar dados) e retorna a opção escolhida
show_action_menu <- function() {
  cat("\n\033[1;32m=== MENU DE AÇÕES ===\033[0m\n")
  cat("1. Exibir estatísticas e análise de regressão\n")
  cat("2. Mostrar gráficos em ASCII\n")
  cat("3. Gerar relatório em tela (ggplot2)\n")
  cat("4. Gerar relatório HTML\n")
  cat("5. Voltar ao menu principal\n")
  choice <- as.integer(readline("Escolha uma opção: "))
  if (is.na(choice) || choice < 1 || choice > 5) {
    print_colored("Opção inválida. Por favor, escolha entre 1 e 5.", "error")
    return(NULL)
  }
  return(choice)
}


# Função de barra de progresso no console
display_progress_bar <- function(completed, total, width = 50, label = "Progresso") {
  pct <- completed / total
  filled <- round(width * pct)
  bar <- paste0(label, ": [",
                paste(rep("=", filled), collapse = ""),
                paste(rep(" ", width - filled), collapse = ""),
                "] ", round(pct * 100), "%")
  cat("\r", bar)
  if (completed == total) cat("\n")
}

# Função para simular uma mensagem de carregamento animada
show_loading <- function(message = "Processando", duration = 2) {
  symbols <- c("-", "\\", "|", "/")
  start_time <- Sys.time()
  while (difftime(Sys.time(), start_time, units = "secs") < duration) {
    for (s in symbols) {
      cat("\r", message, " ", s, "  ")
      Sys.sleep(0.1)
    }
  }
  cat("\r", message, " Concluído!       \n")
}


# =============================================================================
# MÓDULO 3: ACESSO A DADOS
# =============================================================================

#' Escolhe um arquivo CSV a partir de um diretório
#' @param dir_path Caminho do diretório
#' @return Caminho completo do arquivo escolhido ou NULL se cancelado
choose_csv_file <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    print_colored(paste("Diretório não encontrado:", dir_path), "error")
    return(NULL)
  }
  files <- list.files(path = dir_path, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0) {
    print_colored(paste("Nenhum arquivo CSV encontrado em", dir_path), "warning")
    return(NULL)
  }
  print_colored("Arquivos CSV encontrados:", "warning")
  for (i in seq_along(files)) {
    cat(sprintf("%d. %s\n", i, basename(files[i])))
  }
  option <- as.integer(readline("Escolha um arquivo (ou 0 para cancelar): "))
  if (is.na(option) || option < 0 || option > length(files)) {
    print_colored("Opção inválida.", "error")
    return(NULL)
  }
  if (option == 0) {
    print_colored("Seleção de arquivo cancelada.", "warning")
    return(NULL)
  }
  return(files[option])
}

#' Lê um arquivo CSV de forma flexível e robusta
#' @param file_path Caminho do arquivo CSV
#' @return Data frame com os dados ou NULL em caso de erro
read_csv_flexible <- function(file_path) {
  tryCatch({
    # Primeiro, verificamos se o arquivo existe
    if (!file.exists(file_path)) {
      print_colored(paste("Arquivo não encontrado:", file_path), "error")
      return(NULL)
    }
    
    # Leia as primeiras linhas para análise
    con <- file(file_path, "r")
    first_lines <- readLines(con, n = 10, warn = FALSE)
    close(con)
    
    if (length(first_lines) == 0) {
      print_colored("O arquivo está vazio.", "error")
      return(NULL)
    }
    
    # Tente detectar o separador mais provável
    separators <- c(",", ";", "\t", "|")
    sep_counts <- sapply(separators, function(sep) {
      mean(sapply(first_lines, function(line) {
        sum(gregexpr(sep, line, fixed = TRUE)[[1]] > 0)
      }))
    })
    
    # Se não encontrou separadores claros, use vírgula como padrão
    if (all(sep_counts == 0)) {
      probable_sep <- ","
    } else {
      probable_sep <- separators[which.max(sep_counts)]
    }
    
    # Verifique o header
    headers <- unlist(strsplit(first_lines[1], probable_sep, fixed = TRUE))
    headers <- trimws(headers)
    
    # Tente diferentes configurações para ler o CSV corretamente
    df <- NULL
    
    # Abordagem 1: Usar read.csv diretamente com diferentes encodings
    for (enc in CONFIG$default_encoding) {
      tryCatch({
        temp_df <- read.csv(file_path, 
                           header = TRUE,
                           sep = probable_sep,
                           fileEncoding = enc,
                           stringsAsFactors = FALSE,
                           check.names = FALSE,
                           strip.white = TRUE)
        
        # Se conseguimos ler mais de uma coluna, provavelmente está correto
        if (ncol(temp_df) > 1) {
          df <- temp_df
          print_colored(paste("Arquivo lido com sucesso usando encoding:", enc), "success")
          break
        }
      }, error = function(e) {
        # Silenciosamente ignore erros e tente o próximo encoding
      })
      
      if (!is.null(df)) break
    }
    
    # Abordagem 2: Se a abordagem 1 falhar, use utils::read.table com mais opções
    if (is.null(df)) {
      for (enc in CONFIG$default_encoding) {
        tryCatch({
          temp_df <- utils::read.table(file_path,
                                      header = TRUE,
                                      sep = probable_sep,
                                      quote = "\"",
                                      fileEncoding = enc,
                                      na.strings = c("", "NA", "N/A", "NULL"),
                                      fill = TRUE,
                                      strip.white = TRUE,
                                      check.names = FALSE)
          
          if (ncol(temp_df) > 1) {
            df <- temp_df
            print_colored(paste("Arquivo lido com sucesso usando read.table e encoding:", enc), "success")
            break
          }
        }, error = function(e) {
          # Ignore erros silenciosamente
        })
        
        if (!is.null(df)) break
      }
    }
    
    # Abordagem 3: Leitura manual linha por linha (último recurso)
    if (is.null(df)) {
      print_colored("Tentando leitura manual do arquivo...", "warning")
      
      # Tente identificar os nomes das colunas
      header_line <- first_lines[1]
      col_names <- unlist(strsplit(header_line, probable_sep, fixed = TRUE))
      col_names <- trimws(col_names)
      
      # Prepare um data frame vazio com esses nomes de colunas
      df <- data.frame(matrix(ncol = length(col_names), nrow = 0))
      names(df) <- col_names
      
      # Leia o resto das linhas
      for (i in 2:length(first_lines)) {
        line <- first_lines[i]
        if (nchar(trimws(line)) == 0) next  # Pule linhas vazias
        
        # Divida a linha pelo separador
        values <- unlist(strsplit(line, probable_sep, fixed = TRUE))
        values <- trimws(values)
        
        # Se o número de valores não corresponder ao número de colunas, ajuste
        if (length(values) < length(col_names)) {
          values <- c(values, rep(NA, length(col_names) - length(values)))
        } else if (length(values) > length(col_names)) {
          values <- values[1:length(col_names)]
        }
        
        # Adicione a linha ao data frame
        df[i-1, ] <- values
      }
      
      # Leia o restante do arquivo
      con <- file(file_path, "r")
      all_lines <- readLines(con)
      close(con)
      
      if (length(all_lines) > length(first_lines)) {
        for (i in (length(first_lines) + 1):length(all_lines)) {
          line <- all_lines[i]
          if (nchar(trimws(line)) == 0) next
          
          values <- unlist(strsplit(line, probable_sep, fixed = TRUE))
          values <- trimws(values)
          
          if (length(values) < length(col_names)) {
            values <- c(values, rep(NA, length(col_names) - length(values)))
          } else if (length(values) > length(col_names)) {
            values <- values[1:length(col_names)]
          }
          
          # Adicione a linha ao data frame
          df[i-1, ] <- values
        }
      }
      
      print_colored("Arquivo lido manualmente com sucesso.", "success")
    }
    
    # Verifica se o DataFrame foi criado corretamente
    if (is.null(df) || ncol(df) <= 1) {
      print_colored("Não foi possível ler o arquivo CSV corretamente.", "error")
      return(NULL)
    }
    
    # Converta tipos de dados apropriadamente
    for (col in names(df)) {
      # Tente converter colunas numéricas
      if (all(grepl("^[\\d.,+-]+$", na.omit(df[[col]])) | is.na(df[[col]]))) {
        # Tente com ponto decimal
        temp <- suppressWarnings(as.numeric(gsub(",", ".", df[[col]])))
        if (!all(is.na(temp) == is.na(df[[col]]))) {
          # Tente com vírgula decimal
          temp <- suppressWarnings(as.numeric(gsub("\\.", "", gsub(",", ".", df[[col]]))))
        }
        if (!all(is.na(temp) == is.na(df[[col]]))) {
          # Se nenhum funcionar, mantenha como está
          next
        }
        df[[col]] <- temp
      }
    }
    
    print_colored(paste("Lendo dados do arquivo:", file_path), "success")
    return(df)
  }, error = function(e) {
    print_colored(paste("Erro ao ler o arquivo:", e$message), "error")
    return(NULL)
  })
}

#' Gera dados fictícios (simulados)
#' @return Data frame com dados simulados
generate_fake_data <- function() {
  print_colored("Gerando dados aleatórios...", "warning")
  set.seed(123)
  cultures <- c(rep("Mandioca", 5), rep("Cana de Açúcar", 5))
  area <- round(runif(10, min = 5, max = 400))
  input <- ifelse(cultures == "Cana de Açúcar", round(area * 0.088, 2), round(area * 0.05, 2))
  estimated_cost <- round(ifelse(cultures == "Cana de Açúcar", area * 13.2, area * 4.5), 2)
  record_date <- as.character(Sys.time() + seq(0, by = 10, length.out = 10))
  df <- data.frame(
    cultura = cultures,
    area = area,
    insumo = input,
    custo_estimado = estimated_cost,
    data_registro = record_date,
    stringsAsFactors = FALSE
  )
  return(df)
}


# =============================================================================
# MÓDULO 4: ANÁLISE ESTATÍSTICA
# =============================================================================

#' Calcula estatísticas descritivas dos dados
#' @param df Data frame com os dados
#' @return NULL (efeito colateral de imprimir resultados)
analyze_statistics <- function(df) {
  print_colored("=== ESTATÍSTICAS DESCRITIVAS ===", "warning")
  if ("Producao" %in% names(df)) {
    print_colored("Estatísticas de Produção (ton)", "info")
    print(summary(df$Producao))
    cat("Desvio Padrão (Produção):", sd(df$Producao, na.rm = TRUE), "\n\n")
  } else if ("area" %in% names(df)) {
    print_colored("Estatísticas de Área", "info")
    print(summary(df$area))
    cat("Desvio Padrão (Área):", sd(df$area, na.rm = TRUE), "\n\n")
  }
  if ("Custo" %in% names(df)) {
    print_colored("Estatísticas de Custo (R$)", "info")
    print(summary(df$Custo))
    cat("Desvio Padrão (Custo):", sd(df$Custo, na.rm = TRUE), "\n")
  } else if ("custo_estimado" %in% names(df)) {
    print_colored("Estatísticas de Custo Estimado (R$)", "info")
    print(summary(df$custo_estimado))
    cat("Desvio Padrão (Custo):", sd(df$custo_estimado, na.rm = TRUE), "\n")
  }
}

#' Analisa os dados por cultura, incluindo regressão linear
#' @param df Data frame com colunas 'cultura', 'area', 'insumo' e opcionalmente 'custo_estimado'
#' @return Data frame com os resultados da análise por cultura
analyze_by_culture <- function(df) {
  # Verificar se as colunas existem, ignorando maiúsculas/minúsculas
  cols <- tolower(names(df))
  req_cols <- c("cultura", "area", "insumo")
  
  # Mapeie os nomes reais das colunas para os nomes esperados (case insensitive)
  col_map <- list()
  for (req in req_cols) {
    idx <- which(cols == req)
    if (length(idx) > 0) {
      col_map[[req]] <- names(df)[idx[1]]
    }
  }
  
  if (length(col_map) < length(req_cols)) {
    print_colored("Os dados não possuem os campos esperados (cultura, área, insumo).", "error")
    print_colored("Colunas disponíveis:", "info")
    print(names(df))
    return(NULL)
  }
  
  print_colored("=== ANÁLISE DE DADOS AGRÍCOLAS ===", "warning")
  results <- data.frame()
  
  # Use os nomes mapeados das colunas
  cultura_col <- col_map[["cultura"]]
  area_col <- col_map[["area"]]
  insumo_col <- col_map[["insumo"]]
  
  # Verificar se a coluna custo_estimado existe
  custo_col <- NULL
  if ("custo_estimado" %in% tolower(names(df))) {
    idx <- which(tolower(names(df)) == "custo_estimado")
    custo_col <- names(df)[idx[1]]
  }
  
  for (cult in unique(df[[cultura_col]])) {
    cult_data <- df[df[[cultura_col]] == cult, ]
    total_records <- nrow(cult_data)
    total_area <- sum(as.numeric(cult_data[[area_col]]), na.rm = TRUE)
    mean_area <- mean(as.numeric(cult_data[[area_col]]), na.rm = TRUE)
    sd_area <- sd(as.numeric(cult_data[[area_col]]), na.rm = TRUE)
    total_input <- sum(as.numeric(cult_data[[insumo_col]]), na.rm = TRUE)
    mean_input <- mean(as.numeric(cult_data[[insumo_col]]), na.rm = TRUE)
    sd_input <- sd(as.numeric(cult_data[[insumo_col]]), na.rm = TRUE)
    
    # Calcular correlação apenas se os dados forem numéricos
    correlation <- NA
    tryCatch({
      correlation <- cor(as.numeric(cult_data[[area_col]]), 
                         as.numeric(cult_data[[insumo_col]]), 
                         use = "complete.obs")
    }, error = function(e) {
      print_colored("Não foi possível calcular a correlação.", "warning")
    })
    
    temp <- data.frame(
      cultura = cult,
      total_registros = total_records,
      area_total = total_area,
      area_media = mean_area,
      area_desvio = sd_area,
      insumo_total = total_input,
      insumo_media = mean_input,
      insumo_desvio = sd_input,
      correlacao = correlation,
      stringsAsFactors = FALSE
    )
    
    if (!is.null(custo_col)) {
      total_cost <- sum(as.numeric(cult_data[[custo_col]]), na.rm = TRUE)
      mean_cost <- mean(as.numeric(cult_data[[custo_col]]), na.rm = TRUE)
      sd_cost <- sd(as.numeric(cult_data[[custo_col]]), na.rm = TRUE)
      temp$custo_total <- total_cost
      temp$custo_medio <- mean_cost
      temp$custo_desvio <- sd_cost
    }
    
    results <- rbind(results, temp)
    print_colored(paste("\n---- Cultura:", cult, "----"), "warning")
    cat("Total de registros: ", total_records, "\n")
    cat("Área Total: ", total_area, " m²\n")
    cat("Média de área: ", round(mean_area, 2), " m²\n")
    cat("Desvio Padrão da área: ", round(sd_area, 2), " m²\n")
    cat("Total de insumos: ", total_input, " litros\n")
    cat("Média de insumos: ", round(mean_input, 2), " litros\n")
    cat("Desvio Padrão dos insumos: ", round(sd_input, 2), " litros\n")
    cat("Correlação área-insumo: ", round(correlation, 2), "\n")
    
    # Regressão Linear: insumo ~ area
    tryCatch({
      # Crie um novo data frame para a regressão com valores numéricos
      reg_data <- data.frame(
        area = as.numeric(cult_data[[area_col]]),
        insumo = as.numeric(cult_data[[insumo_col]])
      )
      
      # Remova linhas com NA
      reg_data <- reg_data[complete.cases(reg_data), ]
      
      if (nrow(reg_data) >= 2) {  # Precisa de pelo menos 2 pontos para regressão
        model <- lm(insumo ~ area, data = reg_data)
        coefs <- coef(model)
        r_squared <- summary(model)$r_squared
        cat("\n--- Regressão Linear (Insumo ~ Área) ---\n")
        cat(sprintf("Equação: Insumo = %.2f + %.2f * Área\n", coefs[1], coefs[2]))
        cat(sprintf("R²: %.2f\n", r_squared))
        
        # Predição com o modelo
        new_area <- data.frame(area = c(100, 250, 500))
        prediction <- predict(model, newdata = new_area)
        cat("\n--- Predições com o Modelo ---\n")
        print(data.frame(Area = new_area$area, Insumo_Predito = prediction))
      } else {
        print_colored("Dados insuficientes para regressão linear.", "warning")
      }
    }, error = function(e) {
      print_colored("Não foi possível realizar a regressão linear.", "error")
      cat(e$message, "\n")
    })
    
    if (!is.null(custo_col)) {
      cat("Custo Total: R$ ", round(total_cost, 2), "\n")
      cat("Custo Médio: R$ ", round(mean_cost, 2), "\n")
      cat("Desvio Padrão do custo: R$ ", round(sd_cost, 2), "\n")
    }
  }
  
  return(results)
}


# =============================================================================
# MÓDULO 5: VISUALIZAÇÃO
# =============================================================================

#' Plota um gráfico de barras em ASCII
#' @param values Vetor numérico com os valores
#' @param title Título do gráfico
plot_ascii_bars <- function(values, title = "Gráfico de Barras em ASCII") {
  # Certifique-se de que os valores são numéricos
  values <- as.numeric(values)
  
  print_colored(paste("\n=== ", title, " ==="), "info")
  max_val <- max(values, na.rm = TRUE)
  scale_factor <- max(0.1, max_val / 50)
  for (i in seq_along(values)) {
    bar_length <- round(values[i] / scale_factor)
    bar <- if (is.na(bar_length)) {
      "\033[0;31mN/A\033[0m"
    } else {
      bar_color <- switch((i %% 3) + 1,
                          "\033[0;34m",  # Azul
                          "\033[0;35m",  # Magenta
                          "\033[0;32m")  # Verde
      paste0(bar_color, paste(rep("#", bar_length), collapse = ""), "\033[0m")
    }
    cat(sprintf("Item %2d | %7.2f | %s\n", i, values[i], bar))
  }
}

#' Gera gráficos utilizando ggplot2 e exibe-os na tela
#' @param df Data frame com os dados
#' @param analysis_results Data frame com os resultados da análise por cultura
generate_ggplots_onscreen <- function(df, analysis_results) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    print_colored("Pacote ggplot2 não encontrado. Gráficos não serão gerados.", "error")
    return()
  }
  
  # Verificar se as colunas necessárias existem (case insensitive)
  cols <- tolower(names(df))
  col_map <- list()
  for (req in c("cultura", "area", "insumo")) {
    idx <- which(cols == req)
    if (length(idx) > 0) {
      col_map[[req]] <- names(df)[idx[1]]
    } else {
      print_colored(paste("Coluna", req, "não encontrada. Gráficos podem não ser gerados corretamente."), "warning")
      return()
    }
  }
  
  # Use os nomes mapeados das colunas
  cultura_col <- col_map[["cultura"]]
  area_col <- col_map[["area"]]
  insumo_col <- col_map[["insumo"]]
  
  # Certifique-se de que os dados são numéricos
  df_plot <- df
  df_plot[[area_col]] <- as.numeric(df[[area_col]])
  df_plot[[insumo_col]] <- as.numeric(df[[insumo_col]])
  
  print_colored("=== GERANDO GRÁFICOS COM ggplot2 (Exibição em Tela) ===", "warning")
  tryCatch({
    library(ggplot2)
        
    # Lista para armazenar os gráficos
    plots <- list()
    
    # Gráfico 1: Boxplot de Área por Cultura
    plots$p1 <- ggplot(df_plot, aes_string(x = cultura_col, y = area_col, fill = cultura_col)) +
      geom_boxplot() +
      labs(title = "Distribuição de Área por Cultura", x = "Cultura", y = "Área (m²)") +
      theme_minimal() +
      theme(legend.position = "none")

    
    # Gráfico 2: Boxplot de Insumo por Cultura
    plots$p2 <- ggplot(df_plot, aes_string(x = cultura_col, y = insumo_col, fill = cultura_col)) +
      geom_boxplot() +
      labs(title = "Distribuição de Insumo por Cultura", x = "Cultura", y = "Insumo (litros)") +
      theme_minimal() +
      theme(legend.position = "none")

    
    # Gráfico 3: Relação entre Área e Insumo
    plots$p3 <- ggplot(df_plot, aes_string(x = area_col, y = insumo_col, color = cultura_col)) +
      geom_point(size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, aes_string(fill = cultura_col)) +
      labs(title = "Relação entre Área e Insumo", x = "Área (m²)", y = "Insumo (litros)") +
      theme_minimal() +
      theme(legend.position = "bottom")

    
    # Verificar se analysis_results tem as colunas necessárias
    if (!is.null(analysis_results) && 
        all(c("cultura", "area_total", "insumo_total") %in% names(analysis_results))) {
      # Gráfico 4: Área Total por Cultura
      plots$p4 <- ggplot(analysis_results, aes(x = cultura, y = area_total, fill = cultura)) +
        geom_bar(stat = "identity") +
        labs(title = "Área Total por Cultura", x = "Cultura", y = "Área Total (m²)") +
        theme_minimal() +
        theme(legend.position = "none")

      
      # Gráfico 5: Insumo Total por Cultura
      plots$p5 <- ggplot(analysis_results, aes(x = cultura, y = insumo_total, fill = cultura)) +
        geom_bar(stat = "identity") +
        labs(title = "Insumo Total por Cultura", x = "Cultura", y = "Insumo Total (litros)") +
        theme_minimal() +
        theme(legend.position = "none")
    }
        
    # Exibe os gráficos
    for (plot_name in names(plots)) {
        print(plots[[plot_name]])
    }

    
    print_colored("Gráficos gerados e exibidos na tela com sucesso!", "success")
  }, error = function(e) {
    print_colored(paste("Erro ao gerar gráficos:", e$message), "error")
  })
}


#' Gera um relatório HTML interativo usando rmarkdown
#' @param data Data frame com os dados
#' @param analysis_results Resultados da análise estatística
#' @param output_file Caminho do arquivo de saída
#' @return Caminho do relatório gerado ou NULL em caso de erro
generate_html_report <- function(data, analysis_results, output_file = file.path(CONFIG$results_dir, "relatorio_farmtech.html")) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    print_colored("Pacote rmarkdown não encontrado. Relatório não será gerado.", "error")
    return(NULL)
  }
  temp_rmd <- tempfile(fileext = ".Rmd")
  # Conteúdo do relatório em RMarkdown
  rmd_content <- paste0(
    "---\n",
    "title: \"Relatório FarmTech\"\n",
    "output: html_document\n",
    "---\n\n",
    "```{r setup, include=FALSE}\nknitr::opts_chunk$set(echo = TRUE)\n```\n\n",
    "# Introdução\n\n",
    "Este relatório apresenta a análise dos dados agrícolas.\n\n",
    "# Dados Carregados\n\n",
    "```{r}\nprint(head(data))\n```\n\n",
    "# Resultados da Análise\n\n",
    "```{r}\nprint(analysis_results)\n```\n\n",
    "# Conclusão\n\n",
    "Relatório gerado automaticamente pelo sistema FarmTech Analytics.\n"
  )
  writeLines(rmd_content, temp_rmd)
  tryCatch({
    rmarkdown::render(temp_rmd, output_file = output_file, quiet = TRUE)
    print_colored(paste("Relatório HTML gerado com sucesso:", output_file), "success")
    return(output_file)
  }, error = function(e) {
    print_colored(paste("Erro ao gerar relatório:", e$message), "error")
    return(NULL)
  }, finally = {
    unlink(temp_rmd)
  })
}

# =============================================================================
# MÓDULO 6: INTEGRAÇÃO COM API METEOROLÓGICA (CPTEC/INPE)
# =============================================================================

#' Consulta a API do CPTEC/INPE e retorna os dados em formato XML
#' @param api_url URL da API
#' @return Dados da API em formato XML ou NULL em caso de erro
get_weather_data <- function(api_url) {
  tryCatch({
    if (!requireNamespace("xml2", quietly = TRUE)) {
      stop("Pacote xml2 é necessário para esta função.")
    }

    show_loading("Consultando API do CPTEC/INPE", duration = 2)
    xml_data <- xml2::read_xml(api_url)
    print_colored("Dados meteorológicos obtidos com sucesso!", "success")
    return(xml_data)
  }, error = function(e) {
    print_colored(paste("Erro ao consultar API:", e$message), "error")
    return(NULL)
  })
}

#' Formata e exibe os dados meteorológicos no console
#' @param xml_data Dados em formato XML
#' @return NULL (efeito colateral de imprimir os dados)
display_weather_data <- function(xml_data) {
  if (is.null(xml_data)) {
    return(NULL)
  }

  tryCatch({
    if (!requireNamespace("xml2", quietly = TRUE)) {
      stop("Pacote xml2 é necessário para esta função.")
    }

    # Extrai os dados relevantes do XML
    city_name <- xml2::xml_text(xml2::xml_find_first(xml_data, "//nome"))
    state <- xml2::xml_text(xml2::xml_find_first(xml_data, "//uf"))
    updated_at <- xml2::xml_text(xml2::xml_find_first(xml_data, "//atualizacao"))
    forecasts <- xml2::xml_find_all(xml_data, "//previsao")

    # Formata a saída
    cat("\n\033[1;34m=== PREVISÃO DO TEMPO (CPTEC/INPE) ===\033[0m\n")
    cat(sprintf("Cidade: %s (%s)\n", city_name, state))
    cat(sprintf("Atualizado em: %s\n\n", updated_at))

    # Itera sobre as previsões
    for (forecast in forecasts) {
      day <- xml2::xml_text(xml2::xml_find_first(forecast, ".//dia"))
      weather <- xml2::xml_text(xml2::xml_find_first(forecast, ".//tempo"))
      max_temp <- xml2::xml_text(xml2::xml_find_first(forecast, ".//maxima"))
      min_temp <- xml2::xml_text(xml2::xml_find_first(forecast, ".//minima"))
      uv <- xml2::xml_text(xml2::xml_find_first(forecast, ".//iuv"))

      # Dicionário de códigos de tempo (poderia ser expandido)
      weather_codes <- list(
        "ec" = "Encoberto com Chuvas Isoladas",
        "ci" = "Chuvas Isoladas",
        "c"  = "Chuva",
        "in" = "Instável",
        "pp" = "Poss. de Pancadas de Chuva",
        "cm" = "Chuva pela Manhã",
        "cn" = "Chuva a Noite",
        "pt" = "Pancadas de Chuva a Tarde",
        "pm" = "Pancadas de Chuva pela Manhã",
        "np" = "Nublado e Pancadas de Chuva",
        "pc" = "Pancadas de Chuva",
        "pn" = "Parcialmente Nublado",
        "cv" = "Chuva",
        "ch" = "Chuvoso",
        "t"  = "Tempestade",
        "ps" = "Predomínio de Sol",
        "e"  = "Encoberto",
        "n"  = "Nublado",
        "cl" = "Céu Claro",
        "nv" = "Nevoeiro",
        "g"  = "Geada",
        "ne" = "Neve",
        "nd" = "Não Definido",
        "pnt" = "Pancadas de Chuva a Noite",
        "psc" = "Possibilidade de Chuva",
        "pcm" = "Possibilidade de Chuva pela Manhã",
        "pct" = "Possibilidade de Chuva a Tarde",
        "pcn" = "Possibilidade de Chuva a Noite",
        "npt" = "Nublado com Pancadas a Tarde",
        "npn" = "Nublado com Pancadas a Noite",
        "npm" = "Nublado com Pancadas pela Manhã",
        "ncn" = "Nublado com Poss. de Chuva a Noite",
        "nct" = "Nublado com Poss. de Chuva a Tarde",
        "ncm" = "Nublado com Poss. de Chuva pela Manhã",
        "npmcm" = "Nublado com Pancadas de Chuva pela Manhã e Chuva a Tarde",
        "nppcm" = "Nublado com Possibilidade de Pancadas de Chuva pela Manhã"

      )
      weather_desc <- ifelse(weather %in% names(weather_codes), weather_codes[[weather]], "Descrição não disponível")

      cat(sprintf("Data: %s\n", day))
      cat(sprintf("Tempo: %s (%s)\n", weather_desc, weather))
      cat(sprintf("Temperatura Máxima: %s°C\n", max_temp))
      cat(sprintf("Temperatura Mínima: %s°C\n", min_temp))
      cat(sprintf("Índice UV: %s\n", uv))
      cat("-----------------------------\n")
    }
  }, error = function(e) {
    print_colored(paste("Erro ao formatar dados meteorológicos:", e$message), "error")
  })
}


# =============================================================================
# MÓDULO 7: CONTROLADOR PRINCIPAL (APLICAÇÃO MVC)
# =============================================================================

main_controller <- function() {
  print_banner()
  print("Bem-vindo à FarmTech Analytics - Sistema de Análise Estatística")
  data_list <- list()
  data_labels <- c()
  current_index <- 1

  repeat {
    choice <- show_main_menu()
    if (is.null(choice)) next

    if (choice == 1) {
      # Carregar dados a partir de arquivo CSV
      file_path <- choose_csv_file("./Dados para Estudo via R")  # Usando caminho relativo
      if (!is.null(file_path)) {
        df <- read_csv_flexible(file_path)
        if (!is.null(df)) {
          print_colored("=== DADOS CARREGADOS ===", "success")
          print(head(df))
          data_list[[current_index]] <- df
          data_labels[current_index] <- paste("Dados Carregados", current_index)
          current_index <- current_index + 1

          # Menu de ações para dados carregados
          repeat {
            action <- show_action_menu()
            if (is.null(action)) next
            if (action == 1) {
              # Estatísticas e análise de regressão
              if (length(data_list) > 0) {
                analysis_results <- analyze_by_culture(df)
              } else {
                print_colored("Nenhum dado carregado para análise.", "error")
              }
            } else if (action == 2) {
              # Gráficos ASCII
              if (length(data_list) > 0) {
                # Encontre a coluna 'area' ou 'Producao' (case insensitive)
                cols <- tolower(names(df))
                area_idx <- which(cols == "area")
                producao_idx <- which(cols == "producao")

                if (length(area_idx) > 0) {
                  plot_ascii_bars(df[[names(df)[area_idx]]], "Áreas (m²)")
                } else if (length(producao_idx) > 0) {
                  plot_ascii_bars(df[[names(df)[producao_idx]]], "Produção (ton)")
                } else {
                  print_colored("Colunas 'area' ou 'Producao' não encontradas.", "warning")
                }
              } else {
                print_colored("Nenhum dado para gerar gráficos.", "error")
              }
            } else if (action == 3) {
              # Gráficos ggplot2 (em tela)
              if (length(data_list) > 0) {
                analysis_results <- analyze_by_culture(df)
                generate_ggplots_onscreen(df, analysis_results)
              } else {
                print_colored("Nenhum dado para gerar gráficos.", "error")
              }
            } else if (action == 4) {
              # Gerar relatório HTML
              if (length(data_list) > 0) {
                analysis_results <- analyze_by_culture(df)
                generate_html_report(df, analysis_results)
              } else {
                print_colored("Nenhum dado para gerar relatório.", "error")
              }
            } else if (action == 5) {
              break  # Volta ao menu principal
            }
          }
        }
      }
    } else if (choice == 2) {
      # Gerar dados fictícios
      df <- generate_fake_data()
      print_colored("=== DADOS GERADOS ===", "success")
      print(head(df))
      data_list[[current_index]] <- df
      data_labels[current_index] <- paste("Dados Fictícios", current_index)
      current_index <- current_index + 1

      # Menu de ações para dados gerados
      repeat {
        action <- show_action_menu()
        if (is.null(action)) next
        if (action == 1) {
          if (length(data_list) > 0) {
            analysis_results <- analyze_by_culture(df)
          } else {
            print_colored("Nenhum dado carregado para análise.", "error")
          }
        } else if (action == 2) {
          if (length(data_list) > 0) {
            if ("area" %in% names(df)) {
              plot_ascii_bars(df$area, "Áreas (m²)")
            } else if ("Producao" %in% names(df)) {
              plot_ascii_bars(df$Producao, "Produção (ton)")
            }
          } else {
            print_colored("Nenhum dado para gerar gráficos.", "error")
          }
        } else if (action == 3) {
          # Gráficos ggplot2 (em tela)
            if(length(data_list) > 0){
                analysis_results <- analyze_by_culture(df)
                generate_ggplots_onscreen(df, analysis_results)
            } else {
                print_colored("Nenhum dado para gerar gráficos.", "error")
            }
        } else if (action == 4) {
          if (length(data_list) > 0) {
            analysis_results <- analyze_by_culture(df)
            generate_html_report(df, analysis_results)
          } else {
            print_colored("Nenhum dado para gerar relatório.", "error")
          }
        } else if (action == 5) {
          break # Volta ao menu principal
        }
      }
    } else if (choice == 3) {
        # Consulta dados meteorológicos
        weather_data <- get_weather_data(CONFIG$weather_api_url)
        display_weather_data(weather_data)

    }else if (choice == 4) {
      print_colored("Encerrando análise. Obrigado por utilizar o FarmTech Analytics!", "info")
      break
    }

    # Pergunta se o usuário deseja reiniciar a análise
    restart <- readline("Deseja reiniciar a análise (s/n)? ")
    if (tolower(restart) != "s") {
        print_colored("Encerrando a análise. Obrigado por utilizar o FarmTech Analytics!", "info")
        break
    }
  }
}

# Execução do controlador principal
main_controller()
