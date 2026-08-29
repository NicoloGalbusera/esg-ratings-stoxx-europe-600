# ==============================================================================
# ANALISI EMPIRICHE DELLA TESI MAGISTRALE
# Autore: Nicolò Galbusera
# Campione: STOXX Europe 600 | Fonte dei dati: Bloomberg
# ==============================================================================
#
# Lo script riproduce le analisi utilizzate nell'elaborato:
#   0. importazione, pulizia, controlli e costruzione del panel impresa-anno;
#   1. andamento dell'ESG Score, traiettorie individuali, pilastri e disclosure;
#   2. regressioni standardizzate annuali ESG ~ Environmental + Social + Governance;
#   3. differenze settoriali nel periodo 2015-2024;
#   4. replica metodologica adattata di D'Amato et al. con Random Forest e GLM;
#   5. correlazioni tra ESG Score, EBIT e Tobin's Q;
#   6. XGBoost Delta-Horizon a uno, due e tre anni;
#   7. analisi ARIMA della serie annuale dell'ESG Score medio.
#
# Il dataset originale non è incluso nel repository per le restrizioni di
# licenza applicabili ai dati Bloomberg. Le istruzioni di esecuzione e la
# descrizione degli output sono riportate nel file README.md.
# ==============================================================================


# ==============================================================================
# 0. CONFIGURAZIONE GENERALE
# ==============================================================================

options(stringsAsFactors = FALSE, scipen = 999)
set.seed(123)

PERCORSO_FILE <- Sys.getenv(
  "DATI_ESG_FILE",
  unset = file.path("data", "dati_esg_bloomberg.xlsx")
)

FOGLIO_EXCEL <- 1

CARTELLA_OUTPUT <- Sys.getenv(
  "OUTPUT_TESI_ESG",
  unset = "results"
)

ANNO_INIZIALE <- 2015
ANNO_FINALE_ESG <- 2024
ANNO_FINALE_DATASET <- 2025

# Moduli di analisi. Impostare FALSE per escludere un singolo modulo.
ESEGUI_DESCRITTIVE <- TRUE
ESEGUI_SETTORI <- TRUE
ESEGUI_RANDOM_FOREST <- TRUE
ESEGUI_CORRELAZIONI <- TRUE
ESEGUI_XGBOOST <- TRUE
ESEGUI_ARIMA <- TRUE

# Random Forest, replica D'Amato et al.
RF_NUMERO_ALBERI <- 500
RF_QUOTA_TRAINING <- 0.80
RF_SEED_SPLIT <- 123
RF_NUMERO_SEED_TUNING <- 100
RF_NODESIZE_GRID <- c(1, 3, 5, 10)

# Correlazioni: winsorizzazione al 1o e 99o percentile.
PERCENTILE_WINSOR <- 0.01

# XGBoost Delta-Horizon.
XGB_ORIZZONTI <- c(1, 2, 3)
XGB_ULTIMO_ANNO_TARGET <- 2023
XGB_INCLUDI_PILASTRI <- FALSE
XGB_N_CONFIGURAZIONI <- 24
XGB_NROUNDS_MAX <- 2000
XGB_EARLY_STOPPING <- 75

# ARIMA.
ARIMA_ANNI_OSSERVATI <- 2015:2024
ARIMA_ANNO_TEST_INDICATIVO <- 2024
ARIMA_ANNI_VALIDAZIONE_ROLLING <- 2021:2023
ARIMA_ANNI_PREVISIONE_RICHIESTI <- c(2025, 2030, 2050)
ARIMA_ULTIMO_ANNO_PREVISIONE <- 2050

N_THREAD <- parallel::detectCores(logical = TRUE)
if (is.na(N_THREAD) || N_THREAD < 2) {
  N_THREAD <- 1L
} else {
  N_THREAD <- min(as.integer(N_THREAD - 1L), 8L)
}


# ==============================================================================
# 1. PACCHETTI
# ==============================================================================

PACCHETTI <- c(
  "readxl",
  "tidyverse",
  "randomForest",
  "corrplot",
  "xgboost",
  "Matrix",
  "forecast"
)

pacchetti_mancanti <- PACCHETTI[
  !vapply(PACCHETTI, requireNamespace, logical(1), quietly = TRUE)
]

if (length(pacchetti_mancanti) > 0) {
  comando_installazione <- paste0(
    "install.packages(c(",
    paste(sprintf("\"%s\"", pacchetti_mancanti), collapse = ", "),
    "))"
  )

  stop(
    paste0(
      "Pacchetti R mancanti: ",
      paste(pacchetti_mancanti, collapse = ", "),
      ".\nInstallarli con:\n",
      comando_installazione
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages(
  invisible(lapply(PACCHETTI, library, character.only = TRUE))
)


# ==============================================================================
# 2. CARTELLE E FUNZIONI DI SUPPORTO
# ==============================================================================

dir.create(CARTELLA_OUTPUT, recursive = TRUE, showWarnings = FALSE)

crea_cartelle_modulo <- function(numero, nome) {
  principale <- file.path(
    CARTELLA_OUTPUT,
    paste0(sprintf("%02d", numero), "_", nome)
  )
  grafici <- file.path(principale, "Grafici")
  tabelle <- file.path(principale, "Tabelle")
  modelli <- file.path(principale, "Modelli")
  
  dir.create(grafici, recursive = TRUE, showWarnings = FALSE)
  dir.create(tabelle, recursive = TRUE, showWarnings = FALSE)
  dir.create(modelli, recursive = TRUE, showWarnings = FALSE)
  
  list(
    principale = principale,
    grafici = grafici,
    tabelle = tabelle,
    modelli = modelli
  )
}

cartelle_controlli <- crea_cartelle_modulo(0, "Controlli_dati")
cartelle_descrittive <- crea_cartelle_modulo(1, "Struttura_e_pilastri")
cartelle_settori <- crea_cartelle_modulo(2, "Analisi_settoriali")
cartelle_rf <- crea_cartelle_modulo(3, "Random_Forest_DAmato")
cartelle_correlazioni <- crea_cartelle_modulo(4, "Correlazioni")
cartelle_xgb <- crea_cartelle_modulo(5, "XGBoost_Delta_Horizon")
cartelle_arima <- crea_cartelle_modulo(6, "ARIMA")

tema_tesi <- ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(color = "gray35"),
    legend.position = "bottom",
    panel.grid.minor = ggplot2::element_blank()
  )

salva_grafico <- function(
    grafico,
    cartella,
    nome_file,
    larghezza = 10,
    altezza = 7,
    salva_pdf = TRUE,
    salva_png = TRUE
) {
  if (salva_pdf) {
    ggplot2::ggsave(
      filename = file.path(cartella, paste0(nome_file, ".pdf")),
      plot = grafico,
      width = larghezza,
      height = altezza,
      units = "in",
      bg = "white"
    )
  }
  
  if (salva_png) {
    ggplot2::ggsave(
      filename = file.path(cartella, paste0(nome_file, ".png")),
      plot = grafico,
      width = larghezza,
      height = altezza,
      units = "in",
      dpi = 300,
      bg = "white"
    )
  }
}

salva_grafico_base <- function(
    funzione_grafico,
    cartella,
    nome_file,
    larghezza = 10,
    altezza = 7
) {
  grDevices::pdf(
    file.path(cartella, paste0(nome_file, ".pdf")),
    width = larghezza,
    height = altezza
  )
  tryCatch(funzione_grafico(), finally = grDevices::dev.off())
  
  grDevices::png(
    file.path(cartella, paste0(nome_file, ".png")),
    width = larghezza,
    height = altezza,
    units = "in",
    res = 300
  )
  tryCatch(funzione_grafico(), finally = grDevices::dev.off())
}

media_sicura <- function(x) {
  valori <- x[is.finite(x)]
  if (length(valori) == 0) NA_real_ else mean(valori)
}

mediana_sicura <- function(x) {
  valori <- x[is.finite(x)]
  if (length(valori) == 0) NA_real_ else median(valori)
}

sd_sicura <- function(x) {
  valori <- x[is.finite(x)]
  if (length(valori) < 2) NA_real_ else stats::sd(valori)
}

winsorize <- function(x, p = 0.01) {
  x <- as.numeric(x)
  validi <- x[is.finite(x)]
  
  if (length(validi) < 2) {
    return(x)
  }
  
  soglie <- stats::quantile(
    validi,
    probs = c(p, 1 - p),
    na.rm = TRUE,
    names = FALSE,
    type = 7
  )
  
  x[is.finite(x) & x < soglie[1]] <- soglie[1]
  x[is.finite(x) & x > soglie[2]] <- soglie[2]
  x
}

cor_sicura <- function(x, y, metodo = "pearson") {
  validi <- is.finite(x) & is.finite(y)
  
  if (
    sum(validi) < 3 ||
    stats::sd(x[validi]) == 0 ||
    stats::sd(y[validi]) == 0
  ) {
    return(NA_real_)
  }
  
  stats::cor(x[validi], y[validi], method = metodo)
}

test_cor_sicuro <- function(x, y, metodo = "pearson") {
  validi <- is.finite(x) & is.finite(y)
  
  if (
    sum(validi) < 3 ||
    stats::sd(x[validi]) == 0 ||
    stats::sd(y[validi]) == 0
  ) {
    return(
      tibble::tibble(
        N = sum(validi),
        Correlazione = NA_real_,
        P_value = NA_real_,
        IC_95_inferiore = NA_real_,
        IC_95_superiore = NA_real_
      )
    )
  }
  
  risultato <- stats::cor.test(
    x[validi],
    y[validi],
    method = metodo
  )
  
  tibble::tibble(
    N = sum(validi),
    Correlazione = unname(risultato$estimate),
    P_value = risultato$p.value,
    IC_95_inferiore = risultato$conf.int[1],
    IC_95_superiore = risultato$conf.int[2]
  )
}

metriche_regressione <- function(reale, predetto) {
  reale <- as.numeric(reale)
  predetto <- as.numeric(predetto)
  
  if (length(reale) != length(predetto)) {
    stop("Le serie osservata e prevista hanno lunghezze differenti.")
  }
  
  validi <- is.finite(reale) & is.finite(predetto)
  
  if (!any(validi)) {
    return(
      tibble::tibble(
        RMSE = NA_real_,
        MAE = NA_real_,
        R2 = NA_real_,
        Correlazione = NA_real_
      )
    )
  }
  
  reale <- reale[validi]
  predetto <- predetto[validi]
  denominatore_r2 <- sum((reale - mean(reale))^2)
  
  tibble::tibble(
    RMSE = sqrt(mean((reale - predetto)^2)),
    MAE = mean(abs(reale - predetto)),
    R2 = if (
      is.finite(denominatore_r2) && denominatore_r2 > 0
    ) {
      1 - sum((reale - predetto)^2) / denominatore_r2
    } else {
      NA_real_
    },
    Correlazione = if (
      length(reale) > 2 &&
      stats::sd(reale) > 0 &&
      stats::sd(predetto) > 0
    ) {
      stats::cor(reale, predetto)
    } else {
      NA_real_
    }
  )
}

rmse <- function(osservato, previsto) {
  sqrt(mean((osservato - previsto)^2, na.rm = TRUE))
}

mape <- function(osservato, previsto) {
  validi <- is.finite(osservato) & is.finite(previsto) & osservato != 0
  if (!any(validi)) return(NA_real_)
  mean(abs((osservato[validi] - previsto[validi]) / osservato[validi])) * 100
}

controlla_variabili <- function(dati, variabili, contesto) {
  mancanti <- setdiff(variabili, names(dati))
  if (length(mancanti) > 0) {
    stop(
      paste0(
        "Variabili mancanti per ", contesto, ": ",
        paste(mancanti, collapse = ", ")
      )
    )
  }
}


# ==============================================================================
# 3. IMPORTAZIONE, PULIZIA E PANEL IMPRESA-ANNO
# ==============================================================================

if (!file.exists(PERCORSO_FILE)) {
  stop(
    paste0(
      "File non trovato: ", PERCORSO_FILE, "\n",
      "Eseguire lo script dalla cartella principale del repository oppure ",
      "impostare la variabile d'ambiente DATI_ESG_FILE."
    ),
    call. = FALSE
  )
}

dati_raw <- readxl::read_excel(
  PERCORSO_FILE,
  sheet = FOGLIO_EXCEL,
  na = c("", "NA", "N/A", "#N/A", "#N/A N/A")
)

names(dati_raw) <- trimws(names(dati_raw))

if (ncol(dati_raw) < 4) {
  stop("Il file non contiene almeno le quattro colonne identificative attese.")
}

# Nel file Bloomberg le prime quattro colonne sono, nell'ordine:
# Settore, Nome, descrizione completa dell'indicatore, nome breve dell'indicatore.
colnames(dati_raw)[1:4] <- c(
  "Settore",
  "Nome",
  "Indicatore_completo",
  "indicatore_nome"
)

anni_richiesti <- as.character(ANNO_INIZIALE:ANNO_FINALE_DATASET)
anni_presenti <- intersect(anni_richiesti, names(dati_raw))
anni_mancanti <- setdiff(
  as.character(ANNO_INIZIALE:ANNO_FINALE_ESG),
  anni_presenti
)

if (length(anni_mancanti) > 0) {
  stop(
    paste0(
      "Mancano le colonne annuali necessarie: ",
      paste(anni_mancanti, collapse = ", ")
    )
  )
}

if (!as.character(ANNO_FINALE_DATASET) %in% anni_presenti) {
  warning("La colonna 2025 non e presente: il relativo controllo verra omesso.")
}

dati_long <- dati_raw %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(anni_presenti),
    names_to = "Anno",
    values_to = "Valore"
  ) %>%
  dplyr::mutate(
    Anno = as.integer(Anno),
    Valore = as.character(Valore),
    Valore = stringr::str_replace_all(Valore, ",", "."),
    Valore = dplyr::na_if(Valore, "#N/A N/A"),
    Valore = dplyr::na_if(Valore, "#N/A"),
    Valore = dplyr::na_if(Valore, "N/A"),
    Valore = suppressWarnings(as.numeric(Valore))
  ) %>%
  dplyr::select(
    Settore,
    Nome,
    Indicatore_completo,
    indicatore_nome,
    Anno,
    Valore
  )

duplicati_indicatore <- dati_long %>%
  dplyr::count(Settore, Nome, Anno, indicatore_nome, name = "Numero_righe") %>%
  dplyr::filter(Numero_righe > 1)

readr::write_csv(
  duplicati_indicatore,
  file.path(cartelle_controlli$tabelle, "00_eventuali_duplicati_indicatore.csv")
)

panel <- dati_long %>%
  dplyr::group_by(Settore, Nome, Anno, indicatore_nome) %>%
  dplyr::summarise(
    Valore = if (all(is.na(Valore))) NA_real_ else mean(Valore, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    id_cols = c(Settore, Nome, Anno),
    names_from = indicatore_nome,
    values_from = Valore
  ) %>%
  dplyr::arrange(Nome, Anno)

variabili_attese <- c(
  "ESG_SCORE",
  "ESG_DISCLOSURE_SCORE",
  "ENVIRONMENTAL_SCORE",
  "ENVIRON_DISCLOSURE_SCORE",
  "SOCIAL_SCORE",
  "SOCIAL_DISCLOSURE_SCORE",
  "GOVERNANCE_SCORE",
  "GOVNCE_DISCLOSURE_SCORE",
  "RETURN_COM_EQY",
  "RETURN_ON_ASSET",
  "EBIT",
  "DIVIDEND_INDICATED_YIELD",
  "TOBIN_Q_RATIO"
)

controlla_variabili(panel, variabili_attese, "le analisi della tesi")

panel <- panel %>%
  dplyr::mutate(
    Settore = as.character(Settore),
    Macro_Settore = dplyr::case_when(
      Settore %in% c(
        "Industria",
        "Materiali",
        "Servizi di pubblica utilita",
        "Servizi di pubblica utilità",
        "Energia"
      ) ~ "Industria, energia e utilities",
      Settore %in% c(
        "Salute",
        "Servizi comunicazione",
        "Servizi di comunicazione",
        "Informatica"
      ) ~ "Servizi, tecnologia e salute",
      Settore %in% c(
        "Beni voluttuari",
        "Beni di consumo discrezionali",
        "Beni di prima necessita",
        "Beni di prima necessità"
      ) ~ "Consumi",
      Settore %in% c(
        "Finanza",
        "Investimento immobiliare",
        "Immobiliare"
      ) ~ "Finanza e immobiliare",
      TRUE ~ "Altro"
    )
  )

variabili_finanziarie <- c(
  "RETURN_COM_EQY",
  "RETURN_ON_ASSET",
  "EBIT",
  "DIVIDEND_INDICATED_YIELD",
  "TOBIN_Q_RATIO"
)

# Per garantire coerenza con l'analisi riportata nell'elaborato, le soglie di
# winsorizzazione sono calcolate sull'intero panel disponibile. Le correlazioni
# sono successivamente stimate sul solo periodo 2015-2024.
panel <- panel %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(variabili_finanziarie),
      ~ winsorize(.x, p = PERCENTILE_WINSOR),
      .names = "{.col}_w"
    )
  )

readr::write_csv(
  panel,
  file.path(cartelle_controlli$tabelle, "panel_completo_pulito.csv")
)

controllo_generale <- tibble::tibble(
  Indicatore = c(
    "Numero imprese",
    "Numero settori",
    "Numero macro-settori",
    "Anno minimo",
    "Anno massimo",
    "Osservazioni impresa-anno",
    "Righe panel duplicate"
  ),
  Valore = c(
    dplyr::n_distinct(panel$Nome),
    dplyr::n_distinct(panel$Settore),
    dplyr::n_distinct(panel$Macro_Settore),
    min(panel$Anno, na.rm = TRUE),
    max(panel$Anno, na.rm = TRUE),
    nrow(panel),
    sum(duplicated(panel[c("Nome", "Anno")]))
  )
)

composizione_settoriale <- panel %>%
  dplyr::distinct(Nome, Settore, Macro_Settore) %>%
  dplyr::count(Settore, Macro_Settore, name = "Numero_imprese") %>%
  dplyr::arrange(dplyr::desc(Numero_imprese))

composizione_macro_settoriale <- panel %>%
  dplyr::distinct(Nome, Settore, Macro_Settore) %>%
  dplyr::group_by(Macro_Settore) %>%
  dplyr::summarise(
    Settori_inclusi = paste(sort(unique(Settore)), collapse = ", "),
    Numero_imprese = dplyr::n_distinct(Nome),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(Numero_imprese))

missing_per_variabile <- purrr::map_dfr(
  variabili_attese,
  function(variabile) {
    tibble::tibble(
      Variabile = variabile,
      Osservazioni_disponibili = sum(!is.na(panel[[variabile]])),
      Osservazioni_mancanti = sum(is.na(panel[[variabile]])),
      Percentuale_mancanti = mean(is.na(panel[[variabile]])) * 100
    )
  }
)

readr::write_csv(
  controllo_generale,
  file.path(cartelle_controlli$tabelle, "01_controllo_generale.csv")
)
readr::write_csv(
  composizione_settoriale,
  file.path(cartelle_controlli$tabelle, "02_composizione_settoriale.csv")
)
readr::write_csv(
  composizione_macro_settoriale,
  file.path(cartelle_controlli$tabelle, "03_composizione_macro_settoriale.csv")
)
readr::write_csv(
  missing_per_variabile,
  file.path(cartelle_controlli$tabelle, "04_missing_per_variabile.csv")
)

audit_aggiornamento <- panel %>%
  dplyr::group_by(Nome) %>%
  dplyr::arrange(Anno, .by_group = TRUE) %>%
  dplyr::mutate(ESG_precedente = dplyr::lag(ESG_SCORE, 1L)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Anno) %>%
  dplyr::summarise(
    N_coppie = sum(is.finite(ESG_SCORE) & is.finite(ESG_precedente)),
    Quota_ESG_invariato = if (
      N_coppie > 0
    ) {
      mean(ESG_SCORE == ESG_precedente, na.rm = TRUE)
    } else {
      NA_real_
    },
    Media_delta_assoluto = if (
      N_coppie > 0
    ) {
      mean(abs(ESG_SCORE - ESG_precedente), na.rm = TRUE)
    } else {
      NA_real_
    },
    .groups = "drop"
  )

readr::write_csv(
  audit_aggiornamento,
  file.path(cartelle_controlli$tabelle, "05_audit_aggiornamento_ESG.csv")
)


# ==============================================================================
# 4. STRUTTURA ED EVOLUZIONE DEI RATING ESG - SEZIONE 4.1.1
# ==============================================================================

if (ESEGUI_DESCRITTIVE) {
  panel_descrittive <- panel %>%
    dplyr::filter(
      Anno >= ANNO_INIZIALE,
      Anno <= ANNO_FINALE_DATASET
    )
  
  trend_esg_generale <- panel_descrittive %>%
    dplyr::group_by(Anno) %>%
    dplyr::summarise(
      N = sum(is.finite(ESG_SCORE)),
      ESG_medio = media_sicura(ESG_SCORE),
      ESG_mediano = mediana_sicura(ESG_SCORE),
      ESG_sd = sd_sicura(ESG_SCORE),
      .groups = "drop"
    )
  
  readr::write_csv(
    trend_esg_generale,
    file.path(cartelle_descrittive$tabelle, "01_ESG_medio_annuale.csv")
  )
  
  grafico_traiettorie_esg <- ggplot2::ggplot(
    panel_descrittive,
    ggplot2::aes(x = Anno, y = ESG_SCORE, group = Nome)
  ) +
    ggplot2::geom_line(alpha = 0.06, color = "gray45", na.rm = TRUE) +
    ggplot2::stat_summary(
      ggplot2::aes(group = 1),
      fun = mean,
      geom = "line",
      color = "red3",
      linewidth = 1.25,
      na.rm = TRUE
    ) +
    ggplot2::scale_x_continuous(breaks = ANNO_INIZIALE:ANNO_FINALE_DATASET) +
    ggplot2::labs(
      title = "Traiettoria ESG score delle imprese dal 2015 al 2025",
      x = "Anno",
      y = "ESG Score"
    ) +
    tema_tesi +
    ggplot2::theme(legend.position = "none")
  
  salva_grafico(
    grafico_traiettorie_esg,
    cartelle_descrittive$grafici,
    "figura_4_1_1_traiettoria_ESG_score",
    larghezza = 9,
    altezza = 6
  )
  
  grafico_trend_esg <- ggplot2::ggplot(
    trend_esg_generale,
    ggplot2::aes(x = Anno, y = ESG_medio)
  ) +
    ggplot2::geom_line(color = "steelblue", linewidth = 1.1) +
    ggplot2::geom_point(color = "navy", size = 2.6) +
    ggplot2::scale_x_continuous(breaks = ANNO_INIZIALE:ANNO_FINALE_DATASET) +
    ggplot2::labs(
      title = "Andamento ESG medio",
      x = "Anno",
      y = "ESG Score medio"
    ) +
    tema_tesi +
    ggplot2::theme(legend.position = "none")
  
  salva_grafico(
    grafico_trend_esg,
    cartelle_descrittive$grafici,
    "figura_4_1_2_andamento_ESG_medio",
    larghezza = 9,
    altezza = 6
  )
  
  variabili_pilastri_plot <- c(
    ENVIRONMENTAL_SCORE = "Environmental",
    SOCIAL_SCORE = "Social",
    GOVERNANCE_SCORE = "Governance",
    ENVIRON_DISCLOSURE_SCORE = "Environmental",
    SOCIAL_DISCLOSURE_SCORE = "Social",
    GOVNCE_DISCLOSURE_SCORE = "Governance"
  )
  
  dati_pilastri_plot <- panel_descrittive %>%
    dplyr::select(
      Anno,
      ENVIRONMENTAL_SCORE,
      SOCIAL_SCORE,
      GOVERNANCE_SCORE,
      ENVIRON_DISCLOSURE_SCORE,
      SOCIAL_DISCLOSURE_SCORE,
      GOVNCE_DISCLOSURE_SCORE
    ) %>%
    tidyr::pivot_longer(
      cols = -Anno,
      names_to = "Variabile",
      values_to = "Punteggio"
    ) %>%
    dplyr::mutate(
      Tipo = dplyr::if_else(
        stringr::str_detect(Variabile, "DISCLOSURE"),
        "Disclosure",
        "Performance ESG"
      ),
      Pilastro = unname(variabili_pilastri_plot[Variabile]),
      Pilastro = factor(
        Pilastro,
        levels = c("Environmental", "Social", "Governance")
      ),
      Tipo = factor(Tipo, levels = c("Performance ESG", "Disclosure"))
    ) %>%
    dplyr::group_by(Anno, Tipo, Pilastro) %>%
    dplyr::summarise(
      N = sum(is.finite(Punteggio)),
      Punteggio_medio = media_sicura(Punteggio),
      .groups = "drop"
    )
  
  readr::write_csv(
    dati_pilastri_plot,
    file.path(
      cartelle_descrittive$tabelle,
      "02_medie_annuali_pilastri_e_disclosure.csv"
    )
  )
  
  grafico_pilastri <- ggplot2::ggplot(
    dati_pilastri_plot,
    ggplot2::aes(x = Anno, y = Punteggio_medio, color = Pilastro)
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_grid(Tipo ~ Pilastro, scales = "free_y") +
    ggplot2::scale_x_continuous(breaks = ANNO_INIZIALE:ANNO_FINALE_DATASET) +
    ggplot2::labs(
      title = "Evoluzione dei pilastri ESG e dei disclosure score",
      x = "Anno",
      y = "Punteggio medio"
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      legend.position = "none",
      strip.text = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold")
    )
  
  salva_grafico(
    grafico_pilastri,
    cartelle_descrittive$grafici,
    "figura_4_1_3_evoluzione_pilastri_e_disclosure",
    larghezza = 12,
    altezza = 7
  )
  
  dati_regressioni_pilastri <- panel_descrittive %>%
    dplyr::select(
      Anno,
      ESG_SCORE,
      ENVIRONMENTAL_SCORE,
      SOCIAL_SCORE,
      GOVERNANCE_SCORE
    ) %>%
    tidyr::drop_na()
  
  influenza_pilastri <- dati_regressioni_pilastri %>%
    dplyr::group_by(Anno) %>%
    dplyr::group_modify(
      ~ {
        modello <- stats::lm(
          scale(ESG_SCORE) ~
            scale(ENVIRONMENTAL_SCORE) +
            scale(SOCIAL_SCORE) +
            scale(GOVERNANCE_SCORE),
          data = .x
        )
        
        tibble::tibble(
          Pilastro = c("Ambientale", "Sociale", "Governance"),
          Coefficiente_standardizzato = unname(stats::coef(modello)[-1]),
          N = stats::nobs(modello),
          R2 = summary(modello)$r.squared,
          R2_aggiustato = summary(modello)$adj.r.squared
        )
      }
    ) %>%
    dplyr::ungroup()
  
  readr::write_csv(
    influenza_pilastri,
    file.path(
      cartelle_descrittive$tabelle,
      "03_regressioni_standardizzate_pilastri_per_anno.csv"
    )
  )
  
  grafico_influenza <- ggplot2::ggplot(
    influenza_pilastri,
    ggplot2::aes(
      x = Anno,
      y = Coefficiente_standardizzato,
      group = Pilastro,
      color = Pilastro
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_x_continuous(breaks = ANNO_INIZIALE:ANNO_FINALE_DATASET) +
    ggplot2::labs(
      title = "Evoluzione dell'influenza dei pilastri ESG sul rating complessivo",
      x = "Anno",
      y = "Coefficiente standardizzato",
      color = "Pilastro"
    ) +
    tema_tesi
  
  salva_grafico(
    grafico_influenza,
    cartelle_descrittive$grafici,
    "figura_4_1_4_evoluzione_influenza_pilastri",
    larghezza = 9,
    altezza = 6
  )
}


# ==============================================================================
# 5. DIFFERENZE SETTORIALI - SEZIONE 4.1.2
# ==============================================================================

if (ESEGUI_SETTORI) {
  panel_settori <- panel %>%
    dplyr::filter(
      Anno >= ANNO_INIZIALE,
      Anno <= ANNO_FINALE_ESG,
      !is.na(Settore),
      Settore != ""
    )
  
  informazioni_variabili_settori <- tibble::tribble(
    ~Variabile, ~Etichetta, ~Nome_file,
    "ESG_SCORE", "ESG Score", "ESG_SCORE",
    "ESG_DISCLOSURE_SCORE", "ESG Disclosure Score", "ESG_DISCLOSURE",
    "ENVIRONMENTAL_SCORE", "Environmental Score", "ENVIRONMENTAL",
    "SOCIAL_SCORE", "Social Score", "SOCIAL",
    "GOVERNANCE_SCORE", "Governance Score", "GOVERNANCE"
  )
  
  analisi_settoriale <- function(
    dati,
    variabile,
    etichetta,
    nome_file,
    anno_iniziale = 2015,
    anno_finale = 2024
  ) {
    andamento_annuale <- dati %>%
      dplyr::group_by(Settore, Anno) %>%
      dplyr::summarise(
        Numero_imprese = sum(is.finite(.data[[variabile]])),
        Media = media_sicura(.data[[variabile]]),
        Mediana = mediana_sicura(.data[[variabile]]),
        Deviazione_standard = sd_sicura(.data[[variabile]]),
        .groups = "drop"
      )
    
    readr::write_csv(
      andamento_annuale,
      file.path(
        cartelle_settori$tabelle,
        paste0("andamento_annuale_", nome_file, "_per_settore.csv")
      )
    )
    
    confronto_anni <- andamento_annuale %>%
      dplyr::filter(Anno %in% c(anno_iniziale, anno_finale)) %>%
      dplyr::select(Settore, Anno, Media, Numero_imprese) %>%
      tidyr::pivot_wider(
        names_from = Anno,
        values_from = c(Media, Numero_imprese),
        names_glue = "{.value}_{Anno}"
      )
    
    colonna_media_iniziale <- paste0("Media_", anno_iniziale)
    colonna_media_finale <- paste0("Media_", anno_finale)
    colonna_n_iniziale <- paste0("Numero_imprese_", anno_iniziale)
    colonna_n_finale <- paste0("Numero_imprese_", anno_finale)
    
    confronto_anni <- confronto_anni %>%
      dplyr::mutate(
        Variabile = etichetta,
        Variazione_assoluta =
          .data[[colonna_media_finale]] - .data[[colonna_media_iniziale]],
        Variazione_percentuale = dplyr::if_else(
          is.na(.data[[colonna_media_iniziale]]) |
            .data[[colonna_media_iniziale]] == 0,
          NA_real_,
          100 * (
            .data[[colonna_media_finale]] - .data[[colonna_media_iniziale]]
          ) / abs(.data[[colonna_media_iniziale]])
        )
      ) %>%
      dplyr::arrange(dplyr::desc(.data[[colonna_media_finale]])) %>%
      dplyr::mutate(Posizione_anno_finale = dplyr::row_number()) %>%
      dplyr::select(
        Variabile,
        Settore,
        Posizione_anno_finale,
        dplyr::all_of(colonna_media_iniziale),
        dplyr::all_of(colonna_media_finale),
        Variazione_assoluta,
        Variazione_percentuale,
        dplyr::all_of(colonna_n_iniziale),
        dplyr::all_of(colonna_n_finale)
      )
    
    readr::write_csv(
      confronto_anni,
      file.path(
        cartelle_settori$tabelle,
        paste0(
          "confronto_", anno_iniziale, "_", anno_finale, "_",
          nome_file, "_per_settore.csv"
        )
      )
    )
    
    grafico_andamento <- ggplot2::ggplot(
      andamento_annuale,
      ggplot2::aes(
        x = Anno,
        y = Media,
        color = Settore,
        group = Settore
      )
    ) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 2) +
      ggplot2::scale_x_continuous(breaks = anno_iniziale:anno_finale) +
      ggplot2::labs(
        title = paste("Evoluzione del", etichetta, "per settore"),
        subtitle = paste0(
          "Valori medi annuali, periodo ", anno_iniziale, "-", anno_finale
        ),
        x = "Anno",
        y = paste(etichetta, "medio"),
        color = "Settore"
      ) +
      tema_tesi
    
    salva_grafico(
      grafico_andamento,
      cartelle_settori$grafici,
      paste0("evoluzione_", nome_file, "_per_settore"),
      larghezza = 12,
      altezza = 8
    )
    
    grafico_facet <- ggplot2::ggplot(
      andamento_annuale,
      ggplot2::aes(x = Anno, y = Media)
    ) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 2) +
      ggplot2::facet_wrap(~ Settore, ncol = 3) +
      ggplot2::scale_x_continuous(
        breaks = seq(anno_iniziale, anno_finale, by = 2)
      ) +
      ggplot2::labs(
        title = paste("Evoluzione del", etichetta, "nei singoli settori"),
        subtitle = paste0(
          "Valori medi annuali, periodo ", anno_iniziale, "-", anno_finale
        ),
        x = "Anno",
        y = paste(etichetta, "medio")
      ) +
      tema_tesi +
      ggplot2::theme(
        legend.position = "none",
        strip.text = ggplot2::element_text(size = 18, face = "bold"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8)
      )
    
    nome_facet <- paste0("evoluzione_", nome_file, "_facet_settori")
    if (variabile == "ESG_SCORE") {
      nome_facet <- "figura_4_1_5_evoluzione_ESG_SCORE_facet_settori"
    }
    
    salva_grafico(
      grafico_facet,
      cartelle_settori$grafici,
      nome_facet,
      larghezza = 12,
      altezza = 12
    )
    
    grafico_variazione <- confronto_anni %>%
      dplyr::mutate(
        Settore = stats::reorder(Settore, Variazione_assoluta)
      ) %>%
      ggplot2::ggplot(
        ggplot2::aes(x = Settore, y = Variazione_assoluta)
      ) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
      ggplot2::labs(
        title = paste("Variazione del", etichetta, "per settore"),
        subtitle = paste0("Differenza tra ", anno_finale, " e ", anno_iniziale),
        x = "Settore",
        y = "Variazione assoluta"
      ) +
      tema_tesi +
      ggplot2::theme(legend.position = "none")
    
    salva_grafico(
      grafico_variazione,
      cartelle_settori$grafici,
      paste0(
        "variazione_", anno_iniziale, "_", anno_finale, "_",
        nome_file, "_per_settore"
      ),
      larghezza = 10,
      altezza = 7
    )
    
    list(
      andamento = andamento_annuale,
      confronto = confronto_anni,
      grafico_andamento = grafico_andamento,
      grafico_facet = grafico_facet,
      grafico_variazione = grafico_variazione
    )
  }
  
  risultati_settoriali <- list()
  
  for (i in seq_len(nrow(informazioni_variabili_settori))) {
    variabile_corrente <- informazioni_variabili_settori$Variabile[i]
    
    risultati_settoriali[[variabile_corrente]] <- analisi_settoriale(
      dati = panel_settori,
      variabile = variabile_corrente,
      etichetta = informazioni_variabili_settori$Etichetta[i],
      nome_file = informazioni_variabili_settori$Nome_file[i],
      anno_iniziale = ANNO_INIZIALE,
      anno_finale = ANNO_FINALE_ESG
    )
  }
  
  tabella_completa_settori_long <- dplyr::bind_rows(
    lapply(risultati_settoriali, function(x) x$confronto)
  )
  
  readr::write_csv(
    tabella_completa_settori_long,
    file.path(
      cartelle_settori$tabelle,
      "tabella_completa_settori_2015_2024.csv"
    )
  )
  
  tabella_completa_settori_wide <- tabella_completa_settori_long %>%
    dplyr::select(
      Settore,
      Variabile,
      dplyr::starts_with("Media_"),
      Variazione_assoluta,
      Variazione_percentuale
    ) %>%
    tidyr::pivot_wider(
      names_from = Variabile,
      values_from = c(
        dplyr::starts_with("Media_"),
        Variazione_assoluta,
        Variazione_percentuale
      ),
      names_glue = "{.value}_{Variabile}"
    )
  
  readr::write_csv(
    tabella_completa_settori_wide,
    file.path(
      cartelle_settori$tabelle,
      "tabella_riassuntiva_wide_settori_2015_2024.csv"
    )
  )
  
  riepilogo_settori_2024 <- panel_settori %>%
    dplyr::filter(Anno == ANNO_FINALE_ESG) %>%
    dplyr::group_by(Settore) %>%
    dplyr::summarise(
      Numero_imprese = dplyr::n_distinct(Nome[is.finite(ESG_SCORE)]),
      ESG_medio = media_sicura(ESG_SCORE),
      ESG_Disclosure_medio = media_sicura(ESG_DISCLOSURE_SCORE),
      Environmental_medio = media_sicura(ENVIRONMENTAL_SCORE),
      Social_medio = media_sicura(SOCIAL_SCORE),
      Governance_medio = media_sicura(GOVERNANCE_SCORE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(ESG_medio)) %>%
    dplyr::mutate(Posizione_ESG_2024 = dplyr::row_number()) %>%
    dplyr::select(Posizione_ESG_2024, dplyr::everything())
  
  readr::write_csv(
    riepilogo_settori_2024,
    file.path(
      cartelle_settori$tabelle,
      "riepilogo_indicatori_settoriali_2024.csv"
    )
  )
}


# ==============================================================================
# 6. REPLICA METODOLOGICA D'AMATO ET AL. - SEZIONE 4.2.1
# ==============================================================================

if (ESEGUI_RANDOM_FOREST) {
  dati_modello_rf <- panel %>%
    dplyr::filter(
      Anno >= ANNO_INIZIALE,
      Anno <= ANNO_FINALE_ESG
    ) %>%
    dplyr::transmute(
      Nome,
      Settore = factor(Settore),
      Year = Anno,
      ESG.Score = ESG_SCORE * 10,
      ROA = RETURN_ON_ASSET,
      EBIT = EBIT,
      Tobin.Q = TOBIN_Q_RATIO,
      ROE = RETURN_COM_EQY,
      DY = DIVIDEND_INDICATED_YIELD
    ) %>%
    dplyr::filter(
      !is.na(Nome),
      !is.na(Settore),
      dplyr::if_all(
        c(Year, ESG.Score, ROA, EBIT, Tobin.Q, ROE, DY),
        ~ is.finite(.x)
      )
    ) %>%
    droplevels()
  
  if (nrow(dati_modello_rf) < 100) {
    stop(
      paste0(
        "Il campione completo della Random Forest contiene soltanto ",
        nrow(dati_modello_rf), " osservazioni."
      )
    )
  }
  
  riepilogo_campione_rf <- tibble::tibble(
    Osservazioni = nrow(dati_modello_rf),
    Imprese = dplyr::n_distinct(dati_modello_rf$Nome),
    Settori = nlevels(dati_modello_rf$Settore),
    Anno_iniziale = min(dati_modello_rf$Year),
    Anno_finale = max(dati_modello_rf$Year),
    ESG_medio_0_100 = mean(dati_modello_rf$ESG.Score),
    ESG_deviazione_standard_0_100 = stats::sd(dati_modello_rf$ESG.Score)
  )
  
  readr::write_csv(
    riepilogo_campione_rf,
    file.path(cartelle_rf$tabelle, "01_riepilogo_campione.csv")
  )
  readr::write_csv(
    dati_modello_rf,
    file.path(cartelle_rf$tabelle, "02_dataset_modello_completo.csv")
  )
  
  variabili_correlazione_rf <- dati_modello_rf %>%
    dplyr::select(ESG.Score, ROA, EBIT, ROE, Year, Tobin.Q, DY)
  
  matrice_correlazioni_rf <- stats::cor(
    variabili_correlazione_rf,
    use = "complete.obs",
    method = "pearson"
  )
  
  readr::write_csv(
    as.data.frame(matrice_correlazioni_rf) %>%
      tibble::rownames_to_column("Variabile"),
    file.path(cartelle_rf$tabelle, "03_matrice_correlazioni.csv")
  )
  
  salva_grafico_base(
    function() {
      corrplot::corrplot(
        matrice_correlazioni_rf,
        method = "color",
        type = "upper",
        order = "original",
        addCoef.col = "black",
        number.cex = 0.8,
        tl.col = "black",
        tl.srt = 45,
        diag = FALSE
      )
    },
    cartelle_rf$grafici,
    "correlogramma_variabili",
    larghezza = 10,
    altezza = 8
  )
  
  set.seed(RF_SEED_SPLIT)
  indici_training_rf <- sample(
    seq_len(nrow(dati_modello_rf)),
    size = floor(RF_QUOTA_TRAINING * nrow(dati_modello_rf)),
    replace = FALSE
  )
  indici_test_rf <- setdiff(
    seq_len(nrow(dati_modello_rf)),
    indici_training_rf
  )
  
  dati_training_rf <- dati_modello_rf[indici_training_rf, , drop = FALSE]
  dati_test_rf <- dati_modello_rf[indici_test_rf, , drop = FALSE]
  
  X_completo_rf <- stats::model.matrix(
    ~ Year + ROA + EBIT + Tobin.Q + ROE + DY + Settore - 1,
    data = dati_modello_rf
  )
  colnames(X_completo_rf) <- make.names(
    colnames(X_completo_rf),
    unique = TRUE
  )
  
  X_training_rf <- X_completo_rf[indici_training_rf, , drop = FALSE]
  X_test_rf <- X_completo_rf[indici_test_rf, , drop = FALSE]
  y_completo_rf <- dati_modello_rf$ESG.Score
  y_training_rf <- y_completo_rf[indici_training_rf]
  y_test_rf <- y_completo_rf[indici_test_rf]
  
  numerosita_settori_training <- table(dati_training_rf$Settore)
  if (any(numerosita_settori_training == 0)) {
    stop(
      "Almeno un settore non e rappresentato nel training set RF. Modificare RF_SEED_SPLIT."
    )
  }
  
  mtry_iniziale_rf <- max(1, floor(ncol(X_training_rf) / 3))
  
  cat(
    "\nRandom Forest: tuning di mtry e nodesize in corso...\n"
  )
  
  tuning_mtry_nodesize_rf <- purrr::map_dfr(
    RF_NODESIZE_GRID,
    function(nodesize_corrente) {
      set.seed(RF_SEED_SPLIT)
      
      risultato_tune_rf <- randomForest::tuneRF(
        x = X_training_rf,
        y = y_training_rf,
        mtryStart = mtry_iniziale_rf,
        ntreeTry = RF_NUMERO_ALBERI,
        stepFactor = 1.5,
        improve = 0.01,
        trace = FALSE,
        plot = FALSE,
        doBest = FALSE,
        nodesize = nodesize_corrente,
        importance = TRUE
      )
      
      tibble::tibble(
        nodesize = nodesize_corrente,
        mtry = as.integer(risultato_tune_rf[, 1]),
        OOB_MSE = as.numeric(risultato_tune_rf[, 2])
      )
    }
  )
  
  migliore_combinazione_rf <- tuning_mtry_nodesize_rf %>%
    dplyr::arrange(OOB_MSE, nodesize, mtry) %>%
    dplyr::slice_head(n = 1)
  
  mtry_ottimale_rf <- migliore_combinazione_rf$mtry[[1]]
  nodesize_ottimale_rf <- migliore_combinazione_rf$nodesize[[1]]
  
  readr::write_csv(
    tuning_mtry_nodesize_rf,
    file.path(cartelle_rf$tabelle, "04_tuning_mtry_nodesize.csv")
  )
  
  cat(
    "Random Forest: valutazione dei ", RF_NUMERO_SEED_TUNING,
    " seed in corso...\n",
    sep = ""
  )
  
  tuning_seed_rf <- purrr::map_dfr(
    seq_len(RF_NUMERO_SEED_TUNING),
    function(seed_corrente) {
      set.seed(seed_corrente)
      
      modello_temporaneo_rf <- randomForest::randomForest(
        x = X_training_rf,
        y = y_training_rf,
        ntree = RF_NUMERO_ALBERI,
        mtry = mtry_ottimale_rf,
        nodesize = nodesize_ottimale_rf,
        importance = TRUE,
        keep.forest = FALSE
      )
      
      tibble::tibble(
        Seed = seed_corrente,
        OOB_MSE = tail(modello_temporaneo_rf$mse, 1),
        OOB_R2 = tail(modello_temporaneo_rf$rsq, 1)
      )
    }
  )
  
  miglior_seed_rf <- tuning_seed_rf %>%
    dplyr::arrange(OOB_MSE, dplyr::desc(OOB_R2)) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::pull(Seed)
  
  parametri_finali_rf <- tibble::tibble(
    Numero_alberi = RF_NUMERO_ALBERI,
    Mtry = mtry_ottimale_rf,
    Nodesize = nodesize_ottimale_rf,
    Seed = miglior_seed_rf
  )
  
  readr::write_csv(
    tuning_seed_rf,
    file.path(cartelle_rf$tabelle, "05_tuning_seed.csv")
  )
  readr::write_csv(
    parametri_finali_rf,
    file.path(cartelle_rf$tabelle, "06_parametri_finali_random_forest.csv")
  )
  
  set.seed(miglior_seed_rf)
  modello_rf <- randomForest::randomForest(
    x = X_training_rf,
    y = y_training_rf,
    ntree = RF_NUMERO_ALBERI,
    mtry = mtry_ottimale_rf,
    nodesize = nodesize_ottimale_rf,
    importance = TRUE,
    keep.forest = TRUE
  )
  
  saveRDS(
    modello_rf,
    file.path(cartelle_rf$modelli, "modello_random_forest.rds")
  )
  
  risultati_oob_rf <- tibble::tibble(
    MSE_OOB = tail(modello_rf$mse, 1),
    Varianza_spiegata_percentuale = tail(modello_rf$rsq, 1) * 100
  )
  
  readr::write_csv(
    risultati_oob_rf,
    file.path(cartelle_rf$tabelle, "07_risultati_OOB_random_forest.csv")
  )
  
  modello_glm <- stats::glm(
    ESG.Score ~ Year + ROA + EBIT + Tobin.Q + ROE + DY + Settore,
    data = dati_training_rf,
    family = stats::gaussian(link = "identity")
  )
  
  saveRDS(
    modello_glm,
    file.path(cartelle_rf$modelli, "modello_GLM.rds")
  )
  
  coefficienti_glm <- summary(modello_glm)$coefficients %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Variabile") %>%
    tibble::as_tibble() %>%
    dplyr::rename(
      Coefficiente = Estimate,
      Errore_standard = `Std. Error`,
      Statistica_t = `t value`,
      P_value = `Pr(>|t|)`
    )
  
  readr::write_csv(
    coefficienti_glm,
    file.path(cartelle_rf$tabelle, "08_coefficienti_GLM.csv")
  )
  capture.output(
    summary(modello_glm),
    file = file.path(cartelle_rf$modelli, "summary_GLM.txt")
  )
  
  previsione_rf_training <- stats::predict(
    modello_rf,
    newdata = X_training_rf
  )
  previsione_rf_test <- stats::predict(modello_rf, newdata = X_test_rf)
  previsione_glm_training <- stats::predict(
    modello_glm,
    newdata = dati_training_rf,
    type = "response"
  )
  previsione_glm_test <- stats::predict(
    modello_glm,
    newdata = dati_test_rf,
    type = "response"
  )
  
  metriche_modelli_rf <- dplyr::bind_rows(
    tibble::tibble(
      Campione = "Training",
      Modello = "Random Forest",
      RMSE = rmse(y_training_rf, previsione_rf_training),
      MAPE = mape(y_training_rf, previsione_rf_training)
    ),
    tibble::tibble(
      Campione = "Training",
      Modello = "GLM",
      RMSE = rmse(y_training_rf, previsione_glm_training),
      MAPE = mape(y_training_rf, previsione_glm_training)
    ),
    tibble::tibble(
      Campione = "Test",
      Modello = "Random Forest",
      RMSE = rmse(y_test_rf, previsione_rf_test),
      MAPE = mape(y_test_rf, previsione_rf_test)
    ),
    tibble::tibble(
      Campione = "Test",
      Modello = "GLM",
      RMSE = rmse(y_test_rf, previsione_glm_test),
      MAPE = mape(y_test_rf, previsione_glm_test)
    )
  )
  
  predizioni_test_rf <- tibble::tibble(
    Nome = dati_test_rf$Nome,
    Settore = dati_test_rf$Settore,
    Year = dati_test_rf$Year,
    ESG_osservato = y_test_rf,
    ESG_previsto_RF = as.numeric(previsione_rf_test),
    ESG_previsto_GLM = as.numeric(previsione_glm_test)
  )
  
  readr::write_csv(
    metriche_modelli_rf,
    file.path(cartelle_rf$tabelle, "09_confronto_RF_GLM_RMSE_MAPE.csv")
  )
  readr::write_csv(
    predizioni_test_rf,
    file.path(cartelle_rf$tabelle, "10_predizioni_test_RF_GLM.csv")
  )
  
  importanza_matrice_rf <- randomForest::importance(modello_rf, type = 2)
  importanza_variabili_rf <- tibble::tibble(
    Variabile = rownames(importanza_matrice_rf),
    IncNodePurity = as.numeric(importanza_matrice_rf[, 1])
  ) %>%
    dplyr::arrange(dplyr::desc(IncNodePurity)) %>%
    dplyr::mutate(
      Etichetta = Variabile %>%
        stringr::str_replace("^Settore", "Settore: ") %>%
        stringr::str_replace_all("\\.", " ")
    )
  
  readr::write_csv(
    importanza_variabili_rf,
    file.path(cartelle_rf$tabelle, "11_importanza_variabili_IncNodePurity.csv")
  )
  
  grafico_importanza_rf <- importanza_variabili_rf %>%
    dplyr::mutate(
      Etichetta = stats::reorder(Etichetta, IncNodePurity)
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = Etichetta, y = IncNodePurity)
    ) +
    ggplot2::geom_col(fill = "gray35") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Importanza delle variabili nella Random Forest",
      subtitle = paste0(
        "Riduzione dell'impurita dei nodi - replica adattata di ",
        "D'Amato et al."
      ),
      x = NULL,
      y = "IncNodePurity"
    ) +
    tema_tesi +
    ggplot2::theme(legend.position = "none")
  
  salva_grafico(
    grafico_importanza_rf,
    cartelle_rf$grafici,
    "figura_4_2_1_importanza_variabili_random_forest",
    larghezza = 11,
    altezza = 8
  )
  
  dati_grafico_previsioni_rf <- predizioni_test_rf %>%
    dplyr::select(
      ESG_osservato,
      `Random Forest` = ESG_previsto_RF,
      GLM = ESG_previsto_GLM
    ) %>%
    tidyr::pivot_longer(
      cols = c(`Random Forest`, GLM),
      names_to = "Modello",
      values_to = "ESG_previsto"
    )
  
  grafico_previsioni_rf <- ggplot2::ggplot(
    dati_grafico_previsioni_rf,
    ggplot2::aes(x = ESG_osservato, y = ESG_previsto, color = Modello)
  ) +
    ggplot2::geom_point(alpha = 0.45, size = 1.4) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::facet_wrap(~ Modello) +
    ggplot2::labs(
      title = "ESG Score osservato e previsto nel test set",
      subtitle = "Confronto tra Random Forest e GLM",
      x = "ESG Score osservato",
      y = "ESG Score previsto",
      color = NULL
    ) +
    tema_tesi +
    ggplot2::theme(legend.position = "none")
  
  salva_grafico(
    grafico_previsioni_rf,
    cartelle_rf$grafici,
    "supporto_ESG_osservato_previsto_RF_GLM",
    larghezza = 11,
    altezza = 6
  )
  
  cat("\nReplica Random Forest completata. Campione:\n")
  print(riepilogo_campione_rf)
  cat("\nConfronto Random Forest - GLM:\n")
  print(metriche_modelli_rf)
  cat("\nPrime variabili per importanza:\n")
  print(
    importanza_variabili_rf %>%
      dplyr::select(Etichetta, IncNodePurity) %>%
      head(10)
  )
}


# ==============================================================================
# 7. CORRELAZIONI ESG, EBIT E TOBIN'S Q - SEZIONE 4.2.2
# ==============================================================================

if (ESEGUI_CORRELAZIONI) {
  panel_correlazioni <- panel %>%
    dplyr::filter(
      Anno >= ANNO_INIZIALE,
      Anno <= ANNO_FINALE_ESG
    )
  
  correlazione_esg_ebit <- test_cor_sicuro(
    panel_correlazioni$ESG_SCORE,
    panel_correlazioni$EBIT_w
  ) %>%
    dplyr::mutate(Indicatore = "EBIT", .before = 1)
  
  correlazione_esg_tobin <- test_cor_sicuro(
    panel_correlazioni$ESG_SCORE,
    panel_correlazioni$TOBIN_Q_RATIO_w
  ) %>%
    dplyr::mutate(Indicatore = "Tobin's Q", .before = 1)
  
  correlazioni_complessive <- dplyr::bind_rows(
    correlazione_esg_ebit,
    correlazione_esg_tobin
  )
  
  readr::write_csv(
    correlazioni_complessive,
    file.path(
      cartelle_correlazioni$tabelle,
      "01_correlazioni_complessive_ESG_EBIT_Tobin.csv"
    )
  )
  
  dati_scatter_correlazioni <- dplyr::bind_rows(
    panel_correlazioni %>%
      dplyr::transmute(
        ESG_SCORE,
        Indicatore = "EBIT",
        Valore = EBIT_w
      ),
    panel_correlazioni %>%
      dplyr::transmute(
        ESG_SCORE,
        Indicatore = "Tobin's Q",
        Valore = TOBIN_Q_RATIO_w
      )
  ) %>%
    dplyr::filter(is.finite(ESG_SCORE), is.finite(Valore)) %>%
    dplyr::mutate(
      Indicatore = factor(Indicatore, levels = c("EBIT", "Tobin's Q"))
    )
  
  grafico_scatter_correlazioni <- ggplot2::ggplot(
    dati_scatter_correlazioni,
    ggplot2::aes(x = ESG_SCORE, y = Valore)
  ) +
    ggplot2::geom_point(alpha = 0.23, size = 0.85, color = "gray30") +
    ggplot2::geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = TRUE,
      linewidth = 1,
      color = "#2C7FB8"
    ) +
    ggplot2::facet_wrap(
      ~ Indicatore,
      nrow = 1,
      scales = "free_y"
    ) +
    ggplot2::labs(
      title = "Relazione lineare tra ESG Score, EBIT e Tobin's Q",
      subtitle = "Periodo 2015-2024; indicatori finanziari winsorizzati all'1% e al 99%",
      x = "ESG Score",
      y = NULL
    ) +
    tema_tesi +
    ggplot2::theme(
      legend.position = "none",
      strip.text = ggplot2::element_text(face = "bold")
    )
  
  salva_grafico(
    grafico_scatter_correlazioni,
    cartelle_correlazioni$grafici,
    "figura_4_2_2_relazione_ESG_EBIT_Tobin",
    larghezza = 11,
    altezza = 6
  )
  
  correlazioni_annuali <- panel_correlazioni %>%
    dplyr::group_by(Anno) %>%
    dplyr::summarise(
      Correlazione_ESG_EBIT = cor_sicura(ESG_SCORE, EBIT_w),
      Correlazione_ESG_Tobin = cor_sicura(ESG_SCORE, TOBIN_Q_RATIO_w),
      N_ESG_EBIT = sum(is.finite(ESG_SCORE) & is.finite(EBIT_w)),
      N_ESG_Tobin = sum(
        is.finite(ESG_SCORE) & is.finite(TOBIN_Q_RATIO_w)
      ),
      .groups = "drop"
    )
  
  readr::write_csv(
    correlazioni_annuali,
    file.path(
      cartelle_correlazioni$tabelle,
      "02_correlazioni_annuali_ESG_EBIT_Tobin.csv"
    )
  )
  
  dati_correlazioni_annuali_plot <- correlazioni_annuali %>%
    dplyr::select(
      Anno,
      EBIT = Correlazione_ESG_EBIT,
      `Tobin's Q` = Correlazione_ESG_Tobin
    ) %>%
    tidyr::pivot_longer(
      cols = -Anno,
      names_to = "Indicatore",
      values_to = "Correlazione"
    ) %>%
    dplyr::mutate(
      Indicatore = factor(Indicatore, levels = c("EBIT", "Tobin's Q"))
    )
  
  grafico_correlazioni_annuali <- ggplot2::ggplot(
    dati_correlazioni_annuali_plot,
    ggplot2::aes(
      x = Anno,
      y = Correlazione,
      color = Indicatore,
      group = Indicatore
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "gray45"
    ) +
    ggplot2::scale_x_continuous(breaks = ANNO_INIZIALE:ANNO_FINALE_ESG) +
    ggplot2::labs(
      title = "Evoluzione annuale delle correlazioni tra ESG Score, EBIT e Tobin's Q",
      x = "Anno",
      y = "Correlazione di Pearson",
      color = NULL
    ) +
    tema_tesi
  
  salva_grafico(
    grafico_correlazioni_annuali,
    cartelle_correlazioni$grafici,
    "figura_4_2_3_correlazioni_annuali_ESG_EBIT_Tobin",
    larghezza = 10,
    altezza = 6
  )
  
  correlazioni_macro_settore <- panel_correlazioni %>%
    dplyr::group_by(Macro_Settore) %>%
    dplyr::summarise(
      ESG_EBIT = cor_sicura(ESG_SCORE, EBIT_w),
      ESG_TobinQ = cor_sicura(ESG_SCORE, TOBIN_Q_RATIO_w),
      N_ESG_EBIT = sum(is.finite(ESG_SCORE) & is.finite(EBIT_w)),
      N_ESG_Tobin = sum(
        is.finite(ESG_SCORE) & is.finite(TOBIN_Q_RATIO_w)
      ),
      .groups = "drop"
    )
  
  readr::write_csv(
    correlazioni_macro_settore,
    file.path(
      cartelle_correlazioni$tabelle,
      "03_correlazioni_di_supporto_per_macro_settore.csv"
    )
  )
  
  cat("\nCorrelazioni complessive 2015-2024:\n")
  print(correlazioni_complessive)
}


# ==============================================================================
# 8. XGBOOST DELTA-HORIZON - SEZIONE 4.3.1
# ==============================================================================

crea_matrici_xgb <- function(train, valid, test, feature_cols) {
  n_train <- nrow(train)
  n_valid <- nrow(valid)
  n_test <- nrow(test)
  
  train_x <- train %>% dplyr::select(dplyr::all_of(feature_cols))
  valid_x <- valid %>% dplyr::select(dplyr::all_of(feature_cols))
  test_x <- test %>% dplyr::select(dplyr::all_of(feature_cols))
  tutti_x <- dplyr::bind_rows(train_x, valid_x, test_x)
  
  colonne_numeriche <- setdiff(feature_cols, "Settore")
  mediane_training <- purrr::map_dbl(
    colonne_numeriche,
    function(variabile) {
      mediana <- stats::median(train[[variabile]], na.rm = TRUE)
      ifelse(is.finite(mediana), mediana, 0)
    }
  )
  names(mediane_training) <- colonne_numeriche
  
  for (variabile in colonne_numeriche) {
    tutti_x[[variabile]][is.na(tutti_x[[variabile]])] <-
      mediane_training[[variabile]]
  }
  
  tutti_x$Settore <- as.character(tutti_x$Settore)
  tutti_x$Settore[
    is.na(tutti_x$Settore) | tutti_x$Settore == ""
  ] <- "Settore_mancante"
  tutti_x$Settore <- factor(tutti_x$Settore)
  
  X <- Matrix::sparse.model.matrix(~ . - 1, data = tutti_x)
  
  ind_train <- seq_len(n_train)
  ind_valid <- n_train + seq_len(n_valid)
  ind_test <- n_train + n_valid + seq_len(n_test)
  
  list(
    X_train = X[ind_train, , drop = FALSE],
    X_valid = X[ind_valid, , drop = FALSE],
    X_test = X[ind_test, , drop = FALSE],
    mediane_training = mediane_training
  )
}

estrai_log_xgb <- function(modello) {
  log_raw <- attr(modello, "evaluation_log", exact = TRUE)
  if (is.null(log_raw)) {
    log_raw <- attributes(modello)[["evaluation_log"]]
  }
  log_raw
}

xgb_boost_delta_horizon <- function(
    dati,
    variabili_correnti,
    horizon,
    ultimo_anno_target,
    cartelle_output,
    n_configurazioni = 24,
    nrounds_max = 2000,
    early_stopping = 75,
    seed = 123
) {
  cat("\n====================================================\n")
  cat("XGBOOST - ORIZZONTE:", horizon, "anno/i\n")
  cat("====================================================\n")
  
  futuro <- dati %>%
    dplyr::select(Nome, Anno, ESG_FUTURO = ESG_SCORE) %>%
    dplyr::mutate(Anno = Anno - horizon)
  
  dataset_modello <- dati %>%
    dplyr::left_join(futuro, by = c("Nome", "Anno")) %>%
    dplyr::mutate(
      Anno_target = Anno + horizon,
      DELTA_ESG_TARGET = ESG_FUTURO - ESG_SCORE
    ) %>%
    dplyr::filter(
      Anno_target <= ultimo_anno_target,
      is.finite(ESG_SCORE),
      is.finite(ESG_FUTURO),
      is.finite(DELTA_ESG_TARGET)
    )
  
  anno_test <- ultimo_anno_target
  anno_validazione <- ultimo_anno_target - 1
  
  train <- dataset_modello %>%
    dplyr::filter(Anno_target <= anno_validazione - 1)
  valid <- dataset_modello %>%
    dplyr::filter(Anno_target == anno_validazione)
  test <- dataset_modello %>%
    dplyr::filter(Anno_target == anno_test)
  
  cat(
    "Campioni: train=", nrow(train),
    ", valid=", nrow(valid),
    ", test=", nrow(test), "\n",
    sep = ""
  )
  
  if (nrow(train) < 200 || nrow(valid) < 50 || nrow(test) < 50) {
    stop(
      paste0(
        "Campione insufficiente per horizon = ", horizon,
        ". Train=", nrow(train),
        ", valid=", nrow(valid),
        ", test=", nrow(test)
      )
    )
  }
  
  lag_cols <- paste0(variabili_correnti, "_L1")
  delta_cols <- paste0("DELTA1_", variabili_correnti)
  feature_cols <- c(
    "Settore",
    "Anno",
    variabili_correnti,
    lag_cols,
    delta_cols
  )
  feature_cols <- intersect(feature_cols, names(dataset_modello))
  
  matrici <- crea_matrici_xgb(
    train = train,
    valid = valid,
    test = test,
    feature_cols = feature_cols
  )
  
  dtrain <- xgboost::xgb.DMatrix(
    data = matrici$X_train,
    label = train$DELTA_ESG_TARGET,
    missing = NA
  )
  dvalid <- xgboost::xgb.DMatrix(
    data = matrici$X_valid,
    label = valid$DELTA_ESG_TARGET,
    missing = NA
  )
  dtest <- xgboost::xgb.DMatrix(
    data = matrici$X_test,
    label = test$DELTA_ESG_TARGET,
    missing = NA
  )
  
  griglia_completa <- tidyr::expand_grid(
    max_depth = c(2L, 3L, 4L, 5L),
    eta = c(0.02, 0.05, 0.10),
    min_child_weight = c(3, 7, 12),
    subsample = c(0.75, 0.90),
    colsample_bytree = c(0.70, 0.90)
  )
  
  set.seed(seed + horizon)
  griglia <- griglia_completa %>%
    dplyr::slice_sample(
      n = min(n_configurazioni, nrow(griglia_completa))
    )
  
  risultati_tuning <- vector("list", nrow(griglia))
  
  for (i in seq_len(nrow(griglia))) {
    cat(
      "Configurazione ", i, "/", nrow(griglia),
      " - horizon ", horizon, "\n",
      sep = ""
    )
    
    parametri <- list(
      booster = "gbtree",
      objective = "reg:squarederror",
      eval_metric = "rmse",
      tree_method = "hist",
      max_depth = as.integer(griglia$max_depth[[i]]),
      eta = as.numeric(griglia$eta[[i]]),
      min_child_weight = as.numeric(griglia$min_child_weight[[i]]),
      subsample = as.numeric(griglia$subsample[[i]]),
      colsample_bytree = as.numeric(griglia$colsample_bytree[[i]]),
      reg_lambda = 5,
      reg_alpha = 0.10,
      nthread = N_THREAD,
      seed = seed + i + 100 * horizon
    )
    
    modello_i <- tryCatch(
      xgboost::xgb.train(
        params = parametri,
        data = dtrain,
        nrounds = nrounds_max,
        evals = list(train = dtrain, valid = dvalid),
        early_stopping_rounds = early_stopping,
        verbose = 0
      ),
      error = function(e) {
        warning(
          paste0(
            "Configurazione ", i,
            " non stimata: ", conditionMessage(e)
          )
        )
        NULL
      }
    )
    
    if (is.null(modello_i)) {
      risultati_tuning[[i]] <- dplyr::bind_cols(
        tibble::tibble(
          Configurazione = i,
          Best_iteration = NA_integer_,
          RMSE = NA_real_,
          MAE = NA_real_,
          R2 = NA_real_,
          Correlazione = NA_real_,
          Stato = "Errore nella stima"
        ),
        griglia[i, ]
      )
      next
    }
    
    log_raw <- estrai_log_xgb(modello_i)
    
    if (is.null(log_raw) || NROW(log_raw) == 0L) {
      risultati_tuning[[i]] <- dplyr::bind_cols(
        tibble::tibble(
          Configurazione = i,
          Best_iteration = NA_integer_,
          RMSE = NA_real_,
          MAE = NA_real_,
          R2 = NA_real_,
          Correlazione = NA_real_,
          Stato = paste0(
            "evaluation_log assente. Attributi: ",
            paste(names(attributes(modello_i)), collapse = ", ")
          )
        ),
        griglia[i, ]
      )
      next
    }
    
    log_i <- as.data.frame(log_raw)
    colonna_rmse_valid <- grep(
      "^valid.*rmse$",
      names(log_i),
      value = TRUE,
      ignore.case = TRUE
    )
    
    if (length(colonna_rmse_valid) != 1L) {
      risultati_tuning[[i]] <- dplyr::bind_cols(
        tibble::tibble(
          Configurazione = i,
          Best_iteration = NA_integer_,
          RMSE = NA_real_,
          MAE = NA_real_,
          R2 = NA_real_,
          Correlazione = NA_real_,
          Stato = paste0(
            "Colonna RMSE validazione non identificata. Colonne: ",
            paste(names(log_i), collapse = ", ")
          )
        ),
        griglia[i, ]
      )
      next
    }
    
    valori_rmse_log <- as.numeric(log_i[[colonna_rmse_valid]])
    righe_finite <- which(is.finite(valori_rmse_log))
    
    if (length(righe_finite) == 0L) {
      risultati_tuning[[i]] <- dplyr::bind_cols(
        tibble::tibble(
          Configurazione = i,
          Best_iteration = NA_integer_,
          RMSE = NA_real_,
          MAE = NA_real_,
          R2 = NA_real_,
          Correlazione = NA_real_,
          Stato = "RMSE interno non finito"
        ),
        griglia[i, ]
      )
      next
    }
    
    best_iteration_i <- righe_finite[
      which.min(valori_rmse_log[righe_finite])
    ]
    rmse_interno_i <- valori_rmse_log[[best_iteration_i]]
    
    pred_valid_i <- tryCatch(
      as.numeric(
        stats::predict(
          modello_i,
          dvalid,
          iterationrange = c(1L, as.integer(best_iteration_i)),
          strict_shape = FALSE
        )
      ),
      error = function(e) {
        warning(
          paste0(
            "Predizione di validazione fallita nella configurazione ",
            i, ": ", conditionMessage(e)
          )
        )
        numeric(0)
      }
    )
    
    predizione_valida <-
      length(pred_valid_i) == nrow(valid) && all(is.finite(pred_valid_i))
    
    if (predizione_valida) {
      metriche_valid_i <- metriche_regressione(
        valid$DELTA_ESG_TARGET,
        pred_valid_i
      )
      metriche_valid_i$RMSE <- rmse_interno_i
      stato_i <- "OK"
    } else {
      metriche_valid_i <- tibble::tibble(
        RMSE = rmse_interno_i,
        MAE = NA_real_,
        R2 = NA_real_,
        Correlazione = NA_real_
      )
      stato_i <- paste0(
        "RMSE interno disponibile; predizione non valida (n=",
        length(pred_valid_i), ")"
      )
    }
    
    risultati_tuning[[i]] <- dplyr::bind_cols(
      tibble::tibble(
        Configurazione = i,
        Best_iteration = as.integer(best_iteration_i)
      ),
      griglia[i, ],
      metriche_valid_i,
      tibble::tibble(Stato = stato_i)
    )
  }
  
  tabella_tuning_completa <- dplyr::bind_rows(risultati_tuning)
  
  readr::write_csv(
    tabella_tuning_completa,
    file.path(
      cartelle_output$tabelle,
      paste0("tuning_completo_horizon_", horizon, ".csv")
    )
  )
  
  tabella_tuning <- tabella_tuning_completa %>%
    dplyr::filter(
      is.finite(RMSE),
      is.finite(Best_iteration),
      Best_iteration >= 1L
    ) %>%
    dplyr::arrange(RMSE, MAE)
  
  if (nrow(tabella_tuning) == 0L) {
    stop(
      paste0(
        "Il tuning XGBoost non ha prodotto risultati validi per horizon = ",
        horizon, "."
      )
    )
  }
  
  migliore_riga <- tabella_tuning %>% dplyr::slice_head(n = 1L)
  best_iteration <- as.integer(migliore_riga$Best_iteration[[1]])
  
  if (
    length(best_iteration) != 1L ||
    is.na(best_iteration) ||
    !is.finite(best_iteration) ||
    best_iteration < 1L
  ) {
    stop(
      paste0(
        "Best_iteration non valida per horizon = ", horizon,
        ": ", paste(best_iteration, collapse = ", ")
      )
    )
  }
  
  readr::write_csv(
    tabella_tuning,
    file.path(
      cartelle_output$tabelle,
      paste0("tuning_horizon_", horizon, ".csv")
    )
  )
  
  cat("\nMigliore configurazione di validazione:\n")
  print(migliore_riga)
  
  X_train_finale <- rbind(matrici$X_train, matrici$X_valid)
  y_train_finale <- c(
    train$DELTA_ESG_TARGET,
    valid$DELTA_ESG_TARGET
  )
  
  dtrain_finale <- xgboost::xgb.DMatrix(
    data = X_train_finale,
    label = y_train_finale,
    missing = NA
  )
  
  parametri_finali <- list(
    booster = "gbtree",
    objective = "reg:squarederror",
    eval_metric = "rmse",
    tree_method = "hist",
    max_depth = as.integer(migliore_riga$max_depth[[1]]),
    eta = as.numeric(migliore_riga$eta[[1]]),
    min_child_weight = as.numeric(migliore_riga$min_child_weight[[1]]),
    subsample = as.numeric(migliore_riga$subsample[[1]]),
    colsample_bytree = as.numeric(migliore_riga$colsample_bytree[[1]]),
    reg_lambda = 5,
    reg_alpha = 0.10,
    nthread = N_THREAD,
    seed = seed + 1000 + horizon
  )
  
  modello_finale <- xgboost::xgb.train(
    params = parametri_finali,
    data = dtrain_finale,
    nrounds = max(1, best_iteration),
    verbose = 0
  )
  
  delta_predetto <- as.numeric(
    stats::predict(
      modello_finale,
      dtest,
      iterationrange = c(1L, as.integer(best_iteration)),
      strict_shape = FALSE
    )
  )
  
  if (length(delta_predetto) != nrow(test)) {
    stop(
      paste0(
        "Numero di predizioni finali errato per horizon = ", horizon,
        ": attese ", nrow(test),
        ", ottenute ", length(delta_predetto)
      )
    )
  }
  
  if (!all(is.finite(delta_predetto))) {
    stop(
      paste0(
        "Le predizioni finali contengono NA/NaN/Inf per horizon = ",
        horizon
      )
    )
  }
  
  predizioni <- test %>%
    dplyr::transmute(
      Nome,
      Settore,
      Anno_base = Anno,
      Anno_target,
      ESG_iniziale = ESG_SCORE,
      ESG_reale = ESG_FUTURO,
      Delta_ESG_reale = DELTA_ESG_TARGET,
      Delta_ESG_predetto = delta_predetto,
      ESG_predetto_XGB = ESG_SCORE + delta_predetto,
      ESG_predetto_naive = ESG_SCORE,
      Errore_XGB = ESG_predetto_XGB - ESG_reale,
      Errore_naive = ESG_predetto_naive - ESG_reale
    )
  
  metriche_delta_xgb <- metriche_regressione(
    predizioni$Delta_ESG_reale,
    predizioni$Delta_ESG_predetto
  )
  metriche_livello_xgb <- metriche_regressione(
    predizioni$ESG_reale,
    predizioni$ESG_predetto_XGB
  )
  metriche_livello_naive <- metriche_regressione(
    predizioni$ESG_reale,
    predizioni$ESG_predetto_naive
  )
  
  accuratezza_direzione <- mean(
    sign(predizioni$Delta_ESG_reale) ==
      sign(predizioni$Delta_ESG_predetto)
  )
  
  miglioramento_rmse_pct <- if (
    is.finite(metriche_livello_naive$RMSE) &&
    metriche_livello_naive$RMSE > 0
  ) {
    100 * (
      metriche_livello_naive$RMSE - metriche_livello_xgb$RMSE
    ) / metriche_livello_naive$RMSE
  } else {
    NA_real_
  }
  
  metriche_finali <- tibble::tibble(
    Horizon = horizon,
    Anno_validazione = anno_validazione,
    Anno_test = anno_test,
    N_train = nrow(train),
    N_valid = nrow(valid),
    N_test = nrow(test),
    Best_iteration = best_iteration,
    RMSE_delta_XGB = metriche_delta_xgb$RMSE,
    MAE_delta_XGB = metriche_delta_xgb$MAE,
    R2_delta_XGB = metriche_delta_xgb$R2,
    Cor_delta_XGB = metriche_delta_xgb$Correlazione,
    Accuratezza_direzione_delta = accuratezza_direzione,
    RMSE_ESG_XGB = metriche_livello_xgb$RMSE,
    MAE_ESG_XGB = metriche_livello_xgb$MAE,
    R2_ESG_XGB = metriche_livello_xgb$R2,
    Cor_ESG_XGB = metriche_livello_xgb$Correlazione,
    RMSE_ESG_naive = metriche_livello_naive$RMSE,
    MAE_ESG_naive = metriche_livello_naive$MAE,
    R2_ESG_naive = metriche_livello_naive$R2,
    Cor_ESG_naive = metriche_livello_naive$Correlazione,
    Miglioramento_RMSE_pct = miglioramento_rmse_pct,
    XGB_batte_naive = metriche_livello_xgb$RMSE <
      metriche_livello_naive$RMSE
  )
  
  readr::write_csv(
    predizioni,
    file.path(
      cartelle_output$tabelle,
      paste0("predizioni_horizon_", horizon, ".csv")
    )
  )
  readr::write_csv(
    metriche_finali,
    file.path(
      cartelle_output$tabelle,
      paste0("metriche_horizon_", horizon, ".csv")
    )
  )
  
  importanza <- xgboost::xgb.importance(
    feature_names = colnames(X_train_finale),
    model = modello_finale
  )
  
  readr::write_csv(
    tibble::as_tibble(importanza),
    file.path(
      cartelle_output$tabelle,
      paste0("importanza_horizon_", horizon, ".csv")
    )
  )
  
  shap_matrix <- as.matrix(
    stats::predict(
      modello_finale,
      dtest,
      predcontrib = TRUE,
      iterationrange = c(1L, as.integer(best_iteration)),
      strict_shape = FALSE
    )
  )
  
  if (ncol(shap_matrix) != ncol(matrici$X_test) + 1L) {
    stop(
      paste0(
        "Numero inatteso di colonne SHAP per horizon = ", horizon,
        "."
      )
    )
  }
  
  colnames(shap_matrix) <- c(colnames(matrici$X_test), "BIAS")
  
  shap_importanza <- tibble::tibble(
    Feature = colnames(shap_matrix)[colnames(shap_matrix) != "BIAS"],
    Mean_abs_SHAP = colMeans(
      abs(
        shap_matrix[
          ,
          colnames(shap_matrix) != "BIAS",
          drop = FALSE
        ]
      )
    )
  ) %>%
    dplyr::arrange(dplyr::desc(Mean_abs_SHAP))
  
  readr::write_csv(
    shap_importanza,
    file.path(
      cartelle_output$tabelle,
      paste0("shap_importanza_horizon_", horizon, ".csv")
    )
  )
  
  risultati_settore <- predizioni %>%
    dplyr::group_by(Settore) %>%
    dplyr::summarise(
      N = dplyr::n(),
      MAE_ESG_XGB = mean(abs(Errore_XGB)),
      RMSE_ESG_XGB = sqrt(mean(Errore_XGB^2)),
      Bias_XGB = mean(Errore_XGB),
      MAE_ESG_naive = mean(abs(Errore_naive)),
      .groups = "drop"
    ) %>%
    dplyr::arrange(MAE_ESG_XGB)
  
  readr::write_csv(
    risultati_settore,
    file.path(
      cartelle_output$tabelle,
      paste0("risultati_settore_horizon_", horizon, ".csv")
    )
  )
  
  grafico_livelli <- ggplot2::ggplot(
    predizioni,
    ggplot2::aes(x = ESG_reale, y = ESG_predetto_XGB)
  ) +
    ggplot2::geom_point(alpha = 0.55) +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed"
    ) +
    ggplot2::labs(
      title = paste0("ESG reale vs previsto - horizon ", horizon),
      subtitle = paste0("Test temporale sull'anno ", anno_test),
      x = "ESG reale",
      y = "ESG previsto da XGBoost"
    ) +
    tema_tesi +
    ggplot2::theme(legend.position = "none")
  
  salva_grafico(
    grafico_livelli,
    cartelle_output$grafici,
    paste0("grafico_reale_previsto_horizon_", horizon),
    larghezza = 8,
    altezza = 6
  )
  
  grafico_delta <- ggplot2::ggplot(
    predizioni,
    ggplot2::aes(x = Delta_ESG_reale, y = Delta_ESG_predetto)
  ) +
    ggplot2::geom_point(alpha = 0.55) +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed"
    ) +
    ggplot2::labs(
      title = paste0("Delta ESG reale vs previsto - horizon ", horizon),
      x = "Delta ESG reale",
      y = "Delta ESG previsto"
    ) +
    tema_tesi +
    ggplot2::theme(legend.position = "none")
  
  salva_grafico(
    grafico_delta,
    cartelle_output$grafici,
    paste0("grafico_delta_horizon_", horizon),
    larghezza = 8,
    altezza = 6
  )
  
  top_importanza <- tibble::as_tibble(importanza) %>%
    dplyr::slice_head(n = 20)
  
  grafico_importanza_xgb <- ggplot2::ggplot(
    top_importanza,
    ggplot2::aes(
      x = stats::reorder(Feature, Gain),
      y = Gain
    )
  ) +
    ggplot2::geom_col(fill = "gray35") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste0("Importanza XGBoost - horizon ", horizon),
      x = NULL,
      y = "Gain"
    ) +
    tema_tesi +
    ggplot2::theme(legend.position = "none")
  
  salva_grafico(
    grafico_importanza_xgb,
    cartelle_output$grafici,
    paste0("grafico_importanza_horizon_", horizon),
    larghezza = 9,
    altezza = 7
  )
  
  grafico_shap <- shap_importanza %>%
    dplyr::slice_head(n = 20) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = stats::reorder(Feature, Mean_abs_SHAP),
        y = Mean_abs_SHAP
      )
    ) +
    ggplot2::geom_col(fill = "gray35") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste0("Importanza media SHAP - horizon ", horizon),
      x = NULL,
      y = "Media del valore SHAP assoluto"
    ) +
    tema_tesi +
    ggplot2::theme(legend.position = "none")
  
  salva_grafico(
    grafico_shap,
    cartelle_output$grafici,
    paste0("grafico_SHAP_horizon_", horizon),
    larghezza = 9,
    altezza = 7
  )
  
  grafico_errori_settore <- ggplot2::ggplot(
    predizioni,
    ggplot2::aes(
      x = stats::reorder(Settore, abs(Errore_XGB), FUN = stats::median),
      y = Errore_XGB
    )
  ) +
    ggplot2::geom_boxplot(outlier.alpha = 0.25) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste0("Errori previsivi per settore - horizon ", horizon),
      x = NULL,
      y = "ESG previsto - ESG reale"
    ) +
    tema_tesi +
    ggplot2::theme(legend.position = "none")
  
  salva_grafico(
    grafico_errori_settore,
    cartelle_output$grafici,
    paste0("grafico_errori_settore_horizon_", horizon),
    larghezza = 10,
    altezza = 7
  )
  
  saveRDS(
    modello_finale,
    file.path(
      cartelle_output$modelli,
      paste0("modello_xgb_horizon_", horizon, ".rds")
    )
  )
  
  print(metriche_finali)
  
  list(
    metriche = metriche_finali,
    predizioni = predizioni,
    tuning = tabella_tuning,
    importanza = importanza,
    shap_importanza = shap_importanza,
    risultati_settore = risultati_settore,
    modello = modello_finale
  )
}

if (ESEGUI_XGBOOST) {
  variabili_finanziarie_xgb <- c(
    "TOBIN_Q_RATIO",
    "RETURN_COM_EQY",
    "RETURN_ON_ASSET",
    "EBIT",
    "DIVIDEND_INDICATED_YIELD"
  )
  variabili_disclosure_xgb <- c(
    "ESG_DISCLOSURE_SCORE",
    "ENVIRON_DISCLOSURE_SCORE",
    "SOCIAL_DISCLOSURE_SCORE",
    "GOVNCE_DISCLOSURE_SCORE"
  )
  variabili_pilastri_xgb <- c(
    "ENVIRONMENTAL_SCORE",
    "SOCIAL_SCORE",
    "GOVERNANCE_SCORE"
  )
  
  variabili_correnti_xgb <- c(
    "ESG_SCORE",
    variabili_finanziarie_xgb,
    variabili_disclosure_xgb
  )
  
  if (XGB_INCLUDI_PILASTRI) {
    variabili_correnti_xgb <- c(
      variabili_correnti_xgb,
      variabili_pilastri_xgb
    )
  }
  
  dati_feature_xgb <- panel %>%
    dplyr::group_by(Nome) %>%
    dplyr::arrange(Anno, .by_group = TRUE) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(variabili_correnti_xgb),
        ~ dplyr::lag(.x, n = 1L),
        .names = "{.col}_L1"
      ),
      dplyr::across(
        dplyr::all_of(variabili_correnti_xgb),
        ~ .x - dplyr::lag(.x, n = 1L),
        .names = "DELTA1_{.col}"
      )
    ) %>%
    dplyr::ungroup()
  
  risultati_modelli_xgb <- purrr::map(
    XGB_ORIZZONTI,
    ~ xgb_boost_delta_horizon(
      dati = dati_feature_xgb,
      variabili_correnti = variabili_correnti_xgb,
      horizon = .x,
      ultimo_anno_target = XGB_ULTIMO_ANNO_TARGET,
      cartelle_output = cartelle_xgb,
      n_configurazioni = XGB_N_CONFIGURAZIONI,
      nrounds_max = XGB_NROUNDS_MAX,
      early_stopping = XGB_EARLY_STOPPING,
      seed = 123
    )
  )
  
  names(risultati_modelli_xgb) <- paste0(
    "horizon_",
    XGB_ORIZZONTI
  )
  
  confronto_orizzonti_xgb <- dplyr::bind_rows(
    purrr::map(risultati_modelli_xgb, "metriche")
  ) %>%
    dplyr::arrange(Horizon)
  
  readr::write_csv(
    confronto_orizzonti_xgb,
    file.path(
      cartelle_xgb$tabelle,
      "confronto_finale_orizzonti.csv"
    )
  )
  
  cat("\nConfronto finale degli orizzonti XGBoost:\n")
  print(confronto_orizzonti_xgb)
}


# ==============================================================================
# 9. ANALISI ARIMA DELL'ESG SCORE MEDIO - SEZIONE 4.3.2
# ==============================================================================

etichetta_modello_arima <- function(modello) {
  p <- modello$arma[1]
  q <- modello$arma[2]
  d <- modello$arma[6]
  presenza_drift <- "drift" %in% names(stats::coef(modello))
  
  paste0(
    "ARIMA(", p, ",", d, ",", q, ")",
    ifelse(presenza_drift, " con drift", "")
  )
}

stima_arima_sicura <- function(serie, ordine, drift = FALSE) {
  tryCatch(
    suppressWarnings(
      forecast::Arima(
        serie,
        order = ordine,
        include.drift = drift,
        method = "ML"
      )
    ),
    error = function(e) NULL
  )
}

stima_auto_arima_aicc <- function(serie) {
  forecast::auto.arima(
    serie,
    d = 1,
    max.p = 1,
    max.q = 1,
    max.P = 0,
    max.Q = 0,
    max.order = 2,
    seasonal = FALSE,
    stationary = FALSE,
    allowdrift = TRUE,
    allowmean = FALSE,
    stepwise = FALSE,
    approximation = FALSE,
    ic = "aicc",
    method = "ML"
  )
}

if (ESEGUI_ARIMA) {
  disponibilita_esg_arima <- panel %>%
    dplyr::filter(Anno %in% ARIMA_ANNI_OSSERVATI) %>%
    dplyr::group_by(Anno) %>%
    dplyr::summarise(
      N_totale = dplyr::n(),
      N_osservati = sum(is.finite(ESG_SCORE)),
      N_mancanti = sum(!is.finite(ESG_SCORE)),
      Percentuale_osservati = 100 * N_osservati / N_totale,
      .groups = "drop"
    ) %>%
    dplyr::arrange(Anno)
  
  esg_anno_arima <- panel %>%
    dplyr::filter(Anno %in% ARIMA_ANNI_OSSERVATI) %>%
    dplyr::group_by(Anno) %>%
    dplyr::summarise(
      ESG_SCORE = media_sicura(ESG_SCORE),
      N_osservazioni = sum(is.finite(ESG_SCORE)),
      .groups = "drop"
    ) %>%
    dplyr::arrange(Anno)
  
  if (!identical(esg_anno_arima$Anno, as.integer(ARIMA_ANNI_OSSERVATI))) {
    stop("La serie ARIMA non contiene esattamente gli anni dal 2015 al 2024.")
  }
  
  if (anyNA(esg_anno_arima$ESG_SCORE)) {
    stop("Almeno una media annuale ESG necessaria per ARIMA non e calcolabile.")
  }
  
  statistiche_descrittive_arima <- tibble::tibble(
    Periodo = paste0(
      min(ARIMA_ANNI_OSSERVATI), "-", max(ARIMA_ANNI_OSSERVATI)
    ),
    Media = mean(esg_anno_arima$ESG_SCORE),
    Varianza = stats::var(esg_anno_arima$ESG_SCORE),
    Minimo = min(esg_anno_arima$ESG_SCORE),
    Anno_minimo = esg_anno_arima$Anno[
      which.min(esg_anno_arima$ESG_SCORE)
    ],
    Massimo = max(esg_anno_arima$ESG_SCORE),
    Anno_massimo = esg_anno_arima$Anno[
      which.max(esg_anno_arima$ESG_SCORE)
    ],
    Variazione_complessiva =
      dplyr::last(esg_anno_arima$ESG_SCORE) -
      dplyr::first(esg_anno_arima$ESG_SCORE)
  )
  
  esg_ts <- stats::ts(
    esg_anno_arima$ESG_SCORE,
    start = min(esg_anno_arima$Anno),
    frequency = 1
  )
  esg_diff <- diff(esg_ts)
  
  grafico_serie_arima <- ggplot2::ggplot(
    esg_anno_arima,
    ggplot2::aes(x = Anno, y = ESG_SCORE)
  ) +
    ggplot2::geom_line(linewidth = 0.9, color = "#1F4E79") +
    ggplot2::geom_point(size = 2.4, color = "#1F4E79") +
    ggplot2::scale_x_continuous(breaks = ARIMA_ANNI_OSSERVATI) +
    ggplot2::labs(
      title = "Andamento annuale dell'ESG Score medio",
      subtitle = "Serie aggregata delle imprese dello STOXX Europe 600",
      x = "Anno",
      y = "ESG Score medio"
    ) +
    ggplot2::theme_bw(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "gray30"),
      legend.position = "bottom"
    )
  
  salva_grafico(
    grafico_serie_arima,
    cartelle_arima$grafici,
    "figura_01_serie_esg_media",
    larghezza = 10,
    altezza = 6
  )
  
  salva_grafico_base(
    function() {
      forecast::ggtsdisplay(
        esg_ts,
        lag.max = 4,
        main = "ESG Score medio: serie, ACF e PACF"
      )
    },
    cartelle_arima$grafici,
    "figura_02_diagnostica_serie_originale",
    larghezza = 10,
    altezza = 7
  )
  
  salva_grafico_base(
    function() {
      forecast::ggtsdisplay(
        esg_diff,
        lag.max = 3,
        main = "Prima differenza dell'ESG Score medio: serie, ACF e PACF"
      )
    },
    cartelle_arima$grafici,
    "figura_03_diagnostica_prima_differenza",
    larghezza = 10,
    altezza = 7
  )
  
  esg_train_2024 <- stats::window(esg_ts, end = 2023)
  esg_test_2024 <- stats::window(
    esg_ts,
    start = ARIMA_ANNO_TEST_INDICATIVO,
    end = ARIMA_ANNO_TEST_INDICATIVO
  )
  valore_reale_2024 <- as.numeric(esg_test_2024)
  
  specifiche_candidate_arima <- tibble::tibble(
    Modello = c(
      "Random walk",
      "Random walk con drift",
      "ARIMA(1,1,0)",
      "ARIMA(1,1,0) con drift",
      "ARIMA(0,1,1)",
      "ARIMA(0,1,1) con drift",
      "ARIMA(1,1,1)",
      "ARIMA(1,1,1) con drift"
    ),
    p = c(0, 0, 1, 1, 0, 0, 1, 1),
    d = rep(1, 8),
    q = c(0, 0, 0, 0, 1, 1, 1, 1),
    Drift = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE)
  )
  
  modelli_candidati_arima <- vector(
    "list",
    nrow(specifiche_candidate_arima)
  )
  names(modelli_candidati_arima) <- specifiche_candidate_arima$Modello
  
  for (i in seq_len(nrow(specifiche_candidate_arima))) {
    modelli_candidati_arima[[i]] <- stima_arima_sicura(
      serie = esg_train_2024,
      ordine = c(
        specifiche_candidate_arima$p[i],
        specifiche_candidate_arima$d[i],
        specifiche_candidate_arima$q[i]
      ),
      drift = specifiche_candidate_arima$Drift[i]
    )
  }
  
  modelli_candidati_arima <- modelli_candidati_arima[
    !vapply(modelli_candidati_arima, is.null, logical(1))
  ]
  
  if (length(modelli_candidati_arima) == 0) {
    stop("Nessun modello ARIMA candidato e stato stimato correttamente.")
  }
  
  confronto_modelli_2024 <- dplyr::bind_rows(
    lapply(names(modelli_candidati_arima), function(nome_modello) {
      modello <- modelli_candidati_arima[[nome_modello]]
      previsione <- forecast::forecast(
        modello,
        h = 1,
        level = c(80, 95)
      )
      previsto <- as.numeric(previsione$mean[1])
      
      tibble::tibble(
        Modello = nome_modello,
        AIC = modello$aic,
        AICc = modello$aicc,
        BIC = modello$bic,
        Osservato_2024 = valore_reale_2024,
        Previsto_2024 = previsto,
        Errore = valore_reale_2024 - previsto,
        Errore_assoluto = abs(valore_reale_2024 - previsto),
        Errore_quadratico = (valore_reale_2024 - previsto)^2,
        Lower_80 = as.numeric(previsione$lower[1, "80%"]),
        Upper_80 = as.numeric(previsione$upper[1, "80%"]),
        Lower_95 = as.numeric(previsione$lower[1, "95%"]),
        Upper_95 = as.numeric(previsione$upper[1, "95%"]),
        Copertura_95 =
          valore_reale_2024 >= previsione$lower[1, "95%"] &
          valore_reale_2024 <= previsione$upper[1, "95%"]
      )
    })
  ) %>%
    dplyr::arrange(AICc)
  
  modello_auto_2024 <- stima_auto_arima_aicc(esg_train_2024)
  previsione_auto_2024 <- forecast::forecast(
    modello_auto_2024,
    h = 1,
    level = c(80, 95)
  )
  previsto_auto_2024 <- as.numeric(previsione_auto_2024$mean[1])
  
  risultato_auto_2024 <- tibble::tibble(
    Training = "2015-2023",
    Test = "2024 - dato parzialmente aggiornato",
    Modello_selezionato = etichetta_modello_arima(modello_auto_2024),
    AIC = modello_auto_2024$aic,
    AICc = modello_auto_2024$aicc,
    BIC = modello_auto_2024$bic,
    Osservato_2024 = valore_reale_2024,
    Previsto_2024 = previsto_auto_2024,
    Errore = valore_reale_2024 - previsto_auto_2024,
    MAE = abs(valore_reale_2024 - previsto_auto_2024),
    RMSE = sqrt((valore_reale_2024 - previsto_auto_2024)^2),
    Lower_80 = as.numeric(previsione_auto_2024$lower[1, "80%"]),
    Upper_80 = as.numeric(previsione_auto_2024$upper[1, "80%"]),
    Lower_95 = as.numeric(previsione_auto_2024$lower[1, "95%"]),
    Upper_95 = as.numeric(previsione_auto_2024$upper[1, "95%"]),
    Copertura_95 =
      valore_reale_2024 >= previsione_auto_2024$lower[1, "95%"] &
      valore_reale_2024 <= previsione_auto_2024$upper[1, "95%"]
  )
  
  punti_test_2024 <- tibble::tibble(
    Anno = c(2024, 2024),
    ESG_SCORE = c(previsto_auto_2024, valore_reale_2024),
    Tipo = c("Previsione ARIMA", "Valore osservato")
  )
  
  grafico_test_2024 <- ggplot2::ggplot(
    esg_anno_arima %>% dplyr::filter(Anno <= 2023),
    ggplot2::aes(x = Anno, y = ESG_SCORE)
  ) +
    ggplot2::geom_line(linewidth = 0.9, color = "gray30") +
    ggplot2::geom_point(size = 2.2, color = "gray30") +
    ggplot2::geom_errorbar(
      data = tibble::tibble(
        Anno = 2024,
        Lower_95 = as.numeric(previsione_auto_2024$lower[1, "95%"]),
        Upper_95 = as.numeric(previsione_auto_2024$upper[1, "95%"])
      ),
      ggplot2::aes(x = Anno, ymin = Lower_95, ymax = Upper_95),
      inherit.aes = FALSE,
      width = 0.10,
      linewidth = 0.7,
      color = "#6BAED6"
    ) +
    ggplot2::geom_errorbar(
      data = tibble::tibble(
        Anno = 2024,
        Lower_80 = as.numeric(previsione_auto_2024$lower[1, "80%"]),
        Upper_80 = as.numeric(previsione_auto_2024$upper[1, "80%"])
      ),
      ggplot2::aes(x = Anno, ymin = Lower_80, ymax = Upper_80),
      inherit.aes = FALSE,
      width = 0.18,
      linewidth = 1.2,
      color = "#2171B5"
    ) +
    ggplot2::geom_point(
      data = punti_test_2024,
      ggplot2::aes(x = Anno, y = ESG_SCORE, color = Tipo, shape = Tipo),
      inherit.aes = FALSE,
      size = 3.2
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Previsione ARIMA" = "#08519C",
        "Valore osservato" = "#CB181D"
      )
    ) +
    ggplot2::scale_shape_manual(
      values = c("Previsione ARIMA" = 16, "Valore osservato" = 17)
    ) +
    ggplot2::scale_x_continuous(breaks = ARIMA_ANNI_OSSERVATI) +
    ggplot2::labs(
      title = "Previsione a un passo dell'ESG Score medio nel 2024",
      subtitle = paste0(
        etichetta_modello_arima(modello_auto_2024),
        " selezionato mediante AICc; intervalli all'80% e al 95%"
      ),
      x = "Anno",
      y = "ESG Score medio",
      color = NULL,
      shape = NULL
    ) +
    ggplot2::theme_bw(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "gray30"),
      legend.position = "bottom"
    )
  
  salva_grafico(
    grafico_test_2024,
    cartelle_arima$grafici,
    "figura_04_test_previsione_2024",
    larghezza = 10,
    altezza = 6
  )
  
  risultati_rolling_lista <- lapply(
    ARIMA_ANNI_VALIDAZIONE_ROLLING,
    function(anno_target) {
      anno_fine_training <- anno_target - 1
      
      valori_training <- esg_anno_arima %>%
        dplyr::filter(Anno <= anno_fine_training) %>%
        dplyr::pull(ESG_SCORE)
      
      serie_training <- stats::ts(
        valori_training,
        start = min(esg_anno_arima$Anno),
        frequency = 1
      )
      
      modello_rolling <- tryCatch(
        stima_auto_arima_aicc(serie_training),
        error = function(e) NULL
      )
      
      if (is.null(modello_rolling)) return(NULL)
      
      previsione_rolling <- forecast::forecast(
        modello_rolling,
        h = 1,
        level = c(80, 95)
      )
      
      osservato <- esg_anno_arima$ESG_SCORE[
        esg_anno_arima$Anno == anno_target
      ]
      previsto_arima <- as.numeric(previsione_rolling$mean[1])
      previsto_random_walk <- dplyr::last(valori_training)
      
      tibble::tibble(
        Anno_target = anno_target,
        Training = paste0("2015-", anno_fine_training),
        N_training = length(valori_training),
        Modello_ARIMA = etichetta_modello_arima(modello_rolling),
        AICc = modello_rolling$aicc,
        Osservato = osservato,
        Previsto_ARIMA = previsto_arima,
        Errore_ARIMA = osservato - previsto_arima,
        Errore_assoluto_ARIMA = abs(osservato - previsto_arima),
        Previsto_random_walk = previsto_random_walk,
        Errore_random_walk = osservato - previsto_random_walk,
        Errore_assoluto_random_walk = abs(
          osservato - previsto_random_walk
        )
      )
    }
  )
  
  risultati_rolling_lista <- risultati_rolling_lista[
    !vapply(risultati_rolling_lista, is.null, logical(1))
  ]
  
  if (length(risultati_rolling_lista) > 0) {
    validazione_rolling <- dplyr::bind_rows(risultati_rolling_lista)
    
    sintesi_validazione_rolling <- tibble::tibble(
      Modello = c("ARIMA selezionato via AICc", "Random walk"),
      N_previsioni = nrow(validazione_rolling),
      MAE = c(
        mean(validazione_rolling$Errore_assoluto_ARIMA),
        mean(validazione_rolling$Errore_assoluto_random_walk)
      ),
      RMSE = c(
        sqrt(mean(validazione_rolling$Errore_ARIMA^2)),
        sqrt(mean(validazione_rolling$Errore_random_walk^2))
      )
    )
  } else {
    validazione_rolling <- tibble::tibble(
      Nota = "Validazione rolling non disponibile."
    )
    sintesi_validazione_rolling <- tibble::tibble(
      Nota = "Sintesi della validazione rolling non disponibile."
    )
  }
  
  modello_finale_arima <- stima_auto_arima_aicc(esg_ts)
  
  sintesi_modello_finale_arima <- tibble::tibble(
    Periodo_stima = "2015-2024",
    N_osservazioni = length(esg_ts),
    Modello_selezionato = etichetta_modello_arima(modello_finale_arima),
    AIC = modello_finale_arima$aic,
    AICc = modello_finale_arima$aicc,
    BIC = modello_finale_arima$bic,
    Varianza_residua = modello_finale_arima$sigma2
  )
  
  coefficienti_finali_arima <- stats::coef(modello_finale_arima)
  
  if (length(coefficienti_finali_arima) > 0) {
    errori_standard_finali_arima <- sqrt(
      diag(modello_finale_arima$var.coef)
    )
    
    tabella_coefficienti_finali_arima <- tibble::tibble(
      Coefficiente = names(coefficienti_finali_arima),
      Stima = as.numeric(coefficienti_finali_arima),
      Errore_standard = as.numeric(errori_standard_finali_arima)
    )
  } else {
    tabella_coefficienti_finali_arima <- tibble::tibble(
      Coefficiente = "Nessun coefficiente AR, MA o drift",
      Stima = NA_real_,
      Errore_standard = NA_real_
    )
  }
  
  residui_finali_arima <- stats::residuals(modello_finale_arima)
  residui_finali_arima <- residui_finali_arima[
    is.finite(residui_finali_arima)
  ]
  
  gradi_liberta_modello_arima <- sum(
    modello_finale_arima$arma[c(1, 2, 3, 4)]
  )
  
  lag_ljung_box <- min(
    length(residui_finali_arima) - 1,
    max(
      gradi_liberta_modello_arima + 3,
      min(10, floor(length(residui_finali_arima) / 5))
    )
  )
  
  if (lag_ljung_box <= gradi_liberta_modello_arima) {
    stop("Osservazioni insufficienti per eseguire il test di Ljung-Box.")
  }
  
  test_ljung_box <- stats::Box.test(
    residui_finali_arima,
    lag = lag_ljung_box,
    type = "Ljung-Box",
    fitdf = gradi_liberta_modello_arima
  )
  
  tabella_ljung_box <- tibble::tibble(
    Modello = etichetta_modello_arima(modello_finale_arima),
    Lag = lag_ljung_box,
    Gradi_liberta_modello = gradi_liberta_modello_arima,
    Statistica_Q = as.numeric(test_ljung_box$statistic),
    Gradi_liberta_test = as.numeric(test_ljung_box$parameter),
    P_value = as.numeric(test_ljung_box$p.value)
  )
  
  salva_grafico_base(
    function() {
      forecast::checkresiduals(
        modello_finale_arima,
        lag = lag_ljung_box
      )
    },
    cartelle_arima$grafici,
    "figura_05_diagnostica_residui_modello_finale",
    larghezza = 10,
    altezza = 7
  )
  
  ultimo_anno_arima <- max(esg_anno_arima$Anno)
  orizzonte_previsione_arima <-
    ARIMA_ULTIMO_ANNO_PREVISIONE - ultimo_anno_arima
  
  previsioni_arima <- forecast::forecast(
    modello_finale_arima,
    h = orizzonte_previsione_arima,
    level = c(80, 95)
  )
  
  tabella_previsioni_arima <- tibble::tibble(
    Anno = (ultimo_anno_arima + 1):ARIMA_ULTIMO_ANNO_PREVISIONE,
    Previsione = as.numeric(previsioni_arima$mean),
    Lower_80 = as.numeric(previsioni_arima$lower[, "80%"]),
    Upper_80 = as.numeric(previsioni_arima$upper[, "80%"]),
    Lower_95 = as.numeric(previsioni_arima$lower[, "95%"]),
    Upper_95 = as.numeric(previsioni_arima$upper[, "95%"])
  ) %>%
    dplyr::mutate(
      Previsione_fuori_scala_0_10 = Previsione < 0 | Previsione > 10,
      Intervallo_95_fuori_scala_0_10 = Lower_95 < 0 | Upper_95 > 10
    )
  
  previsioni_anni_richiesti_arima <- tabella_previsioni_arima %>%
    dplyr::filter(Anno %in% ARIMA_ANNI_PREVISIONE_RICHIESTI)
  
  crea_grafico_previsioni_arima <- function(anno_finale, titolo) {
    previsioni_grafico <- tabella_previsioni_arima %>%
      dplyr::filter(Anno <= anno_finale)
    
    anni_asse <- sort(
      unique(
        c(
          2015,
          2020,
          2024,
          2025,
          2030,
          if (anno_finale >= 2040) {
            seq(2040, anno_finale, by = 10)
          } else {
            numeric(0)
          }
        )
      )
    )
    anni_asse <- anni_asse[anni_asse <= anno_finale]
    
    ggplot2::ggplot() +
      ggplot2::geom_ribbon(
        data = previsioni_grafico,
        ggplot2::aes(x = Anno, ymin = Lower_95, ymax = Upper_95),
        fill = "#9ECAE1",
        alpha = 0.40
      ) +
      ggplot2::geom_ribbon(
        data = previsioni_grafico,
        ggplot2::aes(x = Anno, ymin = Lower_80, ymax = Upper_80),
        fill = "#4292C6",
        alpha = 0.45
      ) +
      ggplot2::geom_line(
        data = esg_anno_arima,
        ggplot2::aes(x = Anno, y = ESG_SCORE),
        linewidth = 0.9,
        color = "gray20"
      ) +
      ggplot2::geom_point(
        data = esg_anno_arima,
        ggplot2::aes(x = Anno, y = ESG_SCORE),
        size = 2.2,
        color = "gray20"
      ) +
      ggplot2::geom_line(
        data = previsioni_grafico,
        ggplot2::aes(x = Anno, y = Previsione),
        linewidth = 1,
        color = "#08519C"
      ) +
      ggplot2::geom_point(
        data = previsioni_grafico,
        ggplot2::aes(x = Anno, y = Previsione),
        size = 1.8,
        color = "#08519C"
      ) +
      ggplot2::geom_vline(
        xintercept = ultimo_anno_arima + 0.5,
        linetype = 2,
        color = "gray40"
      ) +
      ggplot2::geom_hline(
        yintercept = c(0, 10),
        linetype = 3,
        color = "gray65"
      ) +
      ggplot2::scale_x_continuous(breaks = anni_asse) +
      ggplot2::labs(
        title = titolo,
        subtitle = paste0(
          etichetta_modello_arima(modello_finale_arima),
          "; aree scure e chiare: intervalli all'80% e al 95%"
        ),
        x = "Anno",
        y = "ESG Score medio"
      ) +
      ggplot2::theme_bw(base_size = 13) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        plot.subtitle = ggplot2::element_text(color = "gray30"),
        legend.position = "bottom"
      )
  }
  
  grafico_previsioni_2030 <- crea_grafico_previsioni_arima(
    2030,
    "Previsioni dell'ESG Score medio fino al 2030"
  )
  grafico_previsioni_2050 <- crea_grafico_previsioni_arima(
    2050,
    "Previsioni dell'ESG Score medio fino al 2050"
  )
  
  salva_grafico(
    grafico_previsioni_2030,
    cartelle_arima$grafici,
    "figura_06_previsioni_fino_2030",
    larghezza = 10,
    altezza = 6
  )
  salva_grafico(
    grafico_previsioni_2050,
    cartelle_arima$grafici,
    "figura_07_previsioni_fino_2050",
    larghezza = 11,
    altezza = 6.5
  )
  
  readr::write_csv(
    audit_aggiornamento %>%
      dplyr::filter(Anno %in% c(2024, 2025)),
    file.path(cartelle_arima$tabelle, "01_controllo_aggiornamento_dati.csv")
  )
  readr::write_csv(
    disponibilita_esg_arima,
    file.path(cartelle_arima$tabelle, "02_disponibilita_esg_annuale.csv")
  )
  readr::write_csv(
    esg_anno_arima,
    file.path(cartelle_arima$tabelle, "03_serie_esg_media_annuale.csv")
  )
  readr::write_csv(
    statistiche_descrittive_arima,
    file.path(cartelle_arima$tabelle, "04_statistiche_descrittive.csv")
  )
  readr::write_csv(
    confronto_modelli_2024,
    file.path(
      cartelle_arima$tabelle,
      "05_confronto_modelli_AICc_test_2024.csv"
    )
  )
  readr::write_csv(
    risultato_auto_2024,
    file.path(
      cartelle_arima$tabelle,
      "06_risultato_auto_arima_test_2024.csv"
    )
  )
  readr::write_csv(
    validazione_rolling,
    file.path(
      cartelle_arima$tabelle,
      "07_validazione_rolling_2021_2023.csv"
    )
  )
  readr::write_csv(
    sintesi_validazione_rolling,
    file.path(
      cartelle_arima$tabelle,
      "08_sintesi_validazione_rolling.csv"
    )
  )
  readr::write_csv(
    sintesi_modello_finale_arima,
    file.path(cartelle_arima$tabelle, "09_sintesi_modello_finale.csv")
  )
  readr::write_csv(
    tabella_coefficienti_finali_arima,
    file.path(cartelle_arima$tabelle, "10_coefficienti_modello_finale.csv")
  )
  readr::write_csv(
    tabella_ljung_box,
    file.path(cartelle_arima$tabelle, "11_test_ljung_box.csv")
  )
  readr::write_csv(
    tabella_previsioni_arima,
    file.path(cartelle_arima$tabelle, "12_previsioni_2025_2050.csv")
  )
  readr::write_csv(
    previsioni_anni_richiesti_arima,
    file.path(
      cartelle_arima$tabelle,
      "13_previsioni_2025_2030_2050.csv"
    )
  )
  
  saveRDS(
    modello_finale_arima,
    file.path(cartelle_arima$modelli, "modello_ARIMA_finale.rds")
  )
  
  capture.output(
    summary(modello_auto_2024),
    file = file.path(
      cartelle_arima$modelli,
      "summary_modello_test_2024.txt"
    )
  )
  capture.output(
    summary(modello_finale_arima),
    file = file.path(
      cartelle_arima$modelli,
      "summary_modello_ARIMA_finale.txt"
    )
  )
  
  report_risultati_arima <- capture.output({
    cat("============================================================\n")
    cat("ANALISI ARIMA DELL'ESG SCORE MEDIO\n")
    cat("============================================================\n\n")
    cat("1. CONTROLLO DELL'AGGIORNAMENTO DEI DATI\n")
    print(
      audit_aggiornamento %>% dplyr::filter(Anno %in% c(2024, 2025)),
      n = Inf
    )
    cat("\n2. DISPONIBILITA ANNUALE DELL'ESG SCORE\n")
    print(disponibilita_esg_arima, n = Inf)
    cat("\n3. SERIE ESG MEDIA ANNUALE\n")
    print(esg_anno_arima, n = Inf)
    cat("\n4. STATISTICHE DESCRITTIVE\n")
    print(statistiche_descrittive_arima)
    cat("\n5. CONFRONTO MODELLI SUL TRAINING 2015-2023\n")
    print(confronto_modelli_2024, n = Inf)
    cat("\n6. MODELLO AUTOMATICO E TEST INDICATIVO SUL 2024\n")
    print(risultato_auto_2024)
    cat("\n7. VALIDAZIONE ROLLING ORIGIN 2021-2023\n")
    print(validazione_rolling, n = Inf)
    cat("\n8. SINTESI DELLA VALIDAZIONE ROLLING\n")
    print(sintesi_validazione_rolling, n = Inf)
    cat("\n9. MODELLO FINALE STIMATO SUL 2015-2024\n")
    print(sintesi_modello_finale_arima)
    cat("\n10. TEST DI LJUNG-BOX\n")
    print(tabella_ljung_box)
    cat("\n11. PREVISIONI PER GLI ANNI RICHIESTI\n")
    print(previsioni_anni_richiesti_arima, n = Inf)
    cat("\n12. NOTA INTERPRETATIVA\n")
    cat(
      paste0(
        "Le previsioni non sono state tagliate nella scala 0-10. ",
        "Eventuali valori o intervalli esterni alla scala mostrano il limite ",
        "dell'estrapolazione di lungo periodo e vanno letti come scenari ",
        "condizionati, non come stime precise.\n"
      )
    )
  })
  
  writeLines(
    report_risultati_arima,
    con = file.path(
      cartelle_arima$principale,
      "RISULTATI_COMPLETI_ARIMA.txt"
    ),
    useBytes = TRUE
  )
  
  cat("\nSintesi del modello ARIMA finale:\n")
  print(sintesi_modello_finale_arima)
  cat("\nTest di Ljung-Box:\n")
  print(tabella_ljung_box)
}


# ==============================================================================
# 10. INDICE FINALE DEGLI OUTPUT E INFORMAZIONI DI SESSIONE
# ==============================================================================

capture.output(
  utils::sessionInfo(),
  file = file.path(CARTELLA_OUTPUT, "sessionInfo.txt")
)

file_output <- list.files(
  CARTELLA_OUTPUT,
  recursive = TRUE,
  full.names = TRUE,
  include.dirs = FALSE
)

indice_output <- tibble::tibble(
  File = basename(file_output),
  Percorso_relativo = substring(
    file_output,
    nchar(normalizePath(CARTELLA_OUTPUT, winslash = "/")) + 2
  ),
  Estensione = tools::file_ext(file_output),
  Dimensione_byte = file.info(file_output)$size
) %>%
  dplyr::arrange(Percorso_relativo)

readr::write_csv(
  indice_output,
  file.path(CARTELLA_OUTPUT, "INDICE_OUTPUT_GENERATI.csv")
)

cat("\n============================================================\n")
cat("ANALISI COMPLETATE\n")
cat("============================================================\n")
cat("Risultati salvati in:\n", CARTELLA_OUTPUT, "\n", sep = "")
cat("Numero di file prodotti:", nrow(indice_output), "\n")
