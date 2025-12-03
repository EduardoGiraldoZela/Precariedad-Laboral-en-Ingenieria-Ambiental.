packs <- c("tidyverse", "haven", "srvyr")
instalar <- setdiff(packs, rownames(installed.packages()))
if (length(instalar)) install.packages(instalar, dependencies = TRUE)
invisible(lapply(packs, library, character.only = TRUE))

# Función auxiliar: reemplaza NA por 0 en dummies
na0 <- function(x) ifelse(is.na(x), 0, x)

# ==================================================
# 1. RUTAS Y LECTURA DE ARCHIVOS .SAV
# ==================================================
ruta <- "C:/ENAHO/MODULOS"

mod1  <- read_sav(file.path(ruta, "MODULO 1_2011-2024.sav")) %>%
  mutate(AÑO = as.integer(as.character(AÑO)))

mod2  <- read_sav(file.path(ruta, "MODULO 2_2011-2024.sav")) %>%
  mutate(AÑO = as.integer(as.character(AÑO)))

mod3  <- read_sav(file.path(ruta, "MODULO 3_2011-2024.sav")) %>%
  mutate(AÑO = as.integer(as.character(AÑO)))

mod5  <- read_sav(file.path(ruta, "MODULO 5_2011-2024.sav")) %>%
  mutate(AÑO = as.integer(as.character(AÑO)))

# Ajustar nombre EXACTO del archivo según esté en tu carpeta
mod34 <- read_sav(file.path(ruta, "MODULO 34_2011_2024.sav")) %>%
  rename(AÑO = ANIO) %>%
  mutate(AÑO = as.integer(as.character(AÑO)))


# ==================================================
# 2. SELECCIÓN DE VARIABLES ÚTILES EN CADA MÓDULO
#    (para evitar duplicados y basura)
# ==================================================

# --- Módulo 1: vivienda / contexto básico
mod1_sel <- mod1 %>%
  select(
    AÑO, MES, CONGLOME, VIVIENDA, HOGAR,
    UBIGEO, DOMINIO, ESTRATO,
    # FACTOR07 REMOVIDO para evitar duplicado con mod34
    P110, P110A1, P111A,    # agua / saneamiento
    P112A, P113A,           # luz / combustible
    P1141:P1145             # TICs en el hogar
  )

# --- Módulo 2: persona (demografía básica)
mod2_sel <- mod2 %>%
  select(
    AÑO, MES, CONGLOME, VIVIENDA, HOGAR,
    UBIGEO, DOMINIO, ESTRATO,
    CODPERSO,
    FACPOB07,              # peso poblacional
    P207, P208A, P209      # sexo, edad, estado civil
  )

# --- Módulo 3: educación (núcleo para carrera)
mod3_sel <- mod3 %>%
  select(
    AÑO, CONGLOME, VIVIENDA, HOGAR,
    UBIGEO, DOMINIO, ESTRATO,
    CODPERSO,
    P300A,                 # lengua materna (opcional)
    P301A, P301A1,         # nivel educativo + código de carrera
    P301B, P301C, P301D    # año, grado, tipo de centro
  )

# --- Módulo 5: empleo e ingresos individuales
# --- Módulo 5: empleo e ingresos individuales
# --- Módulo 5: empleo e ingresos individuales
mod5_sel <- mod5 %>%
  select(
    AÑO, CONGLOME, VIVIENDA, HOGAR,
    UBIGEO, DOMINIO, ESTRATO,
    CODPERSO,
    OCU500, FAC500A,       # PEA y peso de empleo
    P501, P502, P503,      # ocupación la semana pasada + empleo fijo + negocio propio
    P505R4, P506R4,        # CIUO / CIIU
    P507, P510,            # categoría ocupacional, tipo de empleador
    P511A,                 # tipo de contrato
    P513T, I513T,          # horas trabajadas semana pasada
    P520, I520,            # horas habituales
    P514,                  # otro trabajo
    P521, P521A,           # deseo y disponibilidad de más horas
    P524E1, I524A1,        # ingreso líquido / total imputado
    P524B1,                # descuento de ley (AFP/ONP, etc.)  🔹 (NUEVO)
    P5441A:P5447A, P544T,  # beneficios extraordinarios
    P558A1:P558A5,         # pensión / no afiliado
    P552                   # trabajó antes
  )



# --- Módulo 34: ingresos del hogar / pobreza
mod34_sel <- mod34 %>%
  select(
    AÑO, MES, CONGLOME, VIVIENDA, HOGAR,
    UBIGEO, DOMINIO, ESTRATO,
    FACTOR07,                     # peso hogar (lo dejamos aquí solo)
    INGHOG2D,                     # ingreso neto del hogar
    LINPE, LINEA, POBREZA,        # pobreza monetaria
    LINEAV_RPL, LINEAV, POBREZAV, # vulnerabilidad económica
    ESTRSOCIAL                    # estrato socioeconómico
  )

# ==================================================
# QUITAR LABELS, EVITA WARNINGS
# ==================================================
# mod1_sel  <- haven::zap_labels(mod1_sel)
# mod2_sel  <- haven::zap_labels(mod2_sel)
# mod3_sel  <- haven::zap_labels(mod3_sel)
# mod5_sel  <- haven::zap_labels(mod5_sel)
# mod34_sel <- haven::zap_labels(mod34_sel)

# ==================================================
# 3. UNIÓN DE MÓDULOS (BASE MAESTRA)
# ==================================================
base <- mod2_sel %>%
  left_join(mod3_sel,
            by = c("AÑO","CONGLOME","VIVIENDA","HOGAR",
                   "UBIGEO","DOMINIO","ESTRATO","CODPERSO")) %>%
  left_join(mod5_sel,
            by = c("AÑO","CONGLOME","VIVIENDA","HOGAR",
                   "UBIGEO","DOMINIO","ESTRATO","CODPERSO")) %>%
  left_join(mod34_sel,
            by = c("AÑO","CONGLOME","VIVIENDA","HOGAR",
                   "UBIGEO","DOMINIO","ESTRATO")) %>%
  left_join(mod1_sel,
            by = c("AÑO","CONGLOME","VIVIENDA","HOGAR",
                   "UBIGEO","DOMINIO","ESTRATO"))

# ==================================================
# 4. FILTRAR INGENIERÍA AMBIENTAL (CÓDIGO 521016)
# ==================================================
cod_ambiental <- 521016

# Filtrar carrera
base_iae <- base %>%
  filter(P301A1 == cod_ambiental)

# Filtrar ocupados ENAHO (P501/P502/P503)
base_iae <- base_iae %>%
  mutate(
    ocupado = case_when(
      P501 == 1 ~ 1,
      P502 == 1 ~ 1,
      P503 == 1 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  filter(ocupado == 1)

# ==================================================
# 5. CONSTRUCCIÓN DEL ÍNDICE DE PRECARIEDAD Y Y
# ==================================================

base_iae <- base_iae %>%
  mutate(
    # RMV por año
    rmv_anio = case_when(
      AÑO <= 2011 ~ 600,
      AÑO == 2012 ~ 675,
      AÑO %in% 2013:2015 ~ 750,
      AÑO %in% 2016:2017 ~ 850,
      AÑO %in% 2018:2021 ~ 930,
      AÑO >= 2022 ~ 1025,
      TRUE ~ 930
    ),
    
    salario_mensual = I524A1 / 12,
    
    # 1) Salario bajo: <= 2 RMV
    prec_salario_bajo = if_else(
      salario_mensual <= 2 * rmv_anio,
      1, 0, missing = 0
    ),
    
    # 2) Jornada precaria
    horas_sem = coalesce(I520, P520, I513T, P513T),
    prec_jornada = if_else(
      horas_sem < 34 | horas_sem > 48,
      1, 0, missing = 0
    ),
    
    # 3) Sin contrato escrito (verbal o sin contrato)
    prec_contrato = if_else(
      P511A %in% c(2,3,4),
      1, 0, missing = 0
    ),
    
    # 4) Prestaciones: (beneficios extraordinarios) OR (descuento ley)
    tiene_beneficio_extra = if_else(
      rowSums(across(P5441A:P5447A, ~ .x == 1), na.rm = TRUE) > 0,
      1, 0
    ),
    
    tiene_descuento_ley = if_else(P524B1 == 1, 1, 0, missing = 0),
    
    tiene_prestaciones = if_else(
      tiene_beneficio_extra == 1 | tiene_descuento_ley == 1,
      1, 0
    ),
    
    prec_prestaciones = if_else(tiene_prestaciones == 1, 0, 1),
    
    # 5) No afiliado a pensión
    prec_pension = if_else(P558A5 == 1, 1, 0, missing = 0),
    
    # Índice total
    indice_precariedad =
      na0(prec_salario_bajo) +
      na0(prec_jornada) +
      na0(prec_contrato) +
      na0(prec_prestaciones) +
      na0(prec_pension),
    
    # Variable dependiente
    empleo_no_precarizado = case_when(
      indice_precariedad == 0 ~ 1,
      indice_precariedad > 0 ~ 0,
      TRUE ~ NA_real_
    )
  )



# ==================================================
# 6. RECODIFICAR VARIABLES INDEPENDIENTES CLAVE
# ==================================================
base_iae <- base_iae %>%
  mutate(
    # Sexo
    sexo = factor(P207, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    
    # Edad
    edad = as.numeric(P208A),
    
    # Estado civil (agrupado)
    estado_civil = case_when(
      P209 %in% c(1, 2) ~ "Con_pareja",   # casado / conviviente (ajusta según código ENAHO)
      TRUE              ~ "Sin_pareja"
    ),
    estado_civil = factor(estado_civil),
    
    # Sector de empleo (exposición principal)
    # OJO: revisa cuestionario ENAHO para confirmar códigos de P510
    sector = case_when(
      P510 %in% c(2)             ~ "Publico",
      P510 %in% c(1, 3, 4, 5, 6) ~ "Privado_otros",
      TRUE                       ~ NA_character_
    ),
    sector = factor(sector),
    
    # Pobreza (1 = no pobre; 2 o 3 = pobre)
    pobreza = case_when(
      POBREZA == 1 ~ "No_pobre",
      POBREZA %in% c(2, 3) ~ "Pobre",
      TRUE ~ NA_character_
    ),
    pobreza = factor(pobreza, levels = c("No_pobre", "Pobre")),
    
    # Estrato socioeconómico
    estrato_social = factor(ESTRSOCIAL),
    
    # Área (urbano / rural) – aproximación según ESTRATO
    area = case_when(
      ESTRATO %in% c(1, 2, 3, 4) ~ "Urbano",
      TRUE                      ~ "Rural"
    ),
    area = factor(area)
  )

# ==================================================
# 7. DISEÑO MUESTRAL CON srvyr
# ==================================================
# Quitamos casos sin empleo_no_precarizado
data_desc <- base_iae %>%
  filter(!is.na(empleo_no_precarizado), !is.na(FAC500A))

diseno <- data_desc %>%
  as_survey_design(
    ids = CONGLOME,
    strata = ESTRATO,
    weights = FAC500A,
    nest = TRUE
  )

# ==================================================
# 8. PROCESAMIENTO DESCRIPTIVO
# ==================================================

# 8.1 Prevalencia global de empleo no precarizado
prev_global <- diseno %>%
  summarise(
    prop_no_precarizado = survey_mean(empleo_no_precarizado, vartype = "ci")
  )
prev_global

# 8.2 Prevalencia por sexo
prev_sexo <- diseno %>%
  group_by(sexo) %>%
  summarise(
    prop_no_precarizado = survey_mean(empleo_no_precarizado, vartype = "ci"),
    n = unweighted(n())
  )
prev_sexo

# 8.3 Prevalencia por sector (público vs privado)
prev_sector <- diseno %>%
  group_by(sector) %>%
  summarise(
    prop_no_precarizado = survey_mean(empleo_no_precarizado, vartype = "ci"),
    n = unweighted(n())
  )
prev_sector

# 8.4 Prevalencia por área urbano/rural
prev_area <- diseno %>%
  group_by(area) %>%
  summarise(
    prop_no_precarizado = survey_mean(empleo_no_precarizado, vartype = "ci"),
    n = unweighted(n())
  )
prev_area

# 8.5 Prevalencia por año
prev_anio <- diseno %>%
  group_by(AÑO) %>%
  summarise(
    prop_no_precarizado = survey_mean(empleo_no_precarizado, vartype = "ci"),
    n = unweighted(n())
  )
prev_anio

# 8.6 Ejemplo: tabla cruzada sexo x sector
prev_sexo_sector <- diseno %>%
  group_by(sexo, sector) %>%
  summarise(
    prop_no_precarizado = survey_mean(empleo_no_precarizado, vartype = "ci"),
    n = unweighted(n())
  )
prev_sexo_sector

# 8.7 Prevalencia por dominio geográfico  🔹 (NUEVO)
prev_dominio <- diseno %>%
  group_by(DOMINIO) %>%
  summarise(
    prop_no_precarizado = survey_mean(empleo_no_precarizado, vartype = "ci"),
    n = unweighted(n())
  )
prev_dominio


# =========================
#      GRÁFICOS
# =========================
library(ggplot2)

# ---- Tendencia por año
plot_prev_anio <- prev_anio %>%
  mutate(
    prop = prop_no_precarizado,
    low  = prop_no_precarizado_low,
    upp  = prop_no_precarizado_upp
  )

ggplot(plot_prev_anio, aes(x = AÑO, y = prop)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 3) +
  geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Tendencia del Empleo No Precarizado en Ingeniería Ambiental (2012–2024)",
    x = "Año",
    y = "Prevalencia (%)",
    caption = "Fuente: ENAHO 2011–2024. Elaboración propia."
  ) +
  theme_minimal(base_size = 14)

# ---- Por sexo
plot_prev_sexo <- prev_sexo %>%
  mutate(
    prop = prop_no_precarizado,
    low  = prop_no_precarizado_low,
    upp  = prop_no_precarizado_upp
  )

ggplot(plot_prev_sexo, aes(x = sexo, y = prop, fill = sexo)) +
  geom_col(width = 0.6, alpha = 0.8) +
  geom_errorbar(aes(ymin = low, ymax = upp), width = 0.2, size = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Empleo No Precarizado por Sexo",
    x = "Sexo",
    y = "Prevalencia (%)",
    fill = "Sexo"
  ) +
  theme_minimal(base_size = 14)

# ---- Por sector
plot_prev_sector <- prev_sector %>%
  mutate(
    prop = prop_no_precarizado,
    low  = prop_no_precarizado_low,
    upp  = prop_no_precarizado_upp
  )

ggplot(plot_prev_sector, aes(x = sector, y = prop, fill = sector)) +
  geom_col(width = 0.6, alpha = 0.8) +
  geom_errorbar(aes(ymin = low, ymax = upp), width = 0.2, size = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Empleo No Precarizado por Sector",
    x = "Sector",
    y = "Prevalencia (%)",
    fill = "Sector"
  ) +
  theme_minimal(base_size = 14)

# ---- Por área
plot_prev_area <- prev_area %>%
  mutate(
    prop = prop_no_precarizado,
    low  = prop_no_precarizado_low,
    upp  = prop_no_precarizado_upp
  )

ggplot(plot_prev_area, aes(x = area, y = prop, fill = area)) +
  geom_col(width = 0.6, alpha = 0.8) +
  geom_errorbar(aes(ymin = low, ymax = upp), width = 0.2, size = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Empleo No Precarizado por Área de Residencia",
    x = "Área",
    y = "Prevalencia (%)",
    fill = "Área"
  ) +
  theme_minimal(base_size = 14)

# ---- Por dominio geográfico
plot_prev_dominio <- prev_dominio %>%
  mutate(
    dominio = factor(
      DOMINIO,
      levels = c(1, 2, 3, 4, 5, 6, 7),
      labels = c("Lima Metropolitana", "Resto Costa Urbana", "Costa Rural",
                 "Sierra Urbana", "Sierra Rural", "Selva Urbana", "Selva Rural")
    ),
    prop = prop_no_precarizado,
    low  = prop_no_precarizado_low,
    upp  = prop_no_precarizado_upp
  )

ggplot(plot_prev_dominio, aes(x = dominio, y = prop, fill = dominio)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = low, ymax = upp), width = 0.2) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Empleo No Precarizado por Dominio Geográfico",
    x = "Dominio",
    y = "Prevalencia (%)"
  ) +
  theme_minimal(base_size = 14)

#===================================================
data_desc %>%
  summarise(
    n_sexo        = n_distinct(na.omit(sexo)),
    n_sector      = n_distinct(na.omit(sector)),
    n_estado      = n_distinct(na.omit(estado_civil)),
    n_pobreza     = n_distinct(na.omit(pobreza)),
    n_area        = n_distinct(na.omit(area)),
    n_anio        = n_distinct(na.omit(AÑO))
  )

#===================================================
# ==================================================
# 9. MODELO LOGIT (REGRESIÓN LOGÍSTICA)
#===================================================
library(survey)

modelo_logit <- svyglm(
  empleo_no_precarizado ~ sector + sexo + edad + estado_civil +
    area + factor(AÑO),
  design = diseno,
  family = quasibinomial()
)

summary(modelo_logit)

# ==================================================
# 10. ODD RATIOS DEL MODELO
# ==================================================

library(broom)

odds_ratios <- tidy(modelo_logit) %>%
  mutate(
    OR  = exp(estimate),
    LI  = exp(estimate - 1.96 * std.error),
    LS  = exp(estimate + 1.96 * std.error)
  )

odds_ratios


# ==================================================
# 11. INTERPRETACIÓN DEL EFECTO PRINCIPAL (sector)
# ==================================================
efecto_sector <- odds_ratios %>%
  filter(term == "sectorPublico") %>%
  mutate(
    interpretacion = case_when(
      OR > 1 ~ "Trabajar en el sector público AUMENTA la probabilidad de tener empleo no precarizado.",
      OR < 1 ~ "Trabajar en el sector público REDUCE la probabilidad de tener empleo no precarizado.",
      TRUE   ~ "No hay evidencia de efecto."
    )
  )

efecto_sector


# ==================================================
# 12. PRUEBAS DE HIPÓTESIS
# ==================================================

coef_sector <- summary(modelo_logit)$coefficients["sectorPublico", ]

hip_general <- tibble(
  Hipótesis = "El sector influye en el empleo no precarizado",
  Estimador = coef_sector[1],
  ErrorEstd = coef_sector[2],
  Valor_p   = coef_sector[4],
  Conclusión = if_else(coef_sector[4] < 0.05,
                       "Se RECHAZA H0: El sector SÍ influye.",
                       "NO se rechaza H0: El sector NO influye.")
)

hip_general


