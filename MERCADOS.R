packs <- c("tidyverse", "haven", "srvyr")
instalar <- setdiff(packs, rownames(installed.packages()))
if (length(instalar)) install.packages(instalar, dependencies = TRUE)
invisible(lapply(packs, library, character.only = TRUE))

# Función auxiliar: reemplaza NA por 0 en dummies
na0 <- function(x) ifelse(is.na(x), 0, x)

# ==================================================
# 1. RUTAS Y LECTURA DE ARCHIVOS .SAV
# ==================================================
ruta <- "C:/Users/jegir/Downloads/ENAHO/MODULOS"

mod1  <- read_sav(file.path(ruta, "MODULO 1_2011-2024.sav")) %>%
  mutate(AÑO = as.integer(as.character(AÑO)))

mod2  <- read_sav(file.path(ruta, "MODULO 2_2011-2024.sav")) %>%
  mutate(AÑO = as.integer(as.character(AÑO)))

mod3  <- read_sav(file.path(ruta, "MODULO 3_2011-2024.sav")) %>%
  mutate(AÑO = as.integer(as.character(AÑO)))

mod5  <- read_sav(file.path(ruta, "MODULO 5_2011-2024.sav")) %>%
  mutate(AÑO = as.integer(as.character(AÑO)))

mod34 <- read_sav(file.path(ruta, "MODULO 34_2011_2024.sav")) %>%
  rename(AÑO = ANIO) %>%                         # primero renombramos
  mutate(AÑO = as.integer(as.character(AÑO)))    # luego lo pasamos a número

# ==================================================
# 2. SELECCIÓN DE VARIABLES ÚTILES EN CADA MÓDULO
#    (para evitar duplicados y basura)
# ==================================================

# --- Módulo 1: vivienda / contexto básico
mod1_sel <- mod1 %>%
  select(
    AÑO, MES, CONGLOME, VIVIENDA, HOGAR,
    UBIGEO, DOMINIO, ESTRATO,
    FACTOR07,               # peso módulo 1 (por si lo usas luego)
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
    P301A, P301A1,         # nivel y código de carrera
    P301B, P301C, P301D    # año, grado, tipo de centro
  )
# --- Módulo 5: empleo e ingresos individuales
mod5_sel <- mod5 %>%
  select(
    AÑO, CONGLOME, VIVIENDA, HOGAR,
    UBIGEO, DOMINIO, ESTRATO,
    CODPERSO,
    OCU500, FAC500A,       # PEA y peso de empleo
    P501,                  # ocupación la semana pasada
    P505R4, P506R4,        # CIUO / CIIU
    P507, P510,            # categoría ocupacional, tipo de empleador
    P511A,                 # tipo de contrato
    P513T, I513T,          # horas trabajadas semana pasada
    P520, I520,            # horas habituales
    P514,                  # otro trabajo
    P521, P521A,           # deseo y disponibilidad de más horas
    P524E1, I524A1,        # ingreso líquido / total imputado
    P5441A:P5447A, P544T,  # beneficios extraordinarios
    P558A1:P558A5,         # pensión / no afiliado
    P552                   # trabajó antes
  )

# --- Módulo 34: ingresos del hogar / pobreza
mod34_sel <- mod34 %>%
  select(
    AÑO, MES, CONGLOME, VIVIENDA, HOGAR,
    UBIGEO, DOMINIO, ESTRATO,
    FACTOR07,                     # peso hogar
    INGHOG2D,                     # ingreso neto del hogar
    LINPE, LINEA, POBREZA,        # pobreza
    LINEAV_RPL, LINEAV, POBREZAV, # vulnerabilidad
    ESTRSOCIAL                    # estrato socioeconómico
  )
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

base_iae <- base %>%
  filter(P301A1 == cod_ambiental)

# Opcional: quedarte solo con PEA ocupada
base_iae <- base_iae %>%
  mutate(ocupado = if_else(P501 == 1, 1, 0, missing = 0)) %>%
  filter(ocupado == 1)

# ==================================================
# 5. CONSTRUCCIÓN DEL ÍNDICE DE PRECARIEDAD Y Y
# ==================================================

base_iae <- base_iae %>%
  # RMV por año (aprox; puedes ajustar si quieres ser más fino)
  mutate(
    rmv_anio = case_when(
      AÑO <= 2011 ~ 600,
      AÑO == 2012 ~ 675,
      AÑO %in% 2013:2015 ~ 750,
      AÑO %in% 2016:2017 ~ 850,
      AÑO %in% 2018:2021 ~ 930,
      AÑO >= 2022 ~ 1025,
      TRUE ~ 930
    ),
    # salario mensual (I524A1 es anualizado)
    salario_mensual = I524A1 / 12,
    
    # 1) Salario bajo: <= 2 RMV
    prec_salario_bajo = if_else(
      !is.na(salario_mensual) & salario_mensual <= 2 * rmv_anio, 1, 0, missing = 0
    ),
    
    # 2) Jornada precaria: <34 o >48 horas habituales
    horas_sem = coalesce(I520, P520, I513T, P513T),
    prec_jornada = if_else(
      !is.na(horas_sem) & (horas_sem < 34 | horas_sem > 48),
      1, 0, missing = 0
    ),
    
    # 3) Sin contrato escrito (AJUSTA códigos según tu diccionario)
    # Ejemplo supuesto: 1=indeterminado, 2=plazo fijo, 3=verbal, 4=sin contrato
    prec_contrato = if_else(
      P511A %in% c(3, 4), 1, 0, missing = 0
    ),
    
    # 4) Sin prestaciones: no recibe NINGÚN beneficio extraordinario
    tiene_beneficio = if_else(
      rowSums(across(P5441A:P5447A, ~ .x == 1), na.rm = TRUE) > 0,
      1, 0
    ),
    prec_prestaciones = if_else(tiene_beneficio == 1, 0, 1),
    
    # 5) Sin afiliación previsional (no está afiliado)
    # Supuesto: P558A5==1 => no está afiliado; ajusta si es necesario.
    prec_pension = if_else(P558A5 == 1, 1, 0, missing = 0),
    
    # Índice total de precariedad (0-5)
    indice_precariedad = na0(prec_salario_bajo) +
      na0(prec_jornada) +
      na0(prec_contrato) +
      na0(prec_prestaciones) +
      na0(prec_pension),
    
    # Variable dependiente: empleo no precarizado
    empleo_no_precarizado = case_when(
      indice_precariedad == 0 ~ 1,
      indice_precariedad >= 1 ~ 0,
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
    
    # Estado civil (ejemplo agrupado)
    estado_civil = case_when(
      P209 %in% c(1, 2) ~ "Con_pareja",   # ajusta codificación real
      TRUE              ~ "Sin_pareja"
    ),
    estado_civil = factor(estado_civil),
    
    # Sector de empleo (exposición principal)
    # AJUSTA códigos de P510 según cuestionario ENAHO
    sector = case_when(
      P510 %in% c(2)             ~ "Publico",
      P510 %in% c(1, 3, 4, 5, 6) ~ "Privado_otros",
      TRUE                       ~ NA_character_
    ),
    sector = factor(sector),
    # Pobreza / estrato
    pobreza = factor(POBREZA, levels = c(0,1), labels = c("No_pobre","Pobre")),
    estrato_social = factor(ESTRSOCIAL),
    
    # Área (OJO: revisa codificación de ESTRATO en tu diccionario ENAHO)
    area = case_when(
      ESTRATO %in% c(1,2,3,4) ~ "Urbano",
      TRUE                    ~ "Rural"
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


#GRÁFICOS
library(ggplot2)

# Prev_anio ya existe. Vamos a limpiarlo:
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

#POR SEXO
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
#POR SECTOR

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

#POR AREA
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

#DOMINIO
plot_prev_dominio <- prev_dominio %>%
  mutate(
    dominio = factor(DOMINIO,
                     labels = c("Lima Metropolitana","Resto Costa Urbana","Costa Rural",
                                "Sierra Urbana","Sierra Rural","Selva Urbana","Selva Rural")),
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




















