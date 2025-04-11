library(rvest) # para scrapear
library(tidyverse) # para modelar


# Traemos página ----------------------------------------------------------


url <- "https://www.boletinoficial.gob.ar/seccion/primera"
pagina <- read_html(url)


# Nodos -------------------------------------------------------------------

#Fecha
fechas_nodos <- pagina %>% html_nodes("h6.text-primary-alt.text-bold")
fechas_nodos

fecha_texto <- fechas_nodos[2] %>% html_text(trim = TRUE)
fecha_texto

# Ministerios o órbitas
orbitas <- pagina %>% html_nodes("p.item") %>% html_text()
orbitas

# Nros de normativas
normativas <- pagina %>% html_nodes("p.item-detalle small") %>%
  html_text(trim = TRUE) # limpia la pagina web



# Me quedo con los números impares que serían los números de normativa
numeros_normativa <- normativas[seq(1, length(normativas), by = 2)]
nro_expediente <- normativas[seq(2, length(normativas), by = 2)]

enlaces <- pagina %>% html_nodes("a") %>% html_attr("href") 
enlaces

enlaces_normativa <- enlaces[grepl("/detalleAviso", enlaces) & !grepl("anexos=", enlaces)]
enlaces_normativa

# Generamos dataframe final 
df_normativas <- data.frame(normativa = numeros_normativa,
                            nro_expediente, orbitas,
                            enlaces_normativa,
                            stringsAsFactors = FALSE) |> 
  mutate(fecha_publicacion = fecha_texto,
         enlaces_normativa = paste0("https://www.boletinoficial.gob.ar",
                                    enlaces_normativa)) 

link_normativa_ejemplo <- df_normativas |> 
  head(1) |> 
  pull(enlaces_normativa)

texto_normativa <- read_html(link_normativa_ejemplo)

cuerpo_de_texto <- texto_normativa %>%
  html_nodes("#cuerpoDetalleAviso") %>%  # Selecciona el div con ese ID
  html_text(trim = TRUE) 


# Función -----------------------------------------------------------------


links_normativas <- df_normativas |> 
  pull(enlaces_normativa)

# Armo un dataset vacío
base_texto_normativa <- data.frame()

for (i in links_normativas) {
  link <- i
  
  html_normativa <- read_html(link_normativa_ejemplo)
  
  texto_normativa <- html_normativa %>%
    html_nodes("#cuerpoDetalleAviso") %>%  # Selecciona el div con ese ID
    html_text(trim = TRUE) 
  
  df_temp <- data.frame(enlaces_normativa = link, texto_normativa, 
                        stringsAsFactors = FALSE) 
  
  base_texto_normativa <- base_texto_normativa |> 
    bind_rows(df_temp)
  
}


# Uno todo

df_normativa_final <- df_normativas |> 
  left_join(base_texto_normativa) 

write.csv(df_normativa_final, "df_normativa_final_10_abril.csv")
