# Pacote ----

library(readxl)

library(tidyverse)

library(janitor)

library(flextable)

# Dados ----

## Academicos ----

### Importando ----

acad <- readxl::read_xlsx("Questionário para acadêmicos da FOR (respostas).xlsx")

### Visualizando ----

acad

acad |> dplyr::glimpse()

## Pacientes ----

### Importando ----

pac <- readxl::read_xlsx("Questionário para pacientes da FOR (respostas) 10P tcc.xlsx")

### Visualizando ----

pac

pac |> dplyr::glimpse()

# Análises ----

## Questões ----

#### Investigar a compreensão dos pacientes e acadêmicos de Odontologia sobre o conceito de “brancorexia / bleachorexia” e suas implicações para a saúde bucal e psicológica

### Analisar a percepção dos pacientes e acadêmicos em relação à influência da mídia social o aumento da busca por dentes extremamente brancos?

### Identificar o posicionamento dos acadêmicos frente à ética profissional ao considerar tratamentos estéticos dentais muitas vezes desnecessários;

### Identificar o tipo de procedimento mais procurado para obter dentes brancos;

### Analisar os benefícios e malefícios desta busca incessante por dentes mais brancos na saúde bucal e mental

### Identificar a necessidade de intervenções educativas sobre o equilíbrio entre estética, promoção da saúde bucal e seus reflexos na saúde mental / psicológica

## Pacientes ----

###  Questão 1 ----

result_pac_1 <- pac |>
  dplyr::summarise(Quantidade = dplyr::n(),
                   .by = `4- Você já ouviu falar sobre o termo “bleachorexia” ou “brancorexia”?`)

result_pac_1

### Questão 4 ----

result_pac_4 <- pac |>
  dplyr::summarise(Quantidade = dplyr::n(),
                   .by = `13- Se o/a senhor/a fosse buscar por tratamento para deixar seus dentes mais brancos e harmônicos, qual seria?`)

result_pac_4

## Acadêmicos ----

### Questão 2 ----

result_acad_2 <- acad |>
  dplyr::summarise(Quantidade = dplyr::n(),
                   .by = `8- Você acha que a mídia social influencia na percepção de pacientes sobre o que são “dentes perfeitos”?`)

result_acad_2

### Questão 3 ----

result_acad_3 <- acad |>
  dplyr::summarise(Quantidade = dplyr::n(),
                   .by = `9- Você acha certo a maneira como alguns profissionais de odontologia consideram tratar questões estéticas dentais muitas vezes desnecessárias?`)

result_acad_3
