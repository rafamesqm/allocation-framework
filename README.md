# allocation-framework

Monografia da Especialização em Estatística Computacional Aplicada (UFMG) sobre
alocação de recursos humanos em projetos de consultoria: um modelo híbrido de
recomendação e otimização multiobjetivo, desenvolvido sobre um problema real de
uma consultoria de negócios e tecnologia.

## Reprodutibilidade

O ambiente é gerenciado por `renv` (dependências) e `targets` (pipeline). Para
reconstruir tudo do dado sintético ao resultado, com R 4.5.3:

```r
renv::restore()
targets::tar_make()
source("tests/testthat.R")
source("outputs/results/eda/_renderizar.R")
source("outputs/results/scoring/_renderizar.R")
source("outputs/results/optim/_renderizar.R")
```

A pipeline vem antes dos testes porque parte deles lê o store do `targets`.
O `tar_make()` completo leva cerca de dez horas numa máquina comum: cerca de
três horas e meia nas 100 realizações da wave 3, que refazem a escolha de
hiperparâmetros em cada uma, cerca de três horas nos problemas exatos e no
NSGA-II da wave 4, com tempo-limite de 600 segundos por problema, e cerca de
três horas nas 100 realizações da estabilidade da wave 4. Os problemas que param
no tempo-limite não se reproduzem exatamente entre máquinas.

Para uma corrida rápida, de cerca de 50 minutos,
`Sys.setenv(W3_REALIZACOES = 5, W4_CORRIDA = "reduzida")` antes do `tar_make()`
reduz as realizações, o tempo-limite do HiGHS para 60 segundos, as sementes de
variabilidade do NSGA-II para duas e a corrida exploratória para 2.000
gerações. Nesse modo nada vale como resultado: a pipeline não grava em
`outputs/tables/` nem em `outputs/figures/`, e sim numa pasta temporária
(`W3_SAIDA_REDUZIDA`, ou dentro de `tempdir()`).

A renderização dos documentos exige o Pandoc, que fica fora do `renv`; sem ele,
`tar_make()` ainda gera todas as tabelas e figuras em `outputs/`. O fluxo
`.github/workflows/reproducao.yml` roda a mesma sequência numa máquina limpa: a
cada envio, a corrida rápida, em cerca de uma hora; por disparo manual, a
completa, em três jobs encadeados que passam o store do `targets` de um para o
outro, porque ela não cabe nas seis horas de um job (cerca de 3h40, 3h e 3h15).
A completa também compara os dados regenerados com os versionados.

A pipeline roda por padrão sobre `data/synthetic/`, a única fonte de dados
versionada do repositório. O dado real foi consultado ao vivo no Databricks
Unity Catalog, sem nunca ser persistido em disco, até setembro de 2026, quando
o acesso terminou. A camada de acesso (`R/data/carregar_dados.R`) foi mantida,
parametrizada por fonte, pela portabilidade: a fonte `databricks` para com erro
explícito, em vez de devolver o sintético.

O histórico do Git não é reescrito, nem para retirar rótulos de versões antigas: a prova de anterioridade dos pré-registros depende dos hashes e dos horários de envio registrados pelo GitHub; o repositório é privado, e se precisar ficar público, publica-se um retrato limpo num repositório novo.

## Estrutura do repositório

```
├── README.md
├── _targets.R                # pipeline (targets), roda sobre synthetic por padrão
├── renv.lock                  # dependências fixadas
├── allocation-framework.Rproj
├── R/
│   ├── data/                  # camada de acesso: fonte = synthetic | databricks
│   ├── etl/                   # constrói as camadas
│   ├── features/               # C, P, S, R
│   ├── scoring/               # wave 3: fatoração, mapeamento, híbrido, s chapéu
│   ├── optim/                 # wave 4: montagem, payoff, NSGA-II, avaliação
│   └── viz/
├── config/
│   ├── pre_registro.yml          # protocolo da wave 3, commitado antes de qualquer modelo
│   └── pre_registro_otimizacao.yml  # protocolo da wave 4, com as emendas datadas
├── data/
│   └── synthetic/                # única fonte de dados versionada
├── tests/
│   └── testthat/                 # testes sobre dados sintéticos
├── outputs/
│   ├── figures/                  # figuras em pt-BR
│   ├── tables/
│   └── results/                  # documentos Rmd, um por etapa, que leem os alvos e narram
│       ├── eda/                  # EDA como capítulo
│       ├── scoring/              # wave 3, scoring híbrido
│       │   └── leitura/          # leitura qualitativa do autor, nunca escrita pela pipeline
│       └── optim/                # wave 4, conclusões da otimização
└── docs/
    └── dicionario_de_dados.md    # esquema dos dados e especificação de modelagem
```

## Escopo

| Wave | Conteúdo | Estado |
|------|----------|--------|
| 1 | Setup e sintetizador (ETL) | Concluído |
| 2 | EDA | Concluído |
| 3 | Scoring híbrido (content-based + colaborativo) | Concluído |
| 4 | Otimização multiobjetivo com restrições (HiGHS e NSGA-II) | Concluído |
| 5 | Explicabilidade (SHAP / XAI) | Não realizado |
| 6 | Agente conversacional | Trabalho futuro |

## Stack em R

Os pacotes de fato usados, fixados no `renv.lock`. A fatoração do colaborativo
e o NSGA-II são implementações próprias, em `R/scoring/` e `R/optim/`.

| Uso | Pacotes |
|-----|---------|
| Fluxo de dados / EDA | `tidyverse` (`dplyr`, `tidyr`, `purrr`, `readr`, `tibble`, `forcats`, `lubridate`), `data.table`, `janitor`, `skimr`, `DataExplorer`, `here` |
| Matriz esparsa | `Matrix` |
| Grafo de coalocação | `igraph` |
| Distribuições | `MASS` |
| Recomendação | ALS implícito próprio (`R/scoring/ajustar_als_implicito.R`) |
| Problemas exatos | `highs` (HiGHS) |
| Multiobjetivo | NSGA-II próprio (`R/optim/nsga2.R`) |
| Figuras | `ggplot2`, `scales` |
| Testes | `testthat`, `withr` |
| Reprodutibilidade | `renv`, `targets`, `rmarkdown`, `knitr`, `yaml` |

## Esquema dos dados

Ver `docs/dicionario_de_dados.md` para o contrato de dados e a especificação de
modelagem por camada.
