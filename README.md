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
source("tests/testthat.R")
targets::tar_make()
source("analysis/_renderizar.R")
```

A renderização dos documentos exige o Pandoc, que fica fora do `renv`; sem ele,
`tar_make()` ainda gera todas as tabelas e figuras em `outputs/`. A mesma
sequência roda a cada envio numa máquina limpa, pelo fluxo
`.github/workflows/reproducao.yml`, que também compara os dados regenerados
com os versionados.

A pipeline roda por padrão sobre `data/synthetic/`, a única fonte de dados
versionada do repositório. Dado real é consultado ao vivo no Databricks Unity
Catalog e nunca é persistido em disco.

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
│   ├── scoring/                 # wave 3, não iniciado
│   ├── optim/                    # wave 4, não iniciado
│   └── viz/
├── analysis/                     # documentos Quarto/Rmd, EDA como capítulo
├── data/
│   └── synthetic/                # única fonte de dados versionada
├── tests/
│   └── testthat/                 # testes sobre dados sintéticos
├── outputs/
│   ├── figures/                  # figuras em pt-BR
│   └── tables/
└── docs/
    └── dicionario_de_dados.md    # esquema dos dados e especificação de modelagem
```

## Escopo

| Wave | Conteúdo | Estado |
|------|----------|--------|
| 1 | Setup e sintetizador (ETL) | Concluído |
| 2 | EDA | Concluído |
| 3 | Scoring híbrido (content-based + colaborativo) | A seguir |
| 4 | Otimização multiobjetivo com restrições (ILP, ε-constraint / AUGMECON2) | A seguir |
| 5 | Explicabilidade (SHAP / XAI) | Opcional |
| 6 | Agente conversacional | Trabalho futuro, não implementado |

## Stack em R (candidatos, sujeitos à EDA)

Nenhum método está travado antes da EDA justificar a escolha. A tabela lista
candidatos por tarefa, não uma decisão.

| Uso | Pacotes candidatos |
|-----|---------|
| Acesso ao Databricks | `httr2` (REST) ou `DBI` + `odbc` / `brickster` |
| Fluxo de dados / EDA | `tidyverse`, `data.table`, `janitor`, `skimr`, `DataExplorer`, `here` |
| Matriz esparsa | `Matrix` |
| Recomendação | `recommenderlab` (UBCF, IBCF, funkSVD, ALS), `recosystem` |
| Similaridade (content-based) | `proxy`, cosseno sobre `C` e `P` |
| Otimização ILP | `ompr`, `ompr.roi`, `ROI`, `ROI.plugin.glpk`, `ROI.plugin.symphony` |
| Multiobjetivo (ε-constraint) | laço de escalarização sobre `ompr` / `ROI` |
| Explicabilidade (wave 5) | `DALEX`, `fastshap`, `shapviz`, `kernelshap` |
| Testes | `testthat` |
| Reprodutibilidade | `renv`, `targets`, `rmarkdown` |

## Esquema dos dados

Ver `docs/dicionario_de_dados.md` para o contrato de dados e a especificação de
modelagem por camada.
