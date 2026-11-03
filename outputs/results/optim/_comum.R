# Preâmbulo compartilhado pelos documentos da otimização, wave 4.
#
# `outputs/results/optim/` registra as conclusões da wave 4, um documento por
# etapa, como `outputs/results/scoring/` faz para a wave 3: cada documento lê
# o alvo já construído pelo `targets` e narra, sem recalcular nada. O código
# fica em `R/optim/`. Carregue com:
#
#     source(here::here("outputs", "results", "optim", "_comum.R"))
#
# Rode `targets::tar_make()` antes de renderizar; ver
# `outputs/results/optim/_renderizar.R`.

# O preâmbulo da EDA já carrega os pacotes, as funções de R/, o padrão
# gráfico, a guarda contra valor inline vazio e os leitores de alvo.
source(here::here("outputs", "results", "eda", "_comum.R"))
