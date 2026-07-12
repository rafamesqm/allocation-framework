# Dicionário de dados — Modelo de alocação de consultores

> **Função deste documento.** É o *contrato de dados* da wave 1. Cada campo abaixo
> é um objeto que a literatura e os objetivos do projeto exigem — a lista foi
> derivada de trás para frente (do modelo para o dado), e não da base existente.
>
> **Como usar no ETL.** Para cada campo, o ETL deve: (1) mapeá-lo à coluna real
> na fonte, (2) confirmar `tipo` e `granularidade` contra os dados, e (3)
> atualizar a coluna `disponibilidade` conforme o que de fato existe. A última
> coluna, portanto, funciona também como **diagnóstico de lacunas de coleta**.

## Camadas

| Camada | Alimenta | Objetos algébricos |
|--------|----------|--------------------|
| **C1 · Atributos** | content-based + cold start | `C` (consultor × atributos), `P` (projeto × requisitos), `S` (espaço de skills) |
| **C2 · Interações** | filtragem colaborativa + rótulos | `R` (consultor × projeto) |
| **C3 · Operação** | otimizador multiobjetivo | `c` (custo), `k` (capacidade), `A` (restrições), `f` (objetivos) |
| **C4 · Relacional** | formação de time (TFP) | `G` (grafo de coalocação) |

## Legenda de disponibilidade

- **disponível** — já existe na base, pronto para uso.
- **parcial / derivável** — existe de forma incompleta, ou precisa ser calculado a partir de outra fonte.
- **aspiracional** — não existe hoje; exige nova coleta.

---

## Camada 1 · Atributos

*Alimenta o content-based e o cold start. Define quem são as pessoas e as demandas a casar.*

### Entidade: Consultor

| Campo | Tipo | Granularidade | Papel no modelo | Disponibilidade |
|-------|------|---------------|-----------------|-----------------|
| `nivel_senioridade` | categórico ordinal | consultor | custo · restrição de mix | disponível |
| `tempo_casa` · `tempo_nivel` | numérico | consultor | objetivo de desenvolvimento | disponível |
| `tipo_contrato` | categórico | consultor | disponibilidade · custo | disponível |
| `localizacao` · `fuso` | categórico | consultor | restrição geográfica | disponível |
| `disposicao_viagem` | booleano / escala | consultor | restrição dura | parcial |
| `skills_hard[]` | vetor (skill → proficiência) | consultor | fit técnico · cold start | parcial |
| `certificacoes[]` · `idiomas[]` | lista categórica | consultor | restrição dura | parcial |
| `experiencia_setorial[]` | vetor (setor → intensidade) | consultor | fit de domínio | disponível* |
| `expertise_funcional` | categórico / vetor | consultor | fit complementar | parcial |
| `soft_skills[]` | vetor (derivado das avaliações) | consultor | fit qualitativo | derivável |
| `recencia_skill[]` | vetor (skill → data) | consultor | decaimento de competência | parcial |

\* *reconstruível a partir do histórico de alocação, mesmo sem cadastro explícito.*

### Entidade: Projeto / Demanda

| Campo | Tipo | Granularidade | Papel no modelo | Disponibilidade |
|-------|------|---------------|-----------------|-----------------|
| `setor` · `tipo_projeto` | categórico | projeto | espaço de busca de domínio | disponível |
| `data_inicio` · `data_fim` · `duracao` | data / numérico | projeto | janela de capacidade | disponível |
| `local` · `on_site` | categórico | projeto | restrição geográfica | parcial |
| `modelo_precificacao` | categórico | projeto | formação da margem | disponível |
| `skills_requeridas[]` | vetor (skill → mínimo, peso) | projeto | alvo do fit técnico | aspiracional |
| `mix_papeis` | estrutura (nível → qtd.) | projeto | restrição de composição | parcial |
| `valor_contrato` · `custo_orcado` | numérico (R$) | projeto | margem (objetivo) | disponível |
| `esforco_estimado` | numérico (FTE / horas) | projeto | necessidade de capacidade | parcial |
| `complexidade` · `importancia_estrategica` | escala ordinal | projeto | peso do objetivo | aspiracional |

### Entidade: Espaço de Skills

| Campo | Tipo | Granularidade | Papel no modelo | Disponibilidade |
|-------|------|---------------|-----------------|-----------------|
| `taxonomia_skill` | ontologia hierárquica | skill | espaço comum de match | parcial |
| `skill_adjacencia` | matriz skill × skill | skill | transferência · cold start | aspiracional |

---

## Camada 2 · Interações

*Alimenta a filtragem colaborativa e constrói os rótulos. É o coração do sinal.*

### Entidade: Alocação

| Campo | Tipo | Granularidade | Papel no modelo | Disponibilidade |
|-------|------|---------------|-----------------|-----------------|
| `(consultor_id, projeto_id)` | chave de relação | alocação | célula da matriz `R` | disponível |
| `papel` · `pct_alocacao` · `horas` | categórico + numérico | alocação | peso da interação | disponível |
| `avaliacao_desempenho` | numérico (escala) | alocação | **rótulo explícito** | disponível |
| `avaliador_id` | chave estrangeira | alocação | debiasing de rater | parcial |
| `margem_atribuida` | numérico (R$ / %) | alocação | ponderação econômica | parcial |
| `flag_restaffing` · `solicitacao_nominal` | booleano | alocação | feedback implícito | aspiracional |

> **Cuidado (tratar na EDA):** `avaliacao_desempenho` ≠ `margem_atribuida` — são
> sinais distintos. Como combiná-los no valor de cada célula de `R` é uma decisão
> de modelagem que a EDA deve informar, não pré-decidir. Atribuir margem a um
> indivíduo dentro de um time é não-trivial; avaliações carregam viés de avaliador.

---

## Camada 3 · Operação

*Alimenta o otimizador multiobjetivo.*

### Entidade: Custo & Capacidade

| Campo | Tipo | Granularidade | Papel no modelo | Disponibilidade |
|-------|------|---------------|-----------------|-----------------|
| `custo_carregado` | numérico (R$ / h) | pessoa · período | margem + restrição orçamentária | parcial |
| `calendario_alocacao` · `pct_ocupacao` | série temporal | pessoa · período | restrição de capacidade | parcial |
| `ferias` · `ausencias` · `capacidade_periodo` | data / numérico | pessoa · período | limite de alocação | parcial |

### Entidade: Restrições & Objetivos

| Campo | Tipo | Granularidade | Papel no modelo | Disponibilidade |
|-------|------|---------------|-----------------|-----------------|
| `conflito_interesse` | matriz pessoa × cliente | par | restrição dura | aspiracional |
| `preferencias_dominio` | vetor / ranking | consultor | objetivo de satisfação | aspiracional |
| `objetivos_desenvolvimento` | texto / categórico | consultor | objetivo de crescimento | aspiracional |
| `vetor_objetivos` | vetor | — | eixos do ε-constraint (margem, fit, preferência) | derivável |
| `pipeline_demandas` | lista temporal | projeto futuro | otimização dinâmica | parcial |

---

## Camada 4 · Relacional / Time

*Alimenta a formação de equipes (Team Formation Problem).*

### Entidade: Grafo & Métricas de equipe

| Campo | Tipo | Granularidade | Papel no modelo | Disponibilidade |
|-------|------|---------------|-----------------|-----------------|
| `grafo_coalocacao` | grafo pessoa × pessoa | par | química de time | derivável |
| `cobertura_skills_time` | numérico (derivado) | time | complementaridade | derivável |
| `balanco_senioridade` | distribuição | time | restrição de pirâmide | derivável |

---

## Nota sobre viés de seleção

Quase todos os campos marcados como **disponível** foram gerados pelo processo de
alocação subjetivo que este projeto pretende substituir. A matriz `R`, em
particular, só registra os pares (consultor, projeto) que um gestor escolheu —
o contrafactual (como um consultor teria performado num projeto onde nunca esteve)
é não observado (*missing-not-at-random*). Isso tem consequência direta sobre a
validade de um colaborativo puro e precisa ser enfrentado na EDA, não assumido.
