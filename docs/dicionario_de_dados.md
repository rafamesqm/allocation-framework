# Dicionário de dados e especificação de modelagem

## Função
Este documento é (1) o contrato de esquema, que o ETL e o gerador sintético seguem, e
(2) a especificação micro de como cada dado vira insumo de modelo. Ele para na fronteira
da EDA: descreve o que cada componente exige e as formulações candidatas, sem cravar o
algoritmo.

## Convenções
* Chaves: `consultor_id`, `projeto_id`, `skill_id`, `cliente_id`.
* Disponibilidade: disponível (existe na base), parcial/derivável (incompleto ou
  calculável), aspiracional (exige nova coleta).
* Todo campo abaixo existe no gerador sintético com o mesmo tipo e granularidade.
* Coluna de disponibilidade atualizada em 2026-07-25 após reconciliação ao vivo
  com o schema real do Databricks. Ver `docs/mapeamento_schema.md` para a
  tabela e coluna de origem de cada campo confirmado.

## Exclusões éticas (nunca como insumo de modelo)
A fonte real de RH carrega campos como raça/cor, orientação sexual, deficiência,
estado civil, além de PII direta (CPF, RG, dados bancários, endereço). Nenhum
desses campos é candidato a `C` ou a qualquer objeto algébrico deste documento,
mesmo que algum dia pareçam ter poder preditivo. A exclusão não é por lacuna de
disponibilidade, é deliberada: usá-los como insumo de alocação é discriminação,
e nenhum deles pertence à especificação de nenhuma camada. O gerador sintético
não deve reproduzir esses campos, nem como ruído.

## Objetos algébricos (o alvo)
O destino de todo o pipeline de dados. Cada objeto é construído a partir das camadas.

| Objeto | Definição | Construído de | Consumido por |
|--------|-----------|---------------|---------------|
| `C` | matriz consultor × atributo (após encoding) | Camada 1 (consultor) | content-based |
| `P` | matriz projeto × requisito, no mesmo espaço de skills de `C` | Camada 1 (projeto) | content-based |
| `S` | taxonomia + adjacência skill × skill (similaridade) | Camada 1 (skills) | match `C`↔`P`, transferência no cold start |
| `R` | matriz esparsa consultor × projeto, valor = rótulo | Camada 2 | colaborativo |
| `c` | vetor custo por consultor · período | Camada 3 | margem + restrição orçamentária |
| `k` | vetor capacidade por consultor · período | Camada 3 | restrição de disponibilidade |
| `A` | matriz de restrições duras (cert., conflito, geografia) | Camada 3 | restrições do ILP |
| `f` | vetor de objetivos por alocação candidata | Camada 3 + `ŝ` | função-objetivo do otimizador |
| `G` | grafo pessoa × pessoa, peso = coalocação boa | Camada 4 | formação de time (TFP) |

---

## Camada 1 · Atributos
Alimenta content-based e cold start. Define quem são as pessoas e as demandas a casar.

### Entidade: Consultor → linhas de `C`
| Campo | Tipo | Granularidade | Papel no modelo | Disponibilidade |
|-------|------|---------------|-----------------|-----------------|
| `nivel_senioridade` | categórico ordinal | consultor | custo · restrição de mix | disponível |
| `tempo_casa` · `tempo_nivel` | numérico | consultor | objetivo de desenvolvimento | disponível |
| `tipo_contrato` | categórico | consultor | disponibilidade · custo | disponível |
| `localizacao` · `fuso` | categórico | consultor | restrição geográfica | disponível |
| `disposicao_viagem` | booleano / escala | consultor | restrição dura | disponível |
| `skills_hard[]` | vetor (skill → proficiência) | consultor | fit técnico · cold start | parcial |
| `certificacoes[]` | lista categórica | consultor | restrição dura | disponível |
| `idiomas[]` | lista categórica | consultor | restrição dura | disponível |
| `experiencia_setorial[]` | vetor (setor → intensidade) | consultor | fit de domínio | disponível |
| `expertise_funcional` | categórico / vetor | consultor | fit complementar | disponível |
| `soft_skills[]` | vetor (do formulário de skills) | consultor | fit qualitativo | derivável |
| `recencia_skill[]` | vetor (skill → data) | consultor | decaimento de competência | parcial |

Montagem de `C`: one-hot dos categóricos, proficiências e intensidades como numéricos
normalizados, `recencia_skill` aplicada como peso de decaimento sobre `skills_hard`.

### Entidade: Projeto / Demanda → linhas de `P`
| Campo | Tipo | Granularidade | Papel no modelo | Disponibilidade |
|-------|------|---------------|-----------------|-----------------|
| `setor` · `tipo_projeto` | categórico | projeto | espaço de busca de domínio | disponível |
| `data_inicio` · `data_fim` · `duracao` | data / numérico | projeto | janela de capacidade | disponível |
| `local` · `on_site` | categórico | projeto | restrição geográfica | parcial |
| `modelo_precificacao` | categórico | projeto | formação da margem | disponível |
| `skills_requeridas[]` | vetor (skill → mínimo, peso) | projeto | alvo do fit técnico | aspiracional |
| `mix_papeis` | estrutura (nível → qtd.) | projeto | restrição de composição | parcial |
| `valor_contrato` · `custo_orcado` | numérico (R$) | projeto | margem (objetivo) | disponível |
| `esforco_estimado` | numérico (FTE / horas) | projeto | necessidade de capacidade | disponível |
| `complexidade` · `importancia_estrategica` | escala ordinal | projeto | peso do objetivo | aspiracional |

Montagem de `P`: as colunas de skill de `P` ocupam o mesmo subespaço das de `C`, para o
match viver num espaço comum. `skills_requeridas` traz o mínimo (restrição) e o peso
(coeficiente no fit).

**Achado de processo, não de coleta:** a Peers não formaliza `skills_requeridas[]`
no momento da venda ou alocação (confirmado com o usuário, não é lacuna de busca).
`P` não pode ser construído a partir de captura real. Decisão de projeto:
`P` será estimado a partir de `setor`/`tipo_projeto`/
Offering/Linha de Serviço/Segmento do projeto, como proxy, nunca como requisito
real capturado (`Indústria` de `vw_gtac_com_clientes`, cotada inicialmente,
foi descartada por estar quase inteiramente nula; `Segmento`, de
`vw_crm_oportunidades_latest`, é o substituto validado). A monografia deve
registrar essa ausência como achado de
pesquisa, não maquiar como se fosse dado observado.

**Decidido na EDA (achado II.6.6), e não na wave 3 como este parágrafo
previa:** `P` é o one-hot do setor, usado como filtro do conjunto candidato e
aplicado só a quem declarou setor (II.8.1). O perfil médio de skills foi
rejeitado por não ser avaliável, e não por ter ido mal: no real não existe
registro que ligue skill de pessoa a requisito de projeto. O one-hot é binário,
filtra e não ordena, e a vantagem dele sobre a popularidade não se sustenta
entre realizações; o que ordena dentro do filtro ficou em aberto (III.4).

### Entidade: Espaço de Skills → `S`
| Campo | Tipo | Granularidade | Papel no modelo | Disponibilidade |
|-------|------|---------------|-----------------|-----------------|
| `taxonomia_skill` | ontologia hierárquica | skill | espaço comum de match | disponível |
| `skill_adjacencia` | matriz skill × skill | skill | transferência · cold start | aspiracional |

Montagem de `S`: `skill_adjacencia` é a matriz de similaridade que permite a um consultor
sem a skill exata pontuar por uma skill vizinha. Sem ela, o match é binário e o cold start
sofre.

---

## Camada 2 · Interações
Alimenta o colaborativo e constrói o sinal de interação. É o coração do sinal.

### Entidade: Alocação → células de `R`
| Campo | Tipo | Granularidade | Papel no modelo | Disponibilidade |
|-------|------|---------------|-----------------|-----------------|
| `(consultor_id, projeto_id)` | chave de relação | alocação | célula de `R` | disponível |
| `papel` · `pct_alocacao` · `horas` | categórico + numérico | alocação | peso da interação | disponível |
| `data_inicio` · `data_fim` · `duracao_dias` | data / numérico | alocação | janela de vigência da interação, base de `k` e do decaimento temporal | disponível |
| `margem_atribuida` | numérico (R$ / %) | frente · mês (não alocação) | ponderação econômica | parcial |
| `flag_restaffing` · `solicitacao_nominal` | booleano | alocação | feedback implícito | aspiracional |

### Construção do sinal de interação (decidida na EDA, achados II.5.1 e II.5.7)

A fonte de avaliação de desempenho não foi disponibilizada pela organização, e
o modelo não a considera.

**Decisão:** a célula de `R` é feedback implícito (Hu, Koren e Volinsky, 2008).
A preferência é binária, `r_ij = 1` quando houve alocação, e a confiança é
`w_ij = 1 + α·log(1 + h_ij)`, com `h_ij` as horas do consultor no projeto. A
comparação entre `α = 0` (binário puro) e `α = 1` fica registrada antes de
existir modelo e é decidida pelo mesmo teste pareado de McNemar a 5% do
protocolo de avaliação; se `α = 1` não superar o binário, fica o binário
(II.5.1). O sinal mede a ocorrência da alocação, e por isso a prática atual de
escolha, com o viés de seleção registrado em "Nota sobre viés de seleção"
abaixo.

**A margem fica fora de `R`.** Três formas foram confrontadas com o medido:

* **por alocação**: descartada, porque a margem não é atribuível a uma
  alocação individual (II.5.2). A granularidade em que ela existe é projeto e
  mês, e atribuí-la a uma pessoa dentro de um time exigiria regra de rateio,
  que seria estimativa e não captura, no mesmo espírito da decisão sobre
  `skills_requeridas[]`;
* **mensal**: descartada, porque não existe custo mensal, e a fonte que
  parecia tê-lo é receita decomposta por origem (II.5.3);
* **estrutural**, de `custo_carregado` × horas contra `valor_contrato`: é a
  que tem mecanismo, e entra como objetivo separado do otimizador, como
  direção e não como magnitude, porque as âncoras econômicas se
  sobredeterminam (II.5.6, II.5.7).

---

## Camada 3 · Operação
Alimenta o otimizador multiobjetivo.

| Campo | Tipo | Granularidade | Objeto | Papel | Disponibilidade |
|-------|------|---------------|--------|-------|-----------------|
| `custo_carregado` | numérico (R$/h) | pessoa · período | `c` | margem + restrição orçam. | disponível |
| `calendario_alocacao` · `pct_ocupacao` | série temporal | pessoa · mês | `k` | restrição de capacidade | parcial |
| `ferias` · `capacidade_periodo` | data / numérico | pessoa · período | `k` | limite de alocação | parcial |
| `conflito_interesse` | matriz pessoa × cliente | par | `A` | restrição dura | aspiracional |
| `preferencias_dominio` | vetor / ranking | consultor | `f` | objetivo de satisfação | parcial |
| `objetivos_desenvolvimento` | texto / categórico | consultor | `f` | objetivo de crescimento | aspiracional |
| `pipeline_demandas` | lista temporal | projeto futuro | n/a | otimização dinâmica | disponível |

### Score de afinidade `ŝ` e entrada em `f`
O recomendador (content-based mais colaborativo) produz `ŝ[i,j]` em [0,1]. `ŝ`
entra em `f` como o coeficiente de aderência da alocação (i, j), ao lado da
margem.

**Calibrado pela EDA:** a forma da combinação é a chaveada, pelo quadrante de
ter histórico e ter respondido o formulário (II.3.3); a ponderada e a cascata
caíram. O conteúdo filtra e não ordena (II.6.2), e o colaborativo não é o
componente principal (II.2.4). O que ordena dentro do filtro, e portanto o
valor de `ŝ`, **não ficou determinado** (III.4): para quem tem histórico sobra a
popularidade, e para quem não tem, só o perfil de skills, que não pôde ser
avaliado. A avaliação de `ŝ` tem dois níveis, aderência ao processo e
plausibilidade das discordâncias entre recomendado e realizado (II.7.8).

---

## Camada 4 · Relacional / Time
Alimenta a formação de equipes (Team Formation Problem).

| Campo | Tipo | Granularidade | Objeto | Papel | Disponibilidade |
|-------|------|---------------|--------|-------|-----------------|
| `grafo_coalocacao` | grafo pessoa × pessoa | par | `G` | química de time | derivável |
| `cobertura_skills_time` | numérico (derivado) | time | n/a | complementaridade | derivável |
| `balanco_senioridade` | distribuição | time | `f` / `A` | restrição de pirâmide | derivável |

Montagem de `G`: derivável do histórico de alocação (Camada 2). Peso da aresta = medida de
coalocação boa entre duas pessoas, a definir na EDA.

---

## Expectativas do gerador sintético
O sintético deve emular não só o esquema, mas as características que estressam o modelo:
* Esparsidade de `R` compatível com a real (a maioria dos pares nunca observada).
* Fração de consultores em cold start (poucas ou nenhuma alocação).
* Cobertura parcial do mapa de skills (nem todo consultor com skills completas).
* Viés de seleção plantado: pares observados não são aleatórios, correlacionam com atributos.
* Temporalidade: datas de alocação, férias, decaimento de recência de skill.
Parametrize o gerador (`n_consultores`, `n_projetos`, `densidade`, `fracao_cold_start`,
`semente`) para reprodutibilidade.

### Revisão de esquema (2026-09-13), na abertura da wave 2

Três lacunas do gerador foram corrigidas antes de começar a EDA, todas
pela regra 7 do `CLAUDE.md`: o campo existe na fonte real e faltava no
sintético, então o sintético estava incompleto.

* **Janela de datas por alocação.** `alocacoes` não tinha `data_inicio` /
  `data_fim`, embora `vw_gtac_cltv_alocacao` tenha as duas e a
  temporalidade já estivesse na lista de expectativas acima. Sem elas o
  vetor `k` não é construível: a soma de `pct_alocacao` por consultor não
  tem denominador, e o histórico inteiro aparenta superalocação.
  Calibração pela Sexta rodada (`mapeamento_schema.md`).
* **`pct_ocupacao` passa a ser série por pessoa e mês**, com mês parcial
  ponderado pelos dias cobertos, que é a granularidade que a Camada 3
  sempre exigiu. Antes era um total agregado por consultor.
* **Margem por frente-mês ganha chave.** A tabela trazia só receita,
  custo e margem, sem identificador, o que tornava o candidato (c) da
  construção do rótulo impossível de confrontar com `R`. Passa a ser
  projeto e mês. A frente está colapsada em projeto, simplificação
  documentada: quantas frentes um projeto tem nunca foi agregado
  enquanto havia acesso.

Pedido do orientador (2026-09-01): o gerador precisa reproduzir dispersão,
assimetria e correlação entre variáveis, não só a média de cada campo
isoladamente. Metodologia do gerador de dados sintéticos: casamento de
momentos por variável, cópula gaussiana para correlação, amostragem
estratificada onde já há distribuição condicional completa (ex.: custo por
cargo), grau de `R` via Binomial Negativa em vez de Bernoulli i.i.d. uniforme.

## Nota sobre viés de seleção
Quase tudo marcado como disponível foi gerado pelo processo de alocação subjetivo que este
projeto quer substituir. `R`, em particular, só registra os pares que um gestor escolheu. O
contrafactual (como um consultor teria performado num projeto onde nunca esteve) é não
observado (MNAR). Isso pesa sobre a validade de um colaborativo puro e precisa ser
enfrentado na EDA, não assumido.
