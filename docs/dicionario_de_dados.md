# Dicionário de dados e especificação de modelagem

## Função
Este documento é (1) o contrato de esquema, que o ETL e o gerador sintético seguem, e
(2) a especificação micro de como cada dado vira insumo de modelo. Ele para na fronteira
da EDA: descreve o que cada componente exige e as formulações candidatas, sem cravar o
algoritmo.

## Convenções
* Chaves: `consultor_id`, `projeto_id`, `skill_id`, `cliente_id`, `avaliador_id`.
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
| `soft_skills[]` | vetor (das avaliações) | consultor | fit qualitativo | derivável |
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
pesquisa, não maquiar como se fosse dado observado. O mecanismo exato de
estimação (ex.: perfil médio de skills de quem já atuou em projetos do mesmo
setor/offering versus one-hot direto das categorias) é decisão de wave 3,
calibrada pela EDA, não travada aqui.

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
Alimenta o colaborativo e constrói os rótulos. É o coração do sinal.

### Entidade: Alocação → células de `R`
| Campo | Tipo | Granularidade | Papel no modelo | Disponibilidade |
|-------|------|---------------|-----------------|-----------------|
| `(consultor_id, projeto_id)` | chave de relação | alocação | célula de `R` | disponível |
| `papel` · `pct_alocacao` · `horas` | categórico + numérico | alocação | peso da interação | disponível |
| `data_inicio` · `data_fim` · `duracao_dias` | data / numérico | alocação | janela de vigência da interação, base de `k` e do decaimento temporal | disponível |
| `avaliacao_desempenho` | ordinal (3 níveis, por competência) | alocação · competência · etapa | rótulo explícito | disponível |
| `avaliador_id` | categórico (papel: individual, gestor às cegas, gestor final) | alocação · etapa | debiasing de rater | parcial |
| `margem_atribuida` | numérico (R$ / %) | frente · mês (não alocação) | ponderação econômica | parcial |
| `flag_restaffing` · `solicitacao_nominal` | booleano | alocação | feedback implícito | aspiracional |

### Construção do rótulo (questão de modelagem aberta)
O valor de cada célula de `R` combina desempenho e margem, que são sinais distintos.
Candidatos, a decidir pela EDA (distribuição, correlação, cobertura): (a) só desempenho
normalizado por avaliador; (b) média ponderada ou produto desempenho × margem; (c) margem
como sinal separado, virando um segundo objetivo em `f`, não parte de `R`; (d) margem
calculada estruturalmente em `f` a partir de `custo_carregado` × horas versus
`valor_contrato`, em vez de herdada do agregado de frente/mês. Não decidir agora.

Achado que pesa sobre essa decisão: o usuário confirmou que a granularidade
por frente e mês é a única que existe para margem, não uma limitação de busca.
Não há `margem_atribuida` por alocação individual em lugar nenhum da Peers.
Atribuir margem a um indivíduo dentro de um time exigiria alguma regra de
rateio (ex.: proporcional a `pct_alocacao`/`horas`), que seria estimativa, não
captura real, no mesmo espírito da decisão sobre `skills_requeridas[]`. Isso
pesa a favor dos candidatos (c)/(d): tratar margem como sinal separado,
calculado no nível em que ela de fato existe (frente/mês, ou estruturalmente
por custo × contrato), em vez de forçar rateio individual dentro de `R`. Não
decidido, só registrado como evidência para a EDA.

**Acoplamento a nomear:** se margem sai de `R` (candidato c ou d), o rótulo de
`R` passa a depender inteiramente de `avaliacao_desempenho`, sem nenhum sinal
de reserva. Hoje isso trava a validação empírica do colaborativo sobre dado
real (funciona no sintético, porque parte da estrutura; não funciona no real
sem acesso à base de avaliações). Reforça que o acesso ao banco de desempenho
é a alavanca de maior retorno do projeto agora, mesmo não sendo pré-requisito
para começar a trabalhar sobre o sintético.

**Caminho intermediário para o colaborativo sobre dado real, enquanto o
acesso a desempenho não vem:** um colaborativo de *implicit feedback* sobre a
própria ocorrência da alocação (quem foi alocado a quê, com ou sem peso por
`pct_alocacao`/`horas`) funciona sem precisar de `avaliacao_desempenho`.
Ressalva importante: isso não contorna o viés de seleção já registrado em
"Nota sobre viés de seleção" abaixo, herda esse viés diretamente, porque
`R` só contém os pares que um gestor já escolheu. Um colaborativo de implicit
feedback sobre esse sinal está, por construção, modelando o processo de
escolha subjetivo atual, não a qualidade do match. É caminho legítimo e
documentado na literatura, mas a validação que ele produz é mais fraca do que
a de um rótulo com sinal de qualidade (desempenho), e essa diferença deve
ficar explícita se o caminho for adotado, não tratada como equivalente.

### Estrutura de `avaliacao_desempenho` (schema reconstruído, sem acesso a banco)
O time não liberou acesso à base nem ao schema de avaliação de desempenho (é o dado
mais sensível da empresa). A estrutura abaixo foi reconstruída a partir da própria
plataforma (visualização de tela, sem consulta a banco), registrada em
`docs/mapeamento_schema.md`. Nenhum dado de terceiro foi lido, só a forma do
formulário.

Cada ciclo de avaliação roda em três etapas, sobre o mesmo conjunto de campos:
1. **Individual**: autoavaliação do próprio consultor.
2. **Avaliação às cegas (gestor)**: o gestor avalia sem ver a autoavaliação.
3. **Avaliação do gestor (calibração)**: habilitada depois das duas anteriores;
   o gestor vê individual × às cegas lado a lado e define a nota final.

Para cada competência (9 subcompetências em 5 categorias: Cultura, Desenvolvimento,
Gestão de Projeto, Entrega do Projeto, Geração de Valor) e cada item de Performance
(Coletivo: resultado do projeto; Individual: qualidade da entrega, relacionamento
com cliente, comercial, valor para empresa), cada etapa registra:
* uma nota ordinal de 3 níveis (`ABAIXO` / `DENTRO` / `ACIMA` do esperado), com
  rubrica descritiva própria por nível, **calibrada pelo nível de carreira** do
  avaliado (o texto da rubrica muda conforme N3, N4 etc.);
* um comentário de texto livre justificando a nota, tamanho bem variável (de
  poucas palavras a parágrafos).

Isso confirma que `avaliador_id` não é um identificador solto: é um eixo
estrutural de 3 papéis (individual, gestor às cegas, gestor final) por pessoa
por ciclo, e a divergência entre autoavaliação e avaliação do gestor às cegas é
em si um sinal de viés de avaliador a explorar na EDA, não só um problema a
normalizar.

Ainda não há acesso à base real, então `avaliacao_desempenho` segue sem consulta
possível: o gerador sintético para esse campo parte inteiramente da forma acima
(estrutura, escala, rubrica por nível, 3 etapas), não de distribuição real
observada.

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
margem. A forma da combinação (switching, weighted, feature augmentation) é
calibrada pela EDA.

---

## Camada 4 · Relacional / Time
Alimenta a formação de equipes (Team Formation Problem).

| Campo | Tipo | Granularidade | Objeto | Papel | Disponibilidade |
|-------|------|---------------|--------|-------|-----------------|
| `grafo_coalocacao` | grafo pessoa × pessoa | par | `G` | química de time | derivável |
| `cobertura_skills_time` | numérico (derivado) | time | n/a | complementaridade | derivável |
| `balanco_senioridade` | distribuição | time | `f` / `A` | restrição de pirâmide | derivável |

Montagem de `G`: derivável do histórico de alocação (Camada 2). Peso da aresta = medida de
coalocação boa entre duas pessoas (ex.: desempenho médio conjunto), a definir na EDA.

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
