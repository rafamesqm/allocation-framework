# Mapeamento de schema: dicionário → fonte real

## Função deste documento

Crosswalk entre os campos de `dicionario_de_dados.md` e as tabelas/views reais
no Unity Catalog. Para cada campo, registra o que o modelo espera (tipo e papel
definidos no dicionário), qual é a situação real depois da reconciliação ao
vivo, o formato real do dado na fonte, e a origem exata (catálogo.schema.tabela.
coluna). Alimenta a futura camada de acesso (`R/data/`, fonte = "databricks") e
o gerador de dados sintéticos. Levantado ao vivo, só metadados (nomes de
tabela e coluna, tipos, datas de atualização) e a definição SQL das views,
nunca linha de dado real. Nenhuma coluna de PII ou característica protegida
aparece aqui (ver exclusão ética em `dicionario_de_dados.md`).

## Escopo consultado

Catálogo principal: o catálogo comercial, com suas camadas de dado,
confirmado pelo usuário como o
núcleo real dos dados de alocação. O catálogo de alocação é protótipo segundo o
usuário e foi consultado só como fonte secundária, quando não havia
equivalente em `comercial`. Além do Databricks, o espaço de skills tem uma
fonte externa: planilhas de formulário respondidas pelos colaboradores, que
não podem ser carregadas no Databricks e por isso vivem em `data/raw/skills/`,
fora do rastreamento do git (guarda de diretório inteiro, não só de extensão).
Ver seção própria abaixo.

## Fonte externa: formulários de skills (`data/raw/skills/`)

Três planilhas de formulário, populações majoritariamente distintas de
respondentes (verificado por e-mail, sem expor identidade aqui):

| Arquivo | Respondentes | Foco | Sobreposição com os outros |
|---|---|---|---|
| planilha do formulário de skills de Digital | 102 (101 e-mails únicos, 1 duplicata) | Skills técnicas/digitais (~144 ferramentas: frontend, backend, mobile, dados, devops, testes, RPA, agilidade) | 19 em comum com o formulário geral; 20 em comum com o de Tecnologia |
| planilha do formulário geral de skills | 137 (137 e-mails únicos) | Todas as práticas de consultoria (E&E, Talent, Strategy, Supply, Finance, Digital, Public Services, Health, Insurance, Banking): ferramentas, soft skills de consultoria, experiência setorial, certificações, preferência/restrição de alocação | 19 em comum com o Digital Skills; 0 em comum com o de Tecnologia |
| planilha do formulário de skills de Tecnologia | 29 (29 e-mails únicos) | Especialização em segurança/infra (cloud, rede, identidade, OT/IoT, privacidade, resiliência cibernética) | 20 em comum com o de Digital; 0 em comum com o formulário geral |

União das três: **228 e-mails distintos**. Ainda não cruzado contra o total de
consultores ativos (base corporativa de colaboradores) para saber a cobertura
percentual real.

**Achados de qualidade, relevantes para o gerador sintético e para o ETL:**

* **Escalas de proficiência inconsistentes entre arquivos.** O Digital Skills
  usa Júnior/Pleno/Sênior/Especialista/N/A (escala de senioridade reaproveitada);
  o formulário geral usa Iniciante/Básico/Médio/Avançado. Precisam de um de-para
  de escala antes de virarem uma coluna só de `C`. **Resolvido, ver "Resolvido
  (2026-09-10)" adiante.**
* **"N/A" é resposta válida, não é o mesmo que célula vazia.** No Digital
  Skills, "N/A" é o item mais comum em várias colunas de skill (ex.: 37 de 102
  em HTML5) e significa "não se aplica a mim", distinto de não ter respondido.
  Tratar como zero/ausência de skill, não como missing a imputar.
* **Uma duplicata de respondente** no Digital Skills (102 linhas, 101
  e-mails). Precisa dedup (ex.: manter resposta mais recente por
  `Hora de conclusão`) antes de qualquer análise. **Resolvido, ver adiante.**
* **Duas abas quase idênticas dentro da planilha do formulário de skills de Digital**
  (a original e a revisada): 149 das 150 colunas são idênticas linha a
  linha; a diferença é uma coluna renomeada e o conteúdo de texto livre de
  certificações levemente editado. Não está claro qual é a canônica: confirmar
  com quem preparou antes de escolher uma para o ETL. **Resolvido, ver adiante.**
* **Taxonomia de skills existe**, aba `De-para` do Digital Skills: 144 skills
  mapeadas em 14 categorias (BackEnd, FrontEnd, Mobile, Banco de dados, Devops,
  Testes, Agilidade, RPA, Tecnologia de dados, Plataformas/Sistemas
  Corporativos...). Boa candidata a `taxonomia_skill`, mas 4 das 14 categorias
  (Certificações obtidas, Certificados, Experiência em consultoria, Segmentos
  de atuação) são campos administrativos misturados na tabela, não categorias
  de skill de fato: precisa de limpeza antes de usar. **Confirmado já correto
  no gerador, ver adiante.**
* **Formatação de cabeçalho de coluna inconsistente**: espaçamento duplo/triplo
  no meio de nomes de coluna nos três arquivos (ex.: `"PHP   (Laravel..."`,
  `"AWS   (EC2..."`), mistura de idioma nos metadados (`Email`/`Name` em inglês,
  resto em pt-BR), e no arquivo de Tecnologia uma coluna tem quebra de linha
  literal dentro do próprio nome. `janitor::clean_names()` resolve no ETL, mas
  o nome bruto não é limpo.
* **Chave de ligação é e-mail/nome (PII direta), não `consultor_id`.** Vai
  precisar de uma tabela de-para nome/e-mail → `consultor_id` (via
  base corporativa de colaboradores) para juntar com o resto do modelo. Esse
  cruzamento por PII merece cuidado de acesso equivalente ao dado real do
  Databricks, mesmo sendo arquivo local.
* **Achado que não estava no dicionário**: o formulário geral tem duas colunas de
  texto livre, "possui alguma preferência em relação à alocação" e "possui
  alguma restrição em relação à alocação": candidatas diretas a
  `preferencias_dominio` (Camada 3).

### Resolvido (2026-09-10): de-para de escalas, aba canônica e dedup

Trabalho offline sobre as planilhas locais, sem precisar de acesso ao
Databricks, feito antes de soltar os arquivos (PII de colegas, não deve
sair da máquina). Nenhum e-mail, nome ou texto livre de respondente
aparece abaixo, só nomes de coluna, categorias de resposta e contagens.

**De-para de escala de proficiência, contra a escala do gerador sintético
(Iniciante/Básico/Médio/Avançado):**

| Escala real | Arquivo | Equivalente no gerador |
|---|---|---|
| N/A | Digital Skills | ausência de skill (zero, não é nível) |
| Júnior | Digital Skills | Iniciante |
| Pleno | Digital Skills | Básico |
| Sênior | Digital Skills | Médio |
| Especialista | Digital Skills | Avançado |
| Não possuo conhecimento | Tecnologia | ausência de skill (zero, não é nível) |
| Básico | Tecnologia | Básico |
| Intermed. | Tecnologia | Médio |
| Avançado | Tecnologia | Avançado |
| Iniciante / Básico / Médio / Avançado | formulário geral | idêntico, sem de-para |

A escala do formulário de Tecnologia não tinha sido documentada antes desta rodada de
limpeza (achado novo, nenhuma das reconciliações anteriores tinha olhado
para ela). Ela não distingue "iniciante" de "não possuo": quem marca
"Básico" no formulário de Tecnologia pode corresponder tanto a Iniciante quanto a Básico no
gerador, assimetria que fica registrada como limitação, não resolvida por
suposição.

**Achado lateral, fora do escopo desta limpeza mas relevante para
`experiencia_setorial[]`:** o formulário geral usa uma segunda escala,
separada da de proficiência, para as perguntas de experiência setorial por
prática: "Não tenho experiência" / "Tenho conhecimento sobre o tema mas
não fiz projeto" / "1 a 3 projetos" / "Mais de 3 projetos" (85 colunas).
Dá para calibrar `intensidade` em `gerar_experiencia_setorial()` por essa
escala ordinal, hoje é `runif(0,1)` sem calibração real. Trabalho de
código futuro.

**Aba canônica da planilha do formulário de skills de Digital: a revisada.** As
duas abas têm as mesmas 102 linhas e 149 das 150 colunas idênticas célula
a célula. A única diferença de conteúdo é o texto livre de certificações
(3 células), e a única diferença de esquema é uma coluna renomeada
("Você possui algum certificado?" na original, "Certificados" na
revisada). O próprio nome da aba e o conteúdo revisado indicam
que é a versão corrigida.

**Taxonomia de skills: limpeza confirmada já correta no gerador.** As 4
categorias administrativas (Certificações obtidas, Certificados,
Experiência em consultoria, Segmentos de atuação) têm exatamente 1 linha
cada na aba `De-para`, confirmando que são campo solto, não categoria de
skill. As 10 categorias reais somam exatamente 140 skills (Agilidade 14,
BackEnd 19, Banco de dados 17, Devops 16, FrontEnd 18, Mobile 13,
Plataformas/Sistemas Corporativos 9, RPA 6, Tecnologia de dados 19, Testes
9), número que já bate exatamente com `gerar_espaco_skills()`. Nenhuma
mudança de código necessária aqui, só a confirmação.

**Dedup do respondente duplicado, regra decidida.** O e-mail duplicado em
nas duas abas (102 linhas, 101 e-mails distintos) tem duas
respostas cerca de 18 dias de distância uma da outra por `Hora de
conclusão`. Regra: manter a mais recente, descartar a mais antiga. A linha
exata fica só no ambiente local, não é necessária para registrar a regra
e não deve ser citada aqui para não expor a identidade do respondente.

## Legenda de situação

* **Confirmado**: coluna real encontrada, tipo e origem verificados.
* **Confirmado, aproximado**: achado real existe, mas granularidade ou
  semântica ainda precisa validação antes de virar target de ETL.
* **Suposto**: o negócio dá como certo que existe, mas esta reconciliação não
  achou a tabela de origem. Diferente de aspiracional: aqui a suposição é de
  que falta procurar direito, não que o dado nunca foi coletado.
* **Parcial**: parte do campo tem fonte, parte não.
* **Sem fonte**: nenhuma pista encontrada nesta rodada. Mantém o rótulo
  aspiracional do dicionário.
* **Confirmado ausente**: o usuário confirmou que o dado não existe como
  processo de negócio na organização, não é lacuna de busca. Mais forte que "Sem
  fonte": aqui não é "não achamos ainda", é "não tem o que achar".

## Tabelas legado ou fora de uso, não usar

Nomes com sufixo de teste encontrados no catálogo comercial, todos sem
atualização recente (a mais nova é de dezembro de 2025, a mais antiga de junho
de 2025, contra tabelas vigentes atualizadas no dia do levantamento):

| Tabela | Última atualização |
|--------|---------------------|
| primeira tabela de teste do cadastro de pessoas | 2025-12-02 |
| segunda tabela de teste do cadastro de pessoas | 2025-12-05 |
| versão de teste da base corporativa de colaboradores | 2025-12-15 |
| tabela de teste de faturas | 2025-06-04 |
| view de teste comercial | 2026-03-13 |

A versão vigente do cadastro de pessoas é a base corporativa de colaboradores
(view atualizada no dia do levantamento, aponta para
a tabela vigente do cadastro, não para as de teste).

**Achado à parte, não sobre legado:** a base de experiência profissional
tem a definição quebrada: o `SELECT` retorna literais de texto fixos (nomes
de coluna como string), não as colunas reais da tabela de origem. Aparece como
"atualizada hoje" nos metadados, mas isso reflete só o redeploy da definição,
não dado confiável por trás. Não usar até confirmar com quem mantém o dado.
Boa parte das views vigentes carrega um comentário no SQL do tipo "ATENÇÃO:
coloque o nome da tabela de origem aqui", sinal de que essa camada de views
está em manutenção ativa e pode mudar. Vale reconfirmar antes de travar
qualquer mapeamento na camada de acesso.

## Camada 1 · Atributos

### Consultor → linhas de `C`

| Campo | O que o modelo espera | Situação | Formato real | Origem real |
|---|---|---|---|---|
| `nivel_senioridade` | categórico ordinal, custo · restrição de mix | Confirmado | STRING (`Cargo`, `Sigla`, `Nivel`, `Vertical`, `Modelo`) | base de cargos |
| `tempo_casa` · `tempo_nivel` | numérico, objetivo de desenvolvimento | Confirmado, aproximado | DATE por nível (`Data de admissão`, `Intern`, `Trainee`, `Analyst` … `Director`) | base corporativa de colaboradores |
| `tipo_contrato` | categórico, disponibilidade · custo | Suposto | STRING (`Modelo`) | base de cargos / base corporativa de colaboradores, campo `Modelo`; semântica não confirmada |
| `localizacao` | categórico, restrição geográfica | Confirmado | STRING (`Cidade`, `Estado`, `Base`) | base corporativa de colaboradores |
| `fuso` | categórico, restrição geográfica | Suposto | sem campo próprio | derivável de `Estado`, não fonte direta |
| `disposicao_viagem` | booleano / escala, restrição dura | Confirmado, aproximado | STRING (resposta de formulário livre, não booleano estruturado) | base corporativa de colaboradores (`Possui interesse em projetos com viagens nacionais?`, `...internacionais?`, `Possui visto ativo?`) |
| `skills_hard[]` | vetor skill → proficiência, fit técnico · cold start | Confirmado, aproximado | STRING (escalas de proficiência diferentes por arquivo, ver ressalvas) | planilha do formulário de skills de Digital, planilha do formulário de skills de Tecnologia (fonte externa, fora do Databricks) |
| `certificacoes[]` | lista categórica, restrição dura | Confirmado | STRING (texto livre de certificação) | `data/raw/skills/` (todos os três arquivos têm pergunta de certificação) |
| `idiomas[]` | lista categórica, restrição dura | Confirmado, aproximado | STRING (resposta de formulário: inglês, espanhol, outras línguas) | base corporativa de colaboradores |
| `experiencia_setorial[]` | vetor setor → intensidade, fit de domínio | Confirmado | STRING (texto livre, "em quais segmentos já executou projeto", por prática) | planilha do formulário geral de skills |
| `expertise_funcional` | categórico / vetor, fit complementar | Confirmado | STRING (formulário organizado por prática: E&E, Talent, Strategy, Supply, Finance, Digital, Public Services, Health, Insurance, Banking) | planilha do formulário geral de skills; complementa base corporativa de colaboradores (Vertical, Frente) |
| `soft_skills[]` | vetor do formulário de skills, fit qualitativo | Confirmado | STRING (bloco de habilidades de consultoria: estruturação de problema, storylining, design thinking, gestão da mudança...) | planilha do formulário geral de skills |
| `recencia_skill[]` | vetor skill → data, decaimento | Parcial | POSIXct (`Hora de conclusão` do formulário) | derivável da data de resposta de cada planilha, não é recência por skill individual |

### Projeto / Demanda → linhas de `P`

| Campo | O que o modelo espera | Situação | Formato real | Origem real |
|---|---|---|---|---|
| `setor` · `tipo_projeto` | categórico, espaço de busca de domínio | Confirmado, join validado | STRING (`Segmento`, `Offering`, `Linha de Serviço` da base de oportunidades do CRM; `situacao`/`etapa_modalidade` da base de projetos; `Indústria` da base de clientes descartada, 99,9% nula) | join chave de CRM da base de projetos = `base de oportunidades do CRM.\`Negócio Código\`` testado e validado (~98% de cobertura) |
| `data_inicio` · `data_fim` · `duracao` | data / numérico, janela de capacidade | Confirmado | DATE | fonte de alocações (`Data de início`, `Data de fim`) |
| `local` · `on_site` | categórico, restrição geográfica | Suposto | - | nenhum candidato identificado nesta rodada |
| `modelo_precificacao` | categórico, formação da margem | Suposto | - | nenhum candidato identificado nesta rodada |
| `skills_requeridas[]` | vetor skill → mínimo/peso, alvo do fit técnico | Confirmado ausente | - | usuário confirmou: não existe processo de negócio que capture requisito de skill por demanda, não é lacuna de coleta |
| `mix_papeis` | estrutura nível → qtd., restrição de composição | Suposto | - | nenhum candidato identificado nesta rodada |
| `valor_contrato` · `custo_orcado` | numérico (R$), margem (objetivo) | Confirmado | DOUBLE / STRING (valor em reais) | base de projetos, tabelas do sistema de gestão de projetos |
| `esforco_estimado` | numérico (FTE/horas), necessidade de capacidade | Confirmado | DOUBLE (`Horas Semana`, `horas_mes`), INT (`Duração`) | pré-venda: base de pipeline do CRM; pós-venda: fonte de alocação de pessoas por projeto |
| `complexidade` · `importancia_estrategica` | escala ordinal, peso do objetivo | Confirmado ausente | - | usuário confirmou: não existe |

### Espaço de Skills → `S`

| Campo | O que o modelo espera | Situação | Formato real | Origem real |
|---|---|---|---|---|
| `taxonomia_skill` | ontologia hierárquica | Confirmado, aproximado | STRING (144 skills em 14 categorias) | aba `De-para` da planilha do formulário de skills de Digital; precisa limpar 4 categorias que são metadado, não skill |
| `skill_adjacencia` | matriz skill × skill | Confirmado ausente | - | usuário confirmou: não existe |

## Camada 2 · Interações → células de `R`

| Campo | O que o modelo espera | Situação | Formato real | Origem real |
|---|---|---|---|---|
| `(consultor_id, projeto_id)` | chave de relação, célula da matriz `R` | Confirmado | STRING | fonte de alocações (`Cód. Colaborador`, `Código`), fonte de alocação de pessoas por projeto (chave de projeto e de alocação) |
| `papel` · `pct_alocacao` · `horas` | categórico + numérico, peso da interação | Confirmado | DOUBLE (`Percentual de alocação do N1`, `horas_mes`), STRING (`nivel`, `sigla_cargo`) | fonte de alocações, fonte de alocação de pessoas por projeto |
| avaliação de desempenho | fonte não disponibilizada pela organização; o modelo não a considera | Não disponibilizada | - | - |
| `margem_atribuida` | numérico (R$/%), ponderação econômica | Confirmado, granularidade diferente da esperada | DOUBLE (colunas de receita própria, da coligada e outras) | fonte de receita mensal por projeto; usuário confirmou que frente/mês é a única granularidade que existe, não há margem por alocação individual em lugar nenhum |
| `flag_restaffing` · `solicitacao_nominal` | booleano, feedback implícito | Confirmado ausente | - | usuário confirmou: não é documentado |

## Camada 3 · Operação

| Campo | O que o modelo espera | Situação | Formato real | Origem real |
|---|---|---|---|---|
| `custo_carregado` | numérico (R$/h), margem + restrição orçamentária | Confirmado | DOUBLE (`Taxa`) | tabela de taxa por hora (por `Sigla cargo`) |
| `calendario_alocacao` · `pct_ocupacao` | série temporal, restrição de capacidade | Confirmado | DOUBLE / DATE (`data_mes`, `horas_mes`) | fonte de alocações, fonte de alocação de pessoas por projeto |
| `ferias` · `ausencias` · `capacidade_periodo` | data / numérico, limite de alocação | Confirmado | DATE, INT, STRING | base de resumo de férias, base de gestão de férias, base de férias programadas, base de afastamentos, base de colaboradores em férias |
| `conflito_interesse` | matriz pessoa × cliente, restrição dura | Confirmado ausente | - | usuário confirmou: não existe |
| `preferencias_dominio` | vetor / ranking, objetivo de satisfação | Confirmado, aproximado | STRING (texto livre, não estruturado) | planilha do formulário geral de skills ("possui alguma preferência/restrição em relação à alocação") |
| `objetivos_desenvolvimento` | texto / categórico, objetivo de crescimento | Confirmado ausente | - | usuário confirmou: não existe |
| `pipeline_demandas` | lista temporal, otimização dinâmica | Confirmado | STRING / DOUBLE / DATE (variados) | base de oportunidades do CRM, base de pipeline do CRM |

## Camada 4 · Relacional / Time

| Campo | O que o modelo espera | Situação | Formato real | Origem real |
|---|---|---|---|---|
| `grafo_coalocacao` | grafo pessoa × pessoa, química de time | Suposto | - | derivável da Camada 2, nenhuma fonte direta |
| `cobertura_skills_time` | numérico derivado, complementaridade | Suposto | - | deriva de `skills_hard[]`, mesma limitação de fonte |
| `balanco_senioridade` | distribuição, restrição de pirâmide | Suposto | - | deriva de `nivel_senioridade`, mesma fonte |

## Leitura consolidada

* **Melhor do que o dicionário registrava:** `disposicao_viagem`, `idiomas[]`,
  `esforco_estimado`, `custo_carregado`, `pipeline_demandas`, `tempo_casa` ·
  `tempo_nivel` (data por nível de carreira, não só admissão), e agora todo o
  espaço de skills (`skills_hard[]`, `soft_skills[]`, `taxonomia_skill`,
  `certificacoes[]`, `experiencia_setorial[]`, `expertise_funcional`) e
  `preferencias_dominio`, via as planilhas de `data/raw/skills/`.
* **Suposto, não confirmado:** `tipo_contrato`, `local`/`on_site`,
  `modelo_precificacao`, `mix_papeis`.
* **Confirmado ausente pelo usuário (não é lacuna de busca, é ausência real
  de processo ou registro na organização):** `skills_requeridas[]`,
  `complexidade` · `importancia_estrategica`, `conflito_interesse`,
  `objetivos_desenvolvimento`, `skill_adjacencia`,
  `flag_restaffing`/`solicitacao_nominal`.
* **`skills_requeridas[]` ausente: decisão tomada (2026-07-31).**
  `skills_requeridas[]` não é lacuna de coleta, é ausência de processo: a
  organização não formaliza o requisito técnico de uma demanda no momento da venda
  ou alocação. Isso desafiava a arquitetura conceitual que assume `P`
  (projeto × requisito, no mesmo espaço de skills de `C`) como insumo do
  content-based. Decisão: `P` será estimado a partir de
  `setor`/`tipo_projeto`/Offering/Linha de Serviço/Indústria do projeto (proxy,
  campos que existem), nunca tratado como requisito real capturado. A ausência
  vira achado de pesquisa na monografia, registrada explicitamente, não
  maquiada. Mecanismo exato de estimação fica para a EDA (wave 2) ou a
  wave 3 decidir.
* **`margem_atribuida`: granularidade confirmada, não é limitação de busca
  (2026-08-01).** Usuário confirmou que frente/mês é a única granularidade de
  margem que existe na organização, não há valor por alocação individual em lugar
  nenhum. Atribuir margem a uma pessoa dentro de um time exigiria rateio
  estimado (ex.: por `pct_alocacao`/`horas`), não captura real, no mesmo
  espírito da decisão de `skills_requeridas[]`. Isso pesa a favor do candidato
  (c) da "Construção do rótulo" no dicionário: tratar margem como sinal
  separado por frente/mês em `f`, não forçar rateio individual dentro de `R`.
  Não decidido ainda, só registrado como evidência para a EDA.
* **Skills deixou de ser o maior gargalo**, mas tem trabalho de limpeza pela
  frente antes de virar `C`: três planilhas com populações de respondentes
  majoritariamente diferentes (228 e-mails únicos ao todo), duas escalas de
  proficiência distintas que precisam de de-para, uma duplicata de
  respondente, cabeçalhos de coluna com formatação inconsistente, e a
  chave de ligação sendo e-mail/nome (PII), não `consultor_id`.

## Calibração agregada (levantada em 2026-09-01, antes do fim do acesso em 09/09)

Números agregados puxados ao vivo via SQL warehouse, exclusivamente `COUNT`/
`GROUP BY`/`MIN`/`AVG`/`MAX`, nunca linha de dado. Servem para parametrizar o
gerador sintético com distribuições realistas.

**Headcount (base corporativa de colaboradores):** 389 registros no total,
por status: EM ATIVIDADE NORMAL 285, DEMITIDO EM MESES ANTERIORES 75,
GOZANDO FERIAS 15, DEMITIDO NO MES 4, AFASTADO SEM REMUNERACAO 4, GOZANDO
RECESSO 2, AUXILIO-DOENCA 2, LICENCA-MATERNIDADE 2.

**Pirâmide de senioridade, só ativos (n=285):** `Nível` é decrescente com a
hierarquia, `N0` é o topo (mais sênior) e `N4` a base (mais júnior),
confirmado pelo usuário. Contagens: N3 107, N2 71, N1 55, N4 42, N0 8, ADM 1,
NENHUMA 1. Da base ao topo (N4→N3→N2→N1→N0): 42, 107, 71, 55, 8. Não é
pirâmide estrita, o nível logo acima da base (N3) é o mais numeroso, maior
até que a própria base (N4). Não simular como decrescente monotônico da base
ao topo.

**Projetos por situação (base de projetos):** Recusada/Cancelada 2799,
Projeto (executando) 952, Proposta 209, Lead 127, Reunião Inicial 123,
Ideia 92. Funil de vendas com atrição grande, relevante para `pipeline_demandas`.

**Alocações (fonte de alocações):** 4999 linhas, 511 colaboradores
distintos (histórico completo, inclui desligados, por isso maior que os 285
ativos), 1833 códigos de alocação distintos.

**Custo carregado por cargo (tabela de taxa por hora, R$/h):** varia de
~85 (estagiário, sigla `SO_INT`) a ~2066 (diretoria sênior, `MD5`), escalando
de forma monotônica e suave por nível. Tabela completa por sigla salva no
histórico da sessão, não reproduzida aqui campo a campo; usar a faixa
mín/média/máx por nível para amostrar `custo_carregado` no sintético.

**Valor de contrato (fonte de alocação do sistema de gestão de projetos, R$, n=3339):** mínimo 0,
média ~431.862, máximo ~38.816.467. Distribuição muito assimétrica à direita,
não normal, não amostrar como normal no sintético.

**Granularidade confirmada pelo usuário (2026-09-19): `valor_contrato` na
fonte de alocação do sistema de gestão de projetos é por projeto, não por alocação e pessoa.** O
bloco II.5 da EDA levantou a hipótese contrária, a partir do nome da tabela
e do volume de linhas, ao encontrar margem estrutural implausível. A
hipótese está descartada, e a implausibilidade continua a ser explicada.
Ver a questão aberta sobre tabela de taxa por hora abaixo.

**Questão aberta (levantada pelo bloco II.5 da EDA, 2026-09-19): `Taxa` na
tabela de taxa por hora é preço ou custo?** A Quinta rodada registrou a
tabela como *rate card*, termo que designa tabela de preço. O dicionário e o
gerador a tratam como `custo_carregado`, isto é, custo da pessoa para a
empresa. As duas leituras não são compatíveis, e a EDA achou dois indícios
de que a coluna é preço:

* a razão entre custo calculado e valor de contrato centra em torno de 1,
  quando custo contra receita centraria bem abaixo disso;
* sobre a jornada calibrada de 120 h/mês, a taxa implica custo mensal da
  ordem de 32 mil reais para o nível mais júnior e 144 mil para o mais
  sênior, valores plausíveis como faturamento e não como salário mais
  encargos.

Se a coluna for preço, a organização não tem custo interno por pessoa em lugar
nenhum além da coluna de receita de estagiários (frente/mês, 44,7% zerado), e o
objetivo econômico da wave 4 fica sem base de dados defensável. O acesso ao
ambiente está encerrado e a distinção não é derivável do que ficou
registrado, então fica como questão aberta e não como achado.

**Cold start e densidade (join de checagem, não validado a fundo):** 48 dos
285 consultores ativos não aparecem em nenhuma linha da fonte de alocações
(~17%, primeira estimativa de cold start do lado consultor). 4598 pares
distintos (colaborador, código de alocação) sobre 511 colaboradores × 1833
códigos possíveis, densidade bruta ~0,5%, consistente com a esparsidade
esperada de `R`. Tratar como estimativa de primeira passada: o join assume
que `ID` (entidades) e `Cód. Colaborador` (alocação) são a mesma chave, o que
não foi confirmado com o time de dados.

### Segunda rodada de calibração (2026-09-01): dispersão, correlação, grau

**Correção de direção de escala (confirmada pelo usuário): `Nível` é
decrescente com a hierarquia.** `N0` é o topo (mais sênior), `N4` é a base
(mais júnior), o oposto do que a codificação inicial (N0→0 ... N4→4) assumia.
Com a direção corrigida, as duas correlações abaixo passam a fazer sentido e
são confiáveis, só o sinal precisa ser invertido em relação ao que saiu da
consulta bruta:

* `Nível` × taxa de cargo: consulta bruta deu -0,90; com escala ascendente de
  senioridade (N4=0 ... N0=4), a correlação correta é **+0,90**: quanto mais
  sênior, maior o custo, exatamente como a progressão por `Sigla Cargo` já
  mostrava.
* `Nível` × tempo de casa: consulta bruta deu -0,34; com a mesma correção, a
  correlação correta é **+0,34**: quanto mais sênior, mais tempo de casa,
  relação moderada e plausível.

Implicação para o gerador: `nivel_senioridade` deve ser codificado como
escala ascendente de senioridade (ex.: N4 < N3 < N2 < N1 < N0), não pela
leitura literal do número no rótulo. A pirâmide de contagem (abaixo) também
precisa ser lida nessa direção.

**Dispersão e assimetria confirmadas (SQL agregado, nunca linha de dado):**

| Variável | Fonte | Desvio-padrão | P25 | P50 | P75 | Assimetria |
|---|---|---|---|---|---|---|
| Taxa/hora (`custo_carregado`) | tabela de taxa por hora | 453,50 | 286,58 | 485,70 | 944,62 | 0,93 (moderada, positiva) |
| Valor de contrato, só positivos | fonte de alocação do sistema de gestão de projetos | 1.771.644 | 34.814 | 108.300 | 375.054 | 12,68 (extrema, cauda muito pesada) |
| Horas/mês por pessoa alocada (`horas_mes`; registrada antes como `esforco_estimado`, ver nota abaixo) | fonte de alocação de pessoas por projeto | 66,90 | 32 | 120 | 160 | -0,05 (quase simétrica) |

Nota de correção: a linha de horas/mês foi registrada nesta rodada como
`esforco_estimado` de projeto, e o gerador sorteou o esforço de projeto a
partir dela. A fonte tem uma linha por pessoa alocada, então as âncoras são de
`horas_mes` de consultor. O esforço de projeto não tem âncora real e passou a
ser derivado das alocações, como soma das horas mensais da equipe.

**Massa em zero no valor de contrato:** de 3339 registros, 490 (14,7%) são
zero ou negativos. Não é um detalhe menor: uma log-normal comum não acomoda
zero exato, é preciso um modelo com massa pontual em zero mais uma
distribuição contínua para a parte positiva (modelo *hurdle*), não uma
log-normal ingênua sobre todo o campo.

**Distribuição de grau em `R` (para não gerar Bernoulli i.i.d. uniforme):**

| Grau | Média | Desvio-padrão | Máximo |
|---|---|---|---|
| Alocações distintas por colaborador | 9,00 | 6,68 | 35 |
| Colaboradores distintos por código de alocação | 2,51 | 2,94 | 47 |

Nos dois casos a variância é bem maior que a média (sobredispersão clara:
44,6 vs. 9,0 no primeiro; 8,65 vs. 2,51 no segundo), o que descarta Poisson
como modelo de grau e aponta para Binomial Negativa.

### Terceira rodada (2026-09-01): tudo reverificado do zero, mais validação de join

Reexecutei toda a bateria de consultas das rodadas anteriores sem assumir
nada. **Todos os números de headcount, pirâmide, projetos por situação,
alocações, taxa por cargo, valor de contrato, dispersão, assimetria e grau
saíram idênticos aos já registrados acima.** Schema das 6 tabelas usadas
(colunas, tipos) também confere. Os dados não mudaram desde a rodada
anterior; o que muda nesta rodada é a validação dos dois joins pendentes.

**Join `ID` (entidades) = `Cód. Colaborador` (alocação): fraco, usar com
ressalva.** *Superado pela consulta de confirmação da chave, mais abaixo: a chave é a mesma, e a
versão completa da base corporativa de colaboradores eleva a cobertura para cerca de 98%. O
registro abaixo fica como estava na rodada.* Cobertura real: dos 511 colaboradores distintos que aparecem na
fonte de alocações, só 327 (64%) têm `ID` correspondente na tabela de
entidades atual. Testei duas chaves alternativas (a de contratado e a
de data lake, ambas também presentes em entidades): as duas deram 0% de
match, claramente são espaços de chave diferentes, não é isso. `ID` continua
sendo a melhor chave disponível, mas com 36% dos colaboradores históricos de
alocação sem correspondência (provavelmente gente desligada há mais tempo,
removida da tabela de entidades, ou terceirizados/"Billado" que nunca tiveram
registro de entidades). **Consequência prática: a estimativa de cold start
(~17%) e a densidade calculada anteriormente usam essa chave e podem estar
enviesadas** (alguém pode parecer "sem alocação" só porque o `ID` dele não
bate, não porque nunca foi alocado). Tratar como estimativa aproximada, não
como número final para a EDA.

**Join da chave de CRM (base de projetos) = `Negócio Código`
(base de oportunidades do CRM): forte, pode usar.** Cobertura da chave de CRM
nos 952 projetos em execução: 100% (952 de 952 preenchidos). Join contra o
CRM: ~98% de correspondência (939 de 960; o denominador de 960 em vez de 952
sugere que `Negócio Código` não é estritamente único no CRM, algum
fan-out no join, não investigado a fundo, mas a taxa de acerto é alta o
suficiente para confiar). **`Offering` e `Linha de Serviço` podem ser usados
como proxy de `P` para a maioria dos projetos executados**, como já estava
decidido.

**Achado novo, não esperado: `Indústria` na base de clientes está
praticamente vazia.** De 1999 registros, 1997 são `NULL`, só 2 preenchidos.
Esse campo, que eu tinha citado como parte do proxy de `P`, não serve como
está. **Substituto encontrado: `Segmento` na base de oportunidades do CRM**,
96,8% preenchido (4086 de 4220), 10 categorias reais:

| Segmento | n |
|---|---|
| Varejo | 1097 |
| Serviços | 773 |
| Indústria | 510 |
| Banking | 384 |
| Educação | 348 |
| Health | 315 |
| Insurance | 240 |
| Agronegócio | 182 |
| Alimentos e Bebidas | 165 |
| Private Equity | 72 |

E a distribuição de `Offering` (mesma tabela), 12 categorias reais, as duas
maiores Experience & Efficiency (991) e Strategy and M&A (642), seguidas por
Data Analytics & AI (530), Supply Chain (524), Tech Consulting (427), Finance
(424), e mais 6 categorias menores. **Atualizar a decisão de proxy de `P`:
usar `Segmento` (não `Indústria` da base de clientes) e `Offering`.**

**Duração de projeto: estimativa de baixa confiança, não usar sem
ressalva.** `AVG(DATEDIFF(dt_alteracao, dt_criacao))` para projetos em
execução deu 177,6 dias (sd 189,2). `dt_alteracao` é um campo de auditoria
("data da última alteração do registro"), não necessariamente a data de fim
do projeto. Esse número pode servir de teto grosseiro de duração, não como
duração real calibrada. Não achamos um campo de "data de fim" claro na
base de projetos nesta rodada.

### Quarta rodada (2026-09-01): correlações extras e investigação do join

**Correlações extras para a cópula:**

* `pct_alocacao` (`Percentual de alocação do N1`) × `nivel_senioridade`:
  **+0,52** (moderada-forte), calculada sobre os 3957 pares onde o join
  `ID`=`Cód. Colaborador` bate (ver ressalva de cobertura acima).
* `horas_mes` × `nivel_senioridade` (usando a coluna `nivel` própria da
  fonte de alocação de pessoas por projeto, sem depender do join fraco): **-0,55**,
  moderada-forte e **negativa**: quanto mais sênior, menos horas de execução
  por mês. Faz sentido de negócio (cargos mais seniores dedicam mais tempo a
  venda/gestão, menos a entrega faturável).
* **`horas_mes` por `sigla_cargo` (estratificado, sem precisar de
  correlação):** confirma o mesmo padrão de forma muito mais nítida.
  Estagiários (`INT`) em torno de 112h/mês, cargos de execução
  (`AN`/`CT`/`SCT`/`TL`) na faixa de 141-146h/mês (pico da curva), caindo
  para `EMG`/`MG` (~90-137h), despencando em `SMG`/`PRI`/`DIR`/`MD1-MD5`
  (12-60h/mês). **Implicação para o gerador:** `horas_mes` em `R`/Camada 3
  deve ser amostrado condicional ao cargo (ou por cópula com
  `nivel_senioridade`), não com uma distribuição única para todo mundo.

**Investigação da lacuna de 36% no join `ID`=`Cód. Colaborador`: não
totalmente resolvida, mas descartei duas hipóteses.**

* **`Billado` não explica a lacuna.** Praticamente 100% dos colaboradores,
  com ou sem match, têm ao menos uma linha `Billado = Sim`. Não é uma
  distinção entre "terceirizado faturável" e "não faturável".
  Não é isso.
* **Formato da chave não explica.** Prefixo (`ID` + dígitos) é idêntico nos
  dois grupos, comprimento igual (7 caracteres). Não é problema de formato.
  Não é isso.
* **Hipótese que ganhou algum suporte, não conclusiva: turnover.** Entre os
  sem match, a proporção de alocações com `Status da alocação = Finalizado`
  é levemente maior (84% vs. 71% no grupo com match), consistente com gente
  que já saiu da empresa há mais tempo que a tabela de entidades atual não
  cobre mais. Não é conclusivo, só uma tendência na direção esperada.
  Não persegui mais fundo por não ser bloqueante: a ressalva sobre a
  estimativa de cold start já está registrada acima e é suficiente para
  seguir com o gerador.

**`localizacao` e `disposicao_viagem`: calibração real puxada agora (não
eram suposição, só faltava a consulta).** `Estado` (ativos, n=285): São Paulo
182 (64%), Minas Gerais 70 (25%), Rio de Janeiro 13, e mais 8 estados com 1-6
cada (cauda longa). Viagem nacional: 175 sim (61%), 110 não. Viagem
internacional: 185 sim (65%), 100 não. As duas perguntas de viagem são
respostas de formulário (STRING "SIM"/"NAO"), tratar como binária ao
codificar.

### Quinta rodada (2026-09-02): tabela de taxa por cargo e custo/margem por frente-mês

**Achado: tabela de taxa por hora é uma tabela de taxa (rate card), não uma
amostra de custo por pessoa.** O "desvio-padrão" de `Taxa` por `Sigla cargo`
registrado na Segunda rodada media dispersão *histórica* (reajustes de
tarifa ao longo do tempo, coluna `Início vigência`, a maioria das siglas com
3 a 5 revisões), não variação real entre pessoas do mesmo cargo num mesmo
momento. Filtrando para a vigência mais recente por sigla, cada uma das 44
siglas tem uma taxa vigente única, sem dispersão dentro do cargo:

| Sigla cargo | Taxa vigente (R$/h) | Sigla cargo | Taxa vigente (R$/h) |
|---|---|---|---|
| SO_INT | 91,45 | SO_EMG / IT_EMG | 499,06 |
| INT / IT_INT | 112,84 | TL | 518,90 |
| SO_TR / IT_TR | 196,69 | EMG | 567,11 |
| TR | 237,70 | SO_MG / IT_MG | 674,69 |
| SO_AN / IT_AN | 268,22 | MG | 751,33 |
| SO_SAN / IT_SAN | 294,52 | SO_SMG / IT_SMG | 834,51 |
| AN | 311,74 | SMG | 940,38 |
| SAN | 330,26 | IT_PRI / IT_DIR | 944,62 |
| SO_CT / IT_CT | 331,35 | PRI / DIR | 1.095,98 |
| CT | 380,88 | IT_MD1 | 1.196,38 |
| SO_SCT / IT_SCT | 386,66 | IT_MD | 1.267,02 |
| SCT | 444,09 | IT_MD2 / MD1 | 1.337,65 |
| SO_TL / IT_TL | 452,23 | IT_MD3 | 1.471,25 |
| | | MD2 | 1.503,15 |
| | | MD3 | 1.627,98 |
| | | MD | 1.722,61 |
| | | MD4 | 1.859,73 |
| | | MD5 | 2.066,00 |

**Implicação para o gerador:** `custo_carregado` deve ser um *lookup*
praticamente determinístico por cargo, não uma amostragem com ruído. A
dispersão real de `custo_carregado` na população inteira (SD 453,50,
assimetria 0,93, já registrada na Segunda rodada) vem inteiramente da
mistura de cargos na pirâmide, não de ruído dentro de um mesmo cargo. Como
o gerador sintético só tem `nivel_senioridade` (N4-N0), não a sigla de
cargo granular, mapear nível para uma sigla/taxa representativa é uma
decisão de projeto (não uma leitura direta do dado), a documentar na
função geradora.

**Custo e receita por frente-mês (fonte de receita mensal por projeto, n=5000),
para o candidato (c) de margem:**

| Campo | Fração zero | Média geral | Desvio-padrão geral | Assimetria | P25 dos positivos | P50 dos positivos | P75 dos positivos |
|---|---|---|---|---|---|---|---|
| coluna de receita própria (receita) | 15,9% (795/5000) | 192.076,84 | 202.058,84 | 1,85 | 46.105,50 | 136.901,46 | 268.683,84 |
| coluna de receita de estagiários (custo interno) | 44,7% (2235/5000) | 14.315,61 | 17.980,01 | 1,75 (skew) | 16.000,00 | 21.000,00 | 26.250,00 |
| coluna de receita da coligada | 95,8% (4788/5000) | 716,35 | 4.919,21 | 7,67 | - | - | - |

A coluna de receita da coligada é quase sempre zero/nulo (só 4,2% dos registros têm
valor), não é uma boa variável primária para compor margem: descartada como
candidata a insumo direto do gerador, sem prejuízo de retomar se a EDA
achar relevância nos poucos casos não nulos. coluna de receita própria e
coluna de receita de estagiários têm formato *hurdle* semelhante ao de
`valor_contrato` (massa em zero mais cauda assimétrica), calibráveis pelo
mesmo mecanismo já implementado (`gerar_variavel_hurdle_lognormal()`, casamento de
quantis).

**Achado sobre `pipeline_demandas`: as contagens por situação não formam um
funil de coorte, são um censo transversal.** A ordem Ideia (92) → Reunião
Inicial (123) → Lead (127) → Proposta (209) → Projeto executando (952) NÃO
é decrescente, o oposto do que um funil de sobrevivência precisaria ser
para dar taxa de avanço/perda por estágio de forma direta. Isso é esperado
sob fila (a população num estágio num dado momento reflete taxa de entrada
vezes tempo médio de permanência, não quantas oportunidades já passaram por
ali), não um sinal de erro na contagem. Sem timestamp de mudança de
estágio por oportunidade (não levantado nesta rodada, não achado nesta
reconciliação), não dá para calibrar probabilidade de avanço estágio a
estágio de forma estatisticamente válida. Implicação: `pipeline_demandas`
no gerador deve ser um sorteio multinomial do estágio atual por
oportunidade, usando o censo como peso, não uma simulação sequencial de
avanço/perda, o que é uma simplificação documentada frente ao que a
sequência de construção presumia possível. (Revisado na Sexta rodada: as
datas de estágio existem, o funil de coorte é calibrável.)

### Sexta rodada (2026-09-10): última varredura antes do corte de acesso

Contexto: último dia de acesso ao ambiente. Varredura final de agregados
para os campos que seguiam como suposição documentada. Só `COUNT`,
`GROUP BY`, `AVG`, `STDDEV`, `percentile_approx` e `DATEDIFF`, nunca linha
de dado. Headcount reverificado, segue 285 ativos, dados estáveis. Aplicar
estes números aos geradores é trabalho de código posterior, feito sobre
número já registrado aqui.

**`tipo_contrato` (campo `Modelo` da base corporativa de colaboradores, 285
ativos). Semântica do campo agora confirmada pela inspeção dos próprios
valores, não é mais "não confirmada":**

| Valor real de `Modelo` | n | % |
|---|---|---|
| EMPREGADO GERAL CLT | 153 | 53,7 |
| CONTR INDIVIDUAL DIRETOR NAO EMPREGADO SEM FGTS | 89 | 31,2 |
| ESTUDANTE ESTAGIARIO | 42 | 14,7 |
| CONTRIBUINTE INDIVIDUAL AUTONOMO EM GERAL | 1 | 0,4 |

Mapeamento sugerido para `tipo_contrato` binário: CLT (EMPREGADO CLT mais
ESTAGIARIO) 195 (68,4%), não empregado / PJ (DIRETOR mais AUTONOMO) 90
(31,6%). O bloco ESTAGIARIO (42) coincide exatamente com a contagem de N4
na pirâmide: estagiário e nível de base são o mesmo grupo, o que sugere
amarrar `tipo_contrato` a `nivel_senioridade` numa revisão futura.
Substitui a suposição de 50/50 do gerador.

**`idiomas[]` (respostas de formulário na base corporativa de colaboradores,
285 ativos, sem nulo):**

| Nível | Inglês (n) | Espanhol (n) |
|---|---|---|
| FLUENTE ou NATIVO | 61 | 8 |
| AVANCADO | 132 | 18 |
| INTERMEDIARIO | 73 | 54 |
| BASICO | 13 | 147 |
| NENHUM ou NAO POSSUO | 6 | 58 |

Inglês: 97,9% têm ao menos básico, 67,7% avançado ou fluente, praticamente
universal como o gerador já assume. Espanhol: 79,6% têm algum nível, mas
51,6% só básico. Contar só intermediário ou acima dá 28,4%, próximo da
suposição atual de 0,3. Recalibração: presença de espanhol em algum nível
sobe de 0,3 para cerca de 0,8, com a massa concentrada em básico.

**Duração real de alocação e de projeto (fonte de alocações, campos
`Data de início` e `Data de fim`, sem nulo):**

| Métrica | Alocação individual (n=4999) | Projeto, span das alocações (n=1834) |
|---|---|---|
| Média (dias) | 51,6 | 68,8 |
| Desvio-padrão | 86,8 | 119,7 |
| P25 / P50 / P75 | 4 / 18 / 60 | 4 / 18 / 81 |
| Assimetria | 3,97 | 3,6 |
| Máximo | - | 1184 |

Substitui a duração de baixa confiança da Terceira rodada (177,6 dias de
`dt_alteracao`, campo de auditoria). A mediana real é 18 dias, muito
menor. A forma é fortemente assimétrica à direita, mais compatível com
log-normal por casamento de quantis (mediana 18, P75 60) do que com a
Exponencial de média 177,6 que o gerador de `projetos` usa hoje.

**Funil de vendas: as datas de estágio por oportunidade existem
(base de oportunidades do CRM).** A Quinta rodada registrou que não havia
timestamp de estágio. Há: `Data Lead`, `Data Reunião`, `Data Proposta`,
`Data Projeto`, `Data Rec/Can`. Censo de 4233 oportunidades:

| Estágio atingido | n | % das com Data Lead |
|---|---|---|
| Data Lead | 4107 | 100 |
| Data Reunião | 3341 | 81,3 |
| Data Proposta | 2664 | 64,9 |
| Data Projeto (ganho) | 967 | 23,5 |
| Data Rec/Can (perdido) | 2785 | - |

Taxas de avanço condicionais: Lead para Reunião 81,3%, Reunião para
Proposta 79,7%, Proposta para Projeto 36,3%. Tempo entre estágios
(mediana, dias): 2, depois 3, depois 16 (médias 10,6 / 13,8 / 30,
assimétricas). Isso habilita um funil de coorte sequencial real para
`pipeline_demandas`, no lugar do sorteio multinomial transversal que a
Quinta rodada tinha registrado como única opção. A revisão do gerador é
trabalho de código posterior.

**Mix de senioridade nas alocações (fonte de alocação de pessoas por projeto).** O
campo `nivel` desta view usa agrupamento grosso (N3 e N4 juntos 10457, N2
7397, N1 6468, MDs 6452) e tem valores de texto livre sujos misturados,
não serve para `mix_papeis` fino. O campo `sigla_cargo` (as 44 siglas da
rate card) tem a distribuição de senioridade de quem de fato é alocado,
dominada por INT (3646), AN (2467), PRI (2192), CT (2148), SAN (2002), com
cauda longa e alguns valores malformados. Útil como alvo de calibração do
`papel` em `R` numa revisão futura, não aplicado agora. `mix_papeis` por
composição de nível dentro do time segue sem fonte limpa.

**`modelo_precificacao` e `local` / `on_site`: confirmados sem fonte.**
Varridas as tabelas de projeto e CRM. Para precificação, o único candidato
próximo é `Negócio Tipo` (base de oportunidades do CRM), que é origem do
negócio (Selling 3053, UpSelling 1180), não modelo de cobrança.
`etapa_modalidade` da base de projetos é 91% vazia. Nenhuma coluna de
`local` / `on_site` / presencial / remoto em nenhuma das quatro tabelas.
`faturamento_internacional` da base de projetos está preenchido em
2016 de 4799 registros, e desses 99,7% são domésticos. Mantêm o rótulo
Suposto do dicionário, agora com a busca registrada como feita.

**`esforco_estimado` pré-venda (base de pipeline do CRM, n=1845):**
`Horas Semana` tem mediana e P75 ambos exatamente 40, um valor default que
os usuários não alteram, não informativo. `Duração` (em semanas) tem
mediana 12, P75 19, assimetria 1,49. Cross-check secundário da duração,
não substitui a calibração de `horas_mes` pós-venda (mediana 120h/mês) que
o gerador já usa.

**Cobertura real do mapa de skills (228/229 e-mails distintos das 3
planilhas de `data/raw/skills/` contra os 285 ativos).** Medida por
interseção de hashes, sem extrair nenhuma linha de dado identificável do
ambiente: os e-mails das planilhas foram normalizados (minúsculo, sem
espaço) e transformados em SHA-256 localmente, e só a contagem de hashes
coincidentes saiu do Databricks, nunca um e-mail. Resultado: **138 dos 285
ativos (48,4%) têm ao menos uma resposta de skills**, todos batendo contra
o e-mail corporativo, nenhum contra o e-mail pessoal. Do outro lado,
138 dos 229 respondentes distintos (60,3%) correspondem a um ativo atual,
o resto provavelmente já não está na empresa ou tem e-mail divergente
entre as duas fontes. As planilhas não têm data de resposta centralizada
por arquivo para confirmar se a diferença é por turnover ou por
defasagem temporal da coleta, então a explicação exata fica como
limitação, não um número fechado. Fecha o item de cobertura de skills da
definição de pronto da wave 2 (EDA).

## Consulta ao data owner de RH (2026-09-19)

Respostas obtidas por consulta direta, durante a wave 2. Não vêm de query:
são declarações do dono do dado sobre semântica e processo, e é isso que
as torna úteis, porque semântica não é derivável de agregado. Cada item
abaixo diz o que muda no projeto.

**Cobertura do formulário geral de skills isolado: pode assumir 48,4%.**
Confirma a escolha que `sortear_respondentes_formulario()` já fazia. Deixa
de ser teto conservador adotado por falta de número e passa a ser
aproximação sancionada pelo dono do dado.

**Os três formulários tiveram públicos diferentes, e isso contraria o
gerador.** Um foi geral, um só para a empresa coligada e um só para pessoas de Digital.
O gerador hoje usa **um único** conjunto de respondentes compartilhado por
todos os sinais de formulário, o que o bloco II.3 adotou justamente para
corrigir o defeito oposto (cobertura sorteada de forma independente por
sinal). A verdade está no meio: os sinais do 2.1 (experiência setorial,
expertise funcional, soft skills) compartilham um público, e `skills_hard`,
que vem de Digital e da coligada, tem **outro**, com sobreposição parcial (19 dos
137 respondentes do 2.1 também responderam o Digital Skills, 0 responderam o
de Tecnologia). **Fica como questão aberta** a cobertura de cada formulário
entre os 285 ativos, sem a qual não dá para dimensionar os três públicos.

**O formulário virou processo recorrente, anual, com rodada até o fim de
2026.** Muda o que o modelo pode propor sobre atualização de perfil: a
recência de skill deixa de ser um artefato de campanha única e passa a ter
cadência conhecida. Reforça a modelagem em ondas de
`gerar_skills_consultor()`, que era suposição de forma.

**`skills_requeridas[]` não existe: confirmado pelo dono do dado.** Sobe de
"nenhuma fonte encontrada" para confirmação explícita. O achado de pesquisa
registrado em `predefinicoes.md` fica fechado.

**Candidatos cogitados e preteridos não são registrados: confirmado.**
Confirma a terceira linha de `verificar_amostra_de_exposicao_aleatoria()`,
que o bloco II.4 tratava como leitura própria. A ausência de contrafactual
é fato sobre a organização, não suposição.

**"Frente" é termo sobrecarregado, e nenhum dos sentidos é "projeto".**
São ao menos dois conceitos distintos, e a diferença importa:

* **frente interna**: iniciativa da casa, muitas vezes desassociada de
  projeto, com participação **voluntária**. Uma pessoa pode ser da frente
  interna de RH enquanto atua em projetos de cliente.
* **frente ou capability dentro da vertical de Digital**: Data Science,
  Data Analytics, Tech Strategy, Solutions, Cyber Security, Digital Products
  & Platforms.

Consequências. A simplificação de `gerar_margem_frente_mes()`, que colapsa
frente em projeto, fica registrada como **provavelmente incorreta** e não só
como falta de número: se frente interna é voluntária e desassociada de
projeto, custo de frente e custo de projeto não são a mesma coisa. E a qual
dos sentidos fonte de receita mensal por projeto se refere passa a ser questão
aberta, porque a resposta muda a interpretação da margem.

**A afinidade entre atributo do consultor e offering do projeto é mecanismo
real.** "A tendência é que pessoas dessas capabilities sejam alocadas em
projetos específicos dessas offerings." É âncora para o bloco II.6, e muda o
estatuto do viés de seleção do bloco II.4: a **magnitude** plantada no
gerador segue sendo propriedade do código, mas a **existência** de
pareamento por afinidade deixa de ser suposição e passa a ser confirmada
pelo dono do processo.

## Segunda consulta ao data owner, bases comerciais (2026-09-20)

Rodada de consultas agregadas sobre o catálogo comercial, feita a pedido
depois que o bloco II.5 encontrou margem estrutural implausível. Traz
**errata da consulta anterior**: três leituras daquela rodada estavam
erradas e são corrigidas aqui. Nada identificável foi transcrito.

### Errata da consulta de 2026-09-19

| Antes | Agora | Prova |
|---|---|---|
| fonte de receita mensal por projeto seria custo mensal | É a **receita** do projeto distribuída por mês e origem | soma das colunas de receita própria, de subcontratados e da coligada sobre a receita total = **1,000 de p10 a p90** em 724 versões de projeto |
| Fator 1,65 seria conflito de escala | É **receita sobre despesa**, `1/(1 − 0,394)` | CRMs com uma versão dão 1,654 e com várias dão 1,629; dupla contagem daria 1,0 contra 2,0 |
| Truncamento seria prefixo por ID | É **corte cronológico** por data de criação do projeto | IDs têm buracos em toda lista, inclusive nas sem teto |
| Coligada e subcontratados seriam repasse de custo | São **componentes de receita** | Decorre da primeira linha |

Uma razão que fica exatamente em 1,000 do décimo ao nonagésimo percentil é
identidade contábil, não correlação. A leitura de receita está fechada.

### `Taxa` é preço de venda (confirmado, e agora com prova direta)

Além dos indícios já registrados, a rodada mediu que a receita mensal do projeto
é **0,87 vezes horas planejadas vezes `Taxa`**, com valor implícito por hora
entre R$255 e R$426. A `Taxa` é o preço pelo qual a receita é apurada, não
custo. O achado II.5.5 da EDA fica fechado.

O que o 0,87 representa, se desconto comercial ou outra convenção, não foi
verificado.

### Custo por cargo existe, e é **salário base por hora**

tabela de custo por cargo, grão sigla de cargo por ano, 26.000
linhas, 1.137 projetos, 30 siglas, 2023 a 2027. Mediana de 2025 em R$/h:

| Sigla | R$/h | Sigla | R$/h | Sigla | R$/h |
|---|---|---|---|---|---|
| INT | 70 | SCT | 248 | SMG | 405 |
| TR | 119 | TL | 271 | PRI | 474 |
| AN | 163 | MG | 327 | MD1 | 745 |
| SAN | 183 | EMG | 334 | MD4 | 1.093 |
| CT | 218 | | | | |

Razão custo sobre `Taxa` entre 0,40 e 0,72, crescente com a senioridade.
Reajuste de cerca de 7% ao ano.

**Confirmado pelo usuário em 2026-09-20: é por hora e não inclui encargos, é
salário base.** Isso importa para o gerador: o campo do dicionário chamado
`custo_carregado` **não** corresponde a esta coluna. Custo carregado real é
salário base mais encargos, benefícios, décimo terceiro, férias e FGTS, e o
multiplicador não está em lugar nenhum das bases consultadas. A calibração
possível é: base por hora com âncora real, fator de carga como suposição
declarada.

**Corrupção de ingestão:** ao menos 275 linhas de 2025 com o valor sem o
ponto decimal (163,2975283 vira 1.632.975.283), afetando AN, CT, INT, MG,
MD2 entre outras. Média simples fica inutilizável. Usar mediana ou filtrar
`custo <= 3 * Taxa`.

Custo por pessoa não existe no catálogo comercial. Ficaria no catálogo
de folha de pagamento, que não foi aberto e não é necessário: por cargo é a
granularidade que o modelo usa.

### Zero na coluna de receita de estagiários é ausência estrutural de estagiário

44,7% de zeros e 0% de nulos, com a receita própria igual à soma das receitas de nota fiscal e de
estagiários valendo em 100% das linhas. Em 294 pares de projeto e mês
cruzados com horas planejadas, 112 de 112 zeros não têm nenhuma hora de
estagiário e nenhum zero aparece onde há hora de estagiário.

Como o campo é receita, zero significa mês sem **receita** de estagiário.
Persistência por projeto, em 597 projetos com três ou mais meses ativos: 29% nunca,
47% sempre, 24% mista.

**Isto derruba o achado II.5.3 da EDA**, que lia os 44,7% como incompletude
de apontamento de custo e concluía que a margem por frente e mês era
inutilizável. Não é incompletude, e não é custo.

### `ID` e `Cód. Colaborador` são a mesma chave

O `ID` da versão de referência é a chave de funcionário. Na versão intermediária da base corporativa de colaboradores
casam 432 de 511 códigos, e nos casados o primeiro nome coincide em 100% e o
nome completo em 98,1%. A versão de referência perde cerca de 100 códigos; usar a
versão completa da base corporativa de colaboradores eleva a cobertura para cerca de 98%. Na versão
intermediária, 79 códigos, ou 310 linhas de alocação (6%), não casam, e só 9 códigos
não casam em nenhuma tabela testada.

Duas ressalvas. As chaves do sistema contábil não servem para este join,
com 18% de coincidência, que são colisões. E a cadeia do ERP faz o oposto: o
identificador externo do cadastro de funcionários do ERP casa com a chave do
sistema contábil, não com a chave de funcionário.

Fecha a ressalva do bloco II.2, que registrava o join como suposto e nunca
confirmado.

### "Frente" na fonte de receita mensal por projeto é nome enganoso

O grão é **projeto por mês**, com a chave de projeto igual à da base comercial.
Nenhuma coluna nomeia frente; o nome é herdado de um sistema anterior. A
descrição de frente só existe do lado das pessoas.

**Valida a simplificação do gerador** que colapsa frente em projeto, que a
rodada anterior tinha levado a marcar como provavelmente incorreta. Está
correta.

### Teto de 5.000 linhas: da extração, com corte cronológico

O limite é da extração e não da tabela. Nas 199 versões do histórico Delta
toda escrita foi sobrescrita com exatamente 5.000 linhas. Tabelas com teto
aparente: a fonte de receita mensal por projeto, as tabelas de despesa
mensal, de distribuição e de parcelas de comissão e a de informações de
projeto em 5.000; a fonte de alocações em 4.999. Sem teto: a fonte de
alocação de pessoas por projeto com 30.948, a tabela de custo por cargo com
26.000 e a de parcelas de faturamento com 10.878.

O corte é por data de criação da versão do projeto: a cobertura da view mensal cai
a zero para projetos criados a partir do terceiro trimestre de 2025, e **nenhum
dos 772 projetos de 2026 tem valor mensal**.

**O viés foi medido, e a conclusão é mista.** Nível de projeto não é enviesado:
receita mediana, semanas, margem líquida e desconto ficam estáveis entre
coberto e não coberto, em 2024, 2025 e 2026, e a base comercial de projetos é completa com
2.330 linhas. Mas o **mix de offerings muda**: DATA ANALYTICS & AI passa de
cerca de 3% em 2024 para 10,5% em 2026. Tudo que depende da view mensal, isto
é, forma da distribuição mensal, participação de estagiários, subcontratados
e coligada, e meses por projeto, representa projetos de 2023 a meados de 2025 e
subrepresenta a offering que mais cresce.

### O fator 1,65 é receita sobre despesa

Não é dupla contagem nem diferença de escopo. A soma mensal é a própria
receita do projeto, e a razão contra a despesa sem imposto é `1/(1 − 0,394)`, com
0,394 sendo a margem mediana antes de imposto.

**Risco real e separado:** 193 de 227 CRMs têm mais de uma versão de projeto na
view, somando 696 versões. Agregar por projeto sem escolher uma versão
duplica receita. Não era a causa do 1,65, mas vale para qualquer agregação
pela chave de CRM.

### Ponte entre frente e offering: existe como afinidade, é fraca em volume

**Não há de-para documentado.** Há quatro taxonomias de offering em
circulação, com grafias divergentes, e a do projeto carrega rótulos legados como
"DIGITAL" genérico e "IT STRATEGY". O pilar digital das
tabelas comerciais de Digital é o mais próximo de um de-para informal.

**A offering do projeto não é a mesma do CRM.** Em 568 projetos casados, a
igualdade exata de rótulo é de 24,1%, e sobe a 72,0% depois de normalizar
seis grafias. O restante são reclassificações reais, não erro de escrita.

**A ponte empírica, ponderada por horas:**

| Frente da pessoa | Offering do projeto com maior afinidade | Share das horas | Lift |
|---|---|---|---|
| DATA ANALYTICS | DATA ANALYTICS & AI | 15,9% | 9,94 |
| TECH CONSULTING | TECH CONSULTING | 42,9% | 4,50 |
| DIGITAL PRODUCTS & PLATFORMS | DIGITAL | 18,2% | 3,27 |
| DATA SCIENCE | TECH CONSULTING | 16,6% | 1,75 |

Nos dois sentidos, para Data Analytics e Data Science juntas: só **11,6%**
das horas dessas frentes caem em projetos de DATA ANALYTICS & AI, e das horas de
projetos dessa offering elas respondem por 21,2% do total.

**Leitura para o bloco II.6.** A tendência que o dono do dado de RH descreveu
existe, e o lift confirma: quase dez vezes a base para Data Analytics. Mas a
maior parte das horas de qualquer frente cai nas offerings grandes, E&E e
Strategy & M&A. O mecanismo é `P(offering | frente)` com massa dispersa, e
**não** pareamento de um para um. Isso é calibração direta para o viés de
seleção que o bloco II.4 hoje planta com `prob_vies = 0,7`, que
provavelmente superestima a concentração.

Ressalvas de cobertura que limitam esses números: apenas cerca de 50% das
linhas de alocação resolvem frente, 42% das horas não têm chave para o RH, e
os grupos das frentes Digital têm de 5 a 9 pessoas. Frente é vazia para a
vertical Consulting, e o valor "NENHUMA" cobre 200 pessoas e 47% das horas.

### Calibração de nível de projeto, com âncora real

Da base comercial de projetos, completa e sem teto, última versão por CRM, 556 projetos:

| Grandeza | Calibração |
|---|---|
| Receita total | lognormal, mu 13,032, sigma 1,177; p10/50/90 = 88k/457k/1,81M |
| Semanas | lognormal, mu 2,451, sigma 0,973; p10/50/90 = 3,6/11,6/43,6 |
| Imposto | 7% em 90% dos projetos, 14% nos 10% restantes; média efetiva 8% da receita |
| Desconto | quantis 8/23/37 em pontos |
| **Margem líquida** | p05 0,175 · p25 0,272 · **p50 0,320** · p75 0,345 · p95 0,401 |
| Prejuízo | cerca de **2%** dos projetos |
| Margem antes de imposto | p25/50/75 = 0,344/0,394/0,432 |
| Por offering | mediana entre 0,28 e 0,34, sem diferença material; não segmentar |

**Esta é a âncora de margem que faltava.** O bloco II.5 media plausibilidade
contra julgamento; agora há referência. E o contraste é duro: o sintético
produz 32,8% de projetos com margem negativa contra 2% reais.

Convenção de sinal a respeitar: na base comercial de projetos, as colunas de despesa
total, de despesa com impostos e de percentual de impostos são **negativas**, e descontos e
gestão vêm em pontos percentuais inteiros. A margem é
`(receita + despesa) / receita` com despesa negativa.

### Calibração mensal, com ressalva de truncamento

Sobre a view truncada, meses com receita positiva, n = 4.205: lognormal com
mu 11,856 e sigma 1,377; p05/25/50/75/95 = 19k/90k/172k/304k/589k. A
decomposição dá sigma entre projetos de cerca de 1,32 e intra projeto de 0,39.

Forma intra projeto, como desvio em log do nível médio do projeto: primeiro mês
−0,38, segundo +0,19, meio +0,15, último −0,44. Sem sazonalidade de
calendário. Meses por projeto guiados pela duração, com mediana 4, p10 de 1 e p90
de 12. Semanas no mês derivadas do calendário, não sorteadas.

Componentes de receita: subcontratados com Bernoulli 0,065 e lognormal mu
10,648 sigma 1,185; a coligada com Bernoulli 0,05 e lognormal mu 8,987 sigma
1,225; em ambos o campo de faturamento iguala o total em 100% das linhas.
Estagiários entram por persistência de projeto, com 29% nunca, 47,2% sempre e
23,8% mista, e participação lognormal mu −2,263 sigma 0,683.

Linhas totalmente zeradas somam 15,9%, crescendo de 0,5% em 2023 para 27,8%
em 2025, e são provável artefato de projetos sem par na base comercial de projetos.

### Alertas de qualidade que afetam a tese

1. Teto de extração em várias tabelas, com corte cronológico. Elevar o limite
   depende do responsável pelo job de atualização.
2. fonte de alocações com 4.999 linhas: a cobertura de chave da consulta de confirmação foi
   medida em base possivelmente truncada.
3. Corrupção de ponto decimal na tabela de custo por cargo.
4. Sinais negativos na base comercial de projetos para despesa e imposto.
5. O nome da fonte de receita mensal por projeto engana: é receita.
6. Múltiplas versões de projeto por CRM na view.
7. `information_schema` defasado para algumas tabelas; usar `DESCRIBE`.
8. Grafias divergentes de offering entre projeto, CRM e NetSuite.
9. Frente vazia na vertical Consulting e valor "NENHUMA".
10. Taxas "Solutions" são cópia das de IT, e há taxas de 2027 que são
    projeção e não série observada.

### Não verificado nesta rodada

O significado exato da coluna de receita de nota fiscal e se o fator 0,87 é desconto
comercial. Se o teto está no conector ou em um `limit` no notebook. Custo por
pessoa. Cronograma real de receita por mês, já que as parcelas de
faturamento são emissão e vencimento, não alocação por origem. E o gerador
proposto pela consulta nunca foi executado, por indisponibilidade de
ambiente do lado de lá.

**Acrescentado em 2026-10-03, a partir do achado II.6.7.** Quando o `setor`
do projeto é preenchido: na abertura da oportunidade, na assinatura do
contrato ou já com o projeto em andamento. O dicionário registra o campo como
disponível sem dizer quando, e a resposta decide se o filtro de conteúdo serve
a um alocador prospectivo ou apenas à reavaliação retrospectiva da wave 4.

## Fechamento da wave 1 (2026-09-16)

Acesso ao Databricks e às planilhas de `data/raw/skills/` encerraram em
definitivo (09/09 e antes de 16/09, respectivamente). Revisão do que
tinha ficado como "próximos passos": o que dependia só de número já
registrado virou código; o que dependia de acesso ou arquivo vira
limitação permanente, não pendência.

**Aplicado ao gerador (código, sem depender de acesso):**

* `tipo_contrato` (68,4%/31,6%), presença de espanhol (~0,8), duração de
  projeto (log-normal por quantis, com teto de 2.368 dias contra cauda
  implausível) e funil de coorte sequencial em `pipeline_demandas`.
* Cobertura real de skills (48,4%) calibrando a fração de consultores com
  `skills_hard[]` vazio.

**Limitação permanente, não pendência (acesso encerrado, nada a reabrir):**

* Confirmação da definição quebrada da base de experiência profissional.
* Contagens agregadas de completude/cardinalidade por tabela candidata
  (só existência e tipo de coluna foram confirmados nesta reconciliação).
* Calibração de `intensidade` em `gerar_experiencia_setorial()` pela
  escala de experiência do formulário geral: a distribuição populacional
  por essa escala nunca chegou a ser agregada e registrada aqui, só a
  existência da escala. Sem o arquivo, não há mais como extrair esse
  número. `intensidade` segue `runif(0,1)`, suposição documentada.
* Crosswalk e-mail/nome → `consultor_id` linha a linha: as planilhas
  saíram da máquina antes de ser construído. Só a cobertura populacional
  agregada por hash (48,4%) ficou registrada.
* O de-para de escala de proficiência e a escolha de aba canônica
  ("Resolvido (2026-09-10)" acima) não têm ETL real para aplicar: a
  camada de acesso `fonte = "databricks"` nunca foi implementada como
  código (regra 6 do `CLAUDE.md` não permite persistir nem transformar
  dado real em pipeline versionada), e o gerador sintético nunca leu as
  planilhas brutas. Ficam registrados como decisão de projeto, não como
  código pendente.

**Pendência real, não bloqueante, fora da wave 1:** `mix_papeis`
(calibrar `papel` em `R` pela distribuição real de `sigla_cargo`, ver
`docs/roadmap.md`) segue aberto por decisão de design, não por falta de
número. Fica para quando `papel`/`R` for revisitado, possivelmente na
wave 3.

**Achado à parte, corrigido nesta revisão:** o item anterior desta seção
pedia testar se `Offering`/`Linha de Serviço` juntam contra
base de projetos. Já estava resolvido antes mesmo de ser escrito
aqui: a Terceira rodada, acima, tinha validado esse join pela chave de CRM
com ~98% de cobertura. Item removido por já estar coberto, não por ter
sido feito agora.
