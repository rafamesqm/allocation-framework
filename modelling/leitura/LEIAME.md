# Leitura qualitativa das discordâncias

A pipeline gera o modelo da rubrica em branco em
`outputs/tables/w3_rubrica_leitura_modelo.csv`, com os casos sorteados pela
semente 20261022.

Para registrar a leitura, copie esse arquivo para
`modelling/leitura/rubrica_preenchida.csv` e preencha ali os quatro campos de
cada caso: disponibilidade do recomendado no mês, compatibilidade de setor,
justificativa observável da escolha realizada e veredito (plausível,
plausível com ressalva, implausível ou indeterminável).

Nenhum alvo do `targets` escreve nesta pasta. Preencher o arquivo de
`outputs/tables/` perderia os vereditos no próximo `tar_make()`.

O avaliador é o autor, e sobre dados sintéticos a leitura julga a coerência
do gerador, e não o processo real de alocação.
