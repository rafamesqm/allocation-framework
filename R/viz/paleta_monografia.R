#' Paleta de cores das figuras da monografia
#'
#' Fonte única de cor do projeto. As figuras vão para um documento impresso,
#' então a paleta é fixa e clara (sem modo escuro), suave, e cada cor faz um
#' papel só, nunca decoração.
#'
#' **Como estes valores foram escolhidos.** Não no olho. As cinco checagens
#' computáveis de paleta (banda de luminosidade e piso de croma em OKLCH,
#' separação entre matizes sob protanopia e deuteranopia simuladas pelo
#' modelo de Machado, Oliveira e Fernandes (2009), piso de distinção sob
#' visão normal, e contraste WCAG contra o fundo) foram rodadas sobre cada
#' candidata. O que se buscou foi a paleta mais suave, isto é, de menor
#' croma, que ainda passasse em todas. Resultado registrado, no fundo claro
#' `#fcfcfb`:
#'
#' * categórica, pares adjacentes: pior par sob daltonismo dE 8,1 (alvo 8),
#'   pior par sob visão normal dE 17,3 (piso 15), contraste mínimo 3,04:1;
#' * categórica, todos os pares: passa com 3 slots (dE 8,1 e 21,4);
#' * rampa ordinal: luminosidade monotônica, menor salto 0,073 (piso 0,06),
#'   extremo claro a 2,12:1 (piso 2,0), matiz única;
#' * divergente: polos a dE 27,4 sob visão normal e 17,3 sob protanopia,
#'   neutro com croma 0,006, ou seja, cinza de verdade.
#'
#' Suavizar mais do que isso não é possível sem quebrar alguma checagem: é a
#' separação entre matizes que impede, não gosto. O que de fato libera
#' suavidade é usar menos séries, por isso a paleta categórica tem cinco
#' slots e não oito.
#'
#' **Categórica, para identidade.** Cinco matizes em ordem fixa, atribuídas
#' em sequência e nunca recicladas. Série de número seis não ganha matiz
#' nova: agrupe o excedente em "Outros" ou facete. Isso importa aqui,
#' porque `setor` tem 10 categorias e `tipo_projeto` tem 12. Em figura onde
#' qualquer par de marcas pode encostar (dispersão, pequenos múltiplos),
#' o teto cai para as três primeiras.
#'
#' **Ordinal, para ordem.** Uma matiz só, do claro ao escuro. Vale sempre
#' que trocar a ordem das categorias mudaria o sentido.
#' `nivel_senioridade` é o caso central (N4 < N3 < N2 < N1 < N0): pintar
#' senioridade com matizes categóricas jogaria fora a ordem, que é a
#' informação principal. Vantagem extra num trabalho impresso: uma rampa de
#' luminosidade sobrevive à impressão em preto e branco, uma escala de
#' matizes não.
#'
#' **Divergente, para polaridade.** Dois polos opostos com cinza neutro no
#' meio, para a nota da avaliação de desempenho, que não é só ordenada, é
#' bipolar em torno do esperado (`ABAIXO` / `DENTRO` / `ACIMA`).
#'
#' @return Lista nomeada com `categorica` (5 matizes em ordem),
#'   `ordinal_senioridade` (5 passos, nomeados por nível),
#'   `divergente_nota` (3 passos, nomeados pela nota) e `chrome` (fundo,
#'   tintas de texto, grade, subgrade e moldura).
paleta_monografia <- function() {
  list(
    categorica = c("#185ba9", "#bf5125", "#0a8d61", "#c58500", "#be6386"),
    ordinal_senioridade = c(
      N4 = "#97b2d6", N3 = "#739bcf", N2 = "#5083c5",
      N1 = "#2f6cb8", N0 = "#1a569d"
    ),
    divergente_nota = c(
      ABAIXO = "#b94642", DENTRO = "#8a8a86", ACIMA = "#185ba9"
    ),
    chrome = c(
      fundo = "#fcfcfb",
      tinta_primaria = "#0b0b0b",
      tinta_secundaria = "#52514e",
      tinta_suave = "#898781",
      grade = "#e1e0d9",
      subgrade = "#f0efec",
      moldura = "#c3c2b7"
    )
  )
}
