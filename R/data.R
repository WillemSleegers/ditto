#' Example sentences for baseline estimation
#'
#' A small multilingual set of distinct-meaning sentences. No two are
#' translations or paraphrases of one another, so randomly paired sentences are
#' unrelated -- which is what [bertscore_baseline()] needs to estimate the score
#' an embedding model assigns to unrelated text. The English subset is used as
#' the default corpus by [bertscore_baseline()]; the Dutch and French rows let
#' you estimate per-language baselines for multilingual models such as bge-m3.
#'
#' @format A [tibble][tibble::tibble] with two columns:
#' \describe{
#'   \item{language}{ISO 639-1 language code: `"en"`, `"nl"`, or `"fr"`.}
#'   \item{text}{The sentence.}
#' }
#' @seealso [bertscore_baseline()]
#' @export
example_sentences <- tibble::tibble(
  language = c(rep("en", 10), rep("nl", 7), rep("fr", 7)),
  text = c(
    # English
    "please rate your overall experience with us",
    "what is your annual household income",
    "the weather today is sunny and warm",
    "i went to the market to buy bread",
    "the train arrives at nine in the morning",
    "she enjoys reading books about history",
    "the company reported strong quarterly earnings",
    "climate change affects many coastal cities",
    "my dentist appointment is next thursday",
    "the children are playing in the garden",
    # Dutch
    "de kat slaapt op de bank",
    "ik moet vandaag de auto wassen",
    "het museum is op maandag gesloten",
    "wij gaan volgende week op vakantie",
    "de soep heeft te veel zout",
    "hij speelt gitaar in een band",
    "de vergadering is verzet naar vrijdag",
    # French
    "la tour eiffel est tres haute",
    "j'ai oublie mon parapluie ce matin",
    "le cafe est beaucoup trop chaud",
    "nous regardons un film ce soir",
    "les fleurs poussent au printemps",
    "mon ordinateur est tombe en panne",
    "elle court dans le parc tous les matins"
  )
)
