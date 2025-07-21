# length(readLines("words_moby.txt")) # 466550
# length(readLines("words_yawl.txt")) # 264097
# length(readLines("words_scowl.txt")) # 169819
# length(readLines("words_beale3.txt")) # 82532

# https://github.com/dwyl/english-words (full list)
load_words_moby <- function() {
  readLines("words_moby.txt") |>
    stringr::str_replace_all(moby_repls()) |> # keep some initialisms etc
    # exclude elements with unwanted characters or digits
    stringr::str_subset("[/!\\.0-9]", TRUE) |>
    stringr::str_subset("^[\\-']", TRUE) |>
    exclude_prefixes() |>
    exclude_possessives() |>
    stringr::str_replace("court-tial", "court-martial") # error correction
}

# http://wordlist.aspell.net/12dicts/ (3of6all list)
load_words_beale <- function() {
  readLines("words_beale3.txt") |>
    stringr::str_replace_all(beale_repls()) |>
    # exclude rest of the abbreviations and initialisms
    stringr::str_subset("[:\\.]", TRUE) |>
    exclude_prefixes() |>
    stringr::str_subset("^[\\-']", TRUE) |>
    stringr::str_remove("[^[[:alpha:]]]*$") |> # remove Beale's suffixes
    # exclude remaining words with other unwanted characters or digits
    stringr::str_subset("[&\\(\\)/0-9]", TRUE)
}

# https://github.com/elasticdog/yawl
load_words_yawl <- \() readLines("words_yawl.txt") # was `word.list`


# http://app.aspell.net/create (custom list via tool)
load_words_scowl <- function() {
  readLines("words_scowl.txt") |>
    utils::tail(-44) |>
    exclude_possessives()
}

# https://github.com/marcoagpinto/aoo-mozilla-en-dict
load_words_pinto <- function() {
  readLines("words_pinto.txt") |>
    exclude_possessives() |>
    # exclude elements with unwanted characters or digits
    stringr::str_subset("[\\.0-9]", TRUE)
}

convert_to_dict <- function(vec, alpha_only = TRUE) {
  nms <- vec
  vec <- remove_apos_commas(vec)
  vec <- if (alpha_only) to_alpha_only(vec) else vec
  names(vec) <- nms
  vec[!duplicated(vec)] # instead of unique(), so we keep the names
}


exclude_prefixes <- \(vec) stringr::str_subset(vec, "\\-$", TRUE)
exclude_possessives <- \(vec) stringr::str_subset(vec, "'s$", TRUE)

sort_upper_unique <- \(vec) sort(unique(toupper(vec)))
remove_apos_commas <- \(vec) stringr::str_remove_all(vec, "[',]")
to_alpha_only <- \(vec) stringr::str_remove_all(vec, "[\\-\\s]")


beale_repls <- \() c(
  "aka:" = "aka",
  "dpi:" = "dpi",
  "lbw:" = "lbw",
  "mpg:" = "mpg",
  "mph:" = "mph",
  "rpm:" = "rpm",
  "sae:" = "sae",
  "wpm:" = "wpm",
  "A.D." = "AD",
  "A.N. Other" = "AN Other",
  "B.A." = "BA",
  "B.A.s" = "BAs",
  "B.C." = "BC",
  "B.C.E." = "BCE",
  "d.o.b." = "DOB",
  "John Q. Public" = "John Q Public",
  "M.B.A." = "MBA",
  "M.B.A.s" = "MBAs",
  "N.B." = "NB",
  "Ph.D." = "PhD",
  "Ph.D.s" = "PhDs",
  "Q.E.D." = "QED",
  "R.I.P." = "RIP",
  "R.S.V.P." = "RSVP",
  "St. Bernard&" = "St Bernard",
  "U.S.A." = "USA",
  "U.S.S.R." = "USSR",
  "U.K." = "UK"
)

moby_repls <- \() c(
  "falutin'" = "falutin",
  "A.I." = "AI",
  "A.W.O.L." = "AWOL",
  "B.P." = "BP",
  "A.D." = "AD",
  "B.B.C." = "BBC",
  "B.C." = "BC",
  "B.Sc." = "BSC",
  "C.D." = "CD",
  "D.J." = "DJ",
  "D.O.A." = "DOA",
  "D.S.M." = "DSM",
  "D.S.O." = "DSO",
  "D.V." = "DV",
  "E.T.A." = "ETA",
  "G.B." = "GB",
  "G.P.U." = "GPU",
  "H.Q." = "HQ",
  "H.R.H." = "HRH",
  "i.q." = "IQ",
  "I.R.A." = "IRA",
  "I.W.W." = "IWW",
  "K.B.E." = "KBE",
  "K.C." = "KC",
  "K.C.V.O." = "KCVO",
  "K.O." = "KO",
  "LL.B." = "LLB",
  "LL.D." = "LLD",
  "L.D.S." = "LDS",
  "L.S.D." = "LSD",
  "M.B.A." = "MBA",
  "M.B.E." = "MBE",
  "M.C." = "MC",
  "M.D." = "MD",
  "M.S." = "MS",
  "M.Sc." = "MSC",
  "N.B." = "NB",
  "N.C." = "NC",
  "N.C.O." = "NCO",
  "N.D." = "ND",
  "N.J." = "NJ",
  "N.Y." = "NY",
  "N.Y.C." = "NYC",
  "Q.C." = "QC",
  "Q.E.D." = "QED",
  "R.A.M." = "RAM",
  "R.I." = "RI",
  "R.I.B.A." = "RIBA",
  "R.I.P." = "RIP",
  "R.N." = "RN",
  "R.S.V.P." = "RSVP",
  "S.C." = "SC",
  "S.D." = "SD",
  "U.S.A." = "USA",
  "U.S.S." = "USS",
  "U.K." = "UK",
  "U.V." = "UV",
  "V.C." = "VC",
  "Y.M.C.A." = "YMCA",
  "Y.W.C.A." = "YWCA"
)
