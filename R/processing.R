#' Get participant-level data
#'
#' @param bvq_data A named list resulting from calling [get_bvq()]
#' @param longitudinal Should longitudinal data be included? If `"all"` (default), all responses (including repeated measures) are included. If `"no"`, participants with more than one responses to the questionnaire (regardless of the version) are excluded. If `"first"`, only the first response of each participant is included. If `"last"`, only the last response of each participant is included. If `"only"`, only responses with repeated measures are included.
#' @param age Numeric vector of length two indicating the minimum and maximum age of participants that will be included in the resulting dataset.
#' @param lp Character vector indicating the language profile (LP) of the participants that will be included in the resulting dataset. In takes `"Monolingual"`, `"Bilingual"`, and/or `"Other"` as values.
#' @param other_threshold Numeric value between 0 and 1 indicating the minimum exposure to a language other than Catalan or Spanish that a participant need to be exposed to to be excluded.
get_participants <- function(bvq_data,
                             longitudinal = "all",
                             age = c(12, 32),
                             lp = c("Monolingual", "Bilingual"),
                             other_threshold = 0.1) {
  participants <- bvq_data$logs |>
    dplyr::filter(
      completed,
      # get only short versions of the questionnaire
      grepl("lockdown|short", version),
      # get only data from complete questionnaire responses
      # rlang::.env makes sure we use the objects provided in the arguments
      # of the function, and not variables in the piped data frame
      lp %in% .env$lp,
      between(age, .env$age[1], .env$age[2]),
      sum(doe_catalan + doe_spanish) > .env$other_threshold,
      # make sure that degrees of exposure are between 0 and 1
      between(doe_spanish, 0, 1),
      between(doe_catalan, 0, 1),
      between(doe_others, 0, 1)
    ) |>
    mutate(time = as.integer(time)) |>
    get_longitudinal(longitudinal = longitudinal) |>
    select(
      child_id, response_id, time,
      time_stamp = date_finished, list = version,
      age, sex, lp, doe_catalan, doe_spanish, edu_parent
    ) |>
    arrange(child_id)

  # export data
  save_files(participants,
    formats = "csv",
    folder = "data"
  )

  return(participants)
}


#' Get CHILDES lexical frequencies
#'
#' @param collection CHILDES corpora from where to fetch transcriptions. Takes "Eng-NA" (North American English by default). See [CHILDES Index to corpora](https://childes.talkbank.org/access/) to see options
#' @param age_range Numeric vector of length two indicating the minimum and maximum age range of interest for which to compute lexical frequencies in the CHILDES corpora. Frequencies will be summarised across this age range using the mean
#' @paran ... Additional arguments passed to [childesr::get_types()]
get_childes_frequencies <- function(collection = "Eng-NA",
                                    age_range = c(10, 36),
                                    ...) {
  suppressMessages({
    roles <- c(
      "Mother",
      "Father",
      "Investigator",
      "Sibling",
      "Sister",
      "Grandmother",
      "Adult",
      "Friend",
      "Brother",
      "Visitor",
      "Relative",
      "Grandfather",
      "Teacher",
      "Student"
    )

    counts <- childesr::get_types(collection = collection, role = roles, ...)

    speaker_ids <- distinct(
      counts,
      collection_id,
      corpus_id,
      transcript_id,
      speaker_id
    )

    speakers <- speaker_ids |>
      left_join(
        childesr::get_speaker_statistics(collection = collection),
        by = c(
          "collection_id",
          "corpus_id",
          "speaker_id",
          "transcript_id"
        )
      ) |>
      select(
        collection_id,
        corpus_id,
        transcript_id,
        speaker_id,
        num_tokens
      )

    childes <- counts |>
      left_join(speakers,
        by = c(
          "collection_id",
          "corpus_id",
          "speaker_id",
          "transcript_id"
        )
      ) |>
      mutate(
        id = as.character(id),
        age_months = target_child_age,
        age_bin = as.integer(floor(age_months / 6) * 6),
        token = tolower(gloss)
      ) |>
      summarise(
        transcript_count = sum(count),
        transcript_num_tokens = sum(num_tokens),
        .by = c(age_bin, token, target_child_id, transcript_id)
      ) |>
      dplyr::filter(between(
        age_bin,
        age_range[1],
        age_range[2]
      )) |>
      summarise(
        freq_count = mean(transcript_count),
        total_count = mean(transcript_num_tokens),
        n_children = length(unique(target_child_id)),
        .by = token
      ) |>
      mutate(
        freq_million = freq_count / total_count * 1e6,
        freq_zipf = log10(freq_million) + 3
      ) |>
      relocate(
        token,
        n_children,
        freq_count,
        freq_million,
        freq_zipf
      )
  })

  return(childes)
}

#' Get item data
#'
#' Levenshtein similarities are computed using the [stringdist::stringsim()] function (see ?[stringdist::`stringdist-package`]).
#'
#' @param bvq_data A named list resulting from calling [get_bvq()]
#' @param childes A data frame with lexical frequencies extracted from CHILDES, as returned by the [get_childes_frequencies()]
#' @param class A character vector indicating the word classes to be included in the resulting dataset. Takes `"Adjective"`, `"Noun"` and/or `"Verb"` as values.
#'
get_items <- function(bvq_data, childes, class = "Noun") {
  classes_available <- c("Noun", "Verb", "Adjective")

  if (!(class %in% classes_available)) {
    cli_abort("class must be one of {classes_available}")
  }

  classes <- class

  # find TEs that have one word-form in each language
  duplicated_te <- bvq_data$pool$te[duplicated(bvq_data$pool$te)]

  pool_tmp <- bvq_data$pool |>
    # drop items with missing observations in these variables
    drop_na(xsampa, wordbank_lemma) |>
    dplyr::filter(
      n_lemmas == 1,
      # exclude items with more than two lemmas
      !is_multiword,
      # exclude multi-word items
      include,
      # exclude problematic items (e.g., multi-word items)
      te %in% duplicated_te,
      # get only translation equivalents with at least one item in each language
      class %in% classes
    ) |>
    add_count(te, name = "n_te") |> # get only items with one translation in each language
    dplyr::filter(n_te == 2) |>
    distinct(language, te, .keep_all = TRUE) |>
    mutate(
      xsampa_flat = flatten_xsampa(xsampa),
      syll = syllabify_xsampa(xsampa),
      n_syll = purrr::map_int(syll, length),
      item = gsub("cat_|spa_", "", item)
    )

  syll_freq <- pool_tmp |>
    left_join(childes, by = c("childes_lemma" = "token")) |>
    tidyr::replace_na(list(freq_million = 1)) |>
    unnest_longer(syll) |>
    summarise(
      freq_syll = sum(freq_million),
      .by = c(language, syll)
    ) |>
    mutate(freq_syll = log10(freq_syll) + 3)

  syllables <- pool_tmp |>
    unnest_longer(syll, indices_to = "position") |>
    left_join(syll_freq, by = c("syll", "language")) |>
    summarise(
      freq_syll_sum = sum(freq_syll),
      .by = c(te, n_syll, language)
    )

  # compute normalised Levenshtein distance.
  # number of edit operations needed to make two strings identical
  # when applied to phonological transcriptions, it provides an approximation of phonological similarity between two word-forms
  lv_similarities <- pool_tmp |>
    pivot_wider(
      id_cols = te,
      names_from = language,
      values_from = xsampa_flat,
      names_repair = janitor::make_clean_names
    ) |>
    # make sure strings are coded as UTF-8 before computing LVs
    mutate(lv = stringdist::stringsim(catalan, spanish)) |>
    distinct(te, lv)

  # merge datasets
  items <- pool_tmp |>
    rename(list = version) |>
    left_join(lv_similarities,
      by = join_by(te)
    ) |> # add LVs
    left_join(childes, by = c("childes_lemma" = "token")) |>
    left_join(syllables, by = join_by(language, te, n_syll)) |>
    drop_na(lv, list, wordbank_lemma, freq_zipf) |>
    mutate(
      n_phon = nchar(xsampa_flat),
      item = gsub("cat_|spa_", "", item),
      ipa = ipa::xsampa(xsampa)
    ) |>
    select(te,
      meaning = wordbank_lemma, language, item, ipa,
      xsampa, lv, n_phon, n_syll, syll, freq = freq_zipf,
      freq_syll = freq_syll_sum, list
    ) |>
    arrange(te)

  # export to data folder
  save_files(items, folder = "data", formats = "csv")

  return(items)
}



#' Create a data frame with the syllable frequency information of the items
#'
get_syllable_data <- function(items) {
  items |>
    select(te, item, lv, n_syll, freq_syll) |>
    drop_na(freq_syll) |>
    mutate(across(c(n_syll, lv, freq_syll, lv),
      \(x) scale(x)[, 1],
      .names = "{.col}_std"
    ))
}

#' Prepare data for analyses
#'
#' @param bvq_data A named list resulting from calling [get_bvq()].
#' @param items A data frame resulting from calling [get_items()].
#' @param participants A data frame resulting from calling [get_participants()].
get_responses <- function(bvq_data, items, participants) {
  # merge all datasets
  responses_tmp <- bvq_data$responses |>
    mutate(
      time = as.integer(time),
      language = ifelse(grepl("cat_", item), "Catalan", "Spanish"),
      item = stringr::str_remove(item, "cat_|spa_")
    ) |>
    # drop missing responses
    # by default datasets are expanded so that every participant has rows for all items,
    # even for those that were not included in their version of the questionnaire
    drop_na(response) |>
    inner_join(distinct(participants, child_id, response_id),
      by = join_by(child_id, response_id)
    ) |>
    select(child_id, response_id, time, language, item, response)

  response_levels <- c("No", "Understands", "Understands and Says")

  responses <- items |>
    select(-list) |>
    inner_join(responses_tmp,
      by = join_by(language, item)
    ) |>
    inner_join(participants,
      by = join_by(child_id, response_id, time)
    ) |>
    mutate(
      # code responses as factor
      response = factor(response,
        levels = c(1, 2, 3),
        labels = response_levels,
        ordered = TRUE
      ),
      # does should have the value of the corresponding language
      doe = ifelse(language == "Catalan", doe_catalan, doe_spanish),
      # standardise numeric predictors
      n_phon_std = scale(n_phon)[, 1],
      freq_std = scale(freq)[, 1],
      lv_std = scale(lv)[, 1],
      age_std = scale(age)[, 1],
      doe_std = scale(doe)[, 1],
      exposure = freq * doe,
      exposure_std = scale(exposure)[, 1]
    ) |>
    # get only relevant variables
    select(
      child_id, response_id, time, age, age_std, te, language, meaning, item, response, lv, lv_std, freq, freq_std, n_phon, n_phon_std, doe, doe_std,
      exposure, exposure_std
    ) |>
    # reorder rows
    arrange(child_id, response_id, te, language)

  # export data
  save_files(responses,
    formats = "csv",
    folder = "data"
  )

  return(responses)
}
