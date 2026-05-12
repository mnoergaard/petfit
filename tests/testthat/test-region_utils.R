test_that("create_tacs_morph_mapping reuses a single session morph across PET sessions", {
  pipeline_dir <- file.path(tempdir(), "petfit_mapping_single_session_morph")
  unlink(pipeline_dir, recursive = TRUE)

  pet_dirs <- file.path(pipeline_dir, "sub-01", c("ses-01", "ses-02", "ses-03"), "pet")
  anat_dir <- file.path(pipeline_dir, "sub-01", "ses-01", "anat")
  purrr::walk(c(pet_dirs, anat_dir), dir.create, recursive = TRUE, showWarnings = FALSE)

  tacs_files <- file.path(
    pet_dirs,
    c(
      "sub-01_ses-01_trc-FDG_desc-preproc_seg-gtm_tacs.tsv",
      "sub-01_ses-02_trc-MK6240_desc-preproc_seg-gtm_tacs.tsv",
      "sub-01_ses-03_trc-FBB_desc-preproc_seg-gtm_tacs.tsv"
    )
  )
  morph_file <- file.path(anat_dir, "sub-01_ses-01_desc-preproc_seg-gtm_morph.tsv")

  file.create(c(tacs_files, morph_file))

  mapping <- create_tacs_morph_mapping(pipeline_dir)

  expect_equal(nrow(mapping), 3)
  expect_setequal(mapping$tacs_path, tacs_files)
  expect_true(all(mapping$morph_path == morph_file))
})

test_that("create_tacs_morph_mapping does not choose an ambiguous cross-session morph", {
  pipeline_dir <- file.path(tempdir(), "petfit_mapping_ambiguous_session_morph")
  unlink(pipeline_dir, recursive = TRUE)

  pet_dir <- file.path(pipeline_dir, "sub-01", "ses-03", "pet")
  anat_dirs <- file.path(pipeline_dir, "sub-01", c("ses-01", "ses-02"), "anat")
  purrr::walk(c(pet_dir, anat_dirs), dir.create, recursive = TRUE, showWarnings = FALSE)

  tacs_file <- file.path(pet_dir, "sub-01_ses-03_trc-FBB_desc-preproc_seg-gtm_tacs.tsv")
  morph_files <- file.path(
    anat_dirs,
    c(
      "sub-01_ses-01_desc-preproc_seg-gtm_morph.tsv",
      "sub-01_ses-02_desc-preproc_seg-gtm_morph.tsv"
    )
  )

  file.create(c(tacs_file, morph_files))

  mapping <- create_tacs_morph_mapping(pipeline_dir)

  expect_equal(nrow(mapping), 0)
})

test_that("summarise_tacs_descriptions handles seg-only TACs without label entity", {
  pipeline_dir <- file.path(tempdir(), "petfit_seg_only_tacs")
  unlink(pipeline_dir, recursive = TRUE)

  pet_dir <- file.path(pipeline_dir, "sub-01", "ses-test", "pet")
  dir.create(pet_dir, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(
    pet_dir,
    "sub-01_ses-test_trc-11CMC1_desc-preproc_seg-hammers_tacs.tsv"
  ))

  descriptions <- summarise_tacs_descriptions(pipeline_dir)

  expect_equal(nrow(descriptions), 1)
  expect_equal(descriptions$description, "seg-hammers_desc-preproc")
})

test_that("summarise_tacs_descriptions handles label-only TACs without seg entity", {
  pipeline_dir <- file.path(tempdir(), "petfit_label_only_tacs")
  unlink(pipeline_dir, recursive = TRUE)

  pet_dir <- file.path(pipeline_dir, "sub-01", "ses-test", "pet")
  dir.create(pet_dir, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(
    pet_dir,
    "sub-01_ses-test_trc-11CMC1_desc-preproc_label-cerebellum_tacs.tsv"
  ))

  descriptions <- summarise_tacs_descriptions(pipeline_dir)

  expect_equal(nrow(descriptions), 1)
  expect_equal(descriptions$description, "label-cerebellum_desc-preproc")
})

test_that("create_tacs_list discovers seg-only TACs with matching morph files", {
  derivatives_dir <- file.path(tempdir(), "petfit_seg_only_derivatives")
  unlink(derivatives_dir, recursive = TRUE)

  pipeline_dir <- file.path(derivatives_dir, "pmod")
  pet_dir <- file.path(pipeline_dir, "sub-11", "ses-test", "pet")
  anat_dir <- file.path(pipeline_dir, "sub-11", "ses-test", "anat")
  purrr::walk(c(pet_dir, anat_dir), dir.create, recursive = TRUE, showWarnings = FALSE)

  tacs_file <- file.path(pet_dir, "sub-11_ses-test_trc-11CMC1_desc-preproc_seg-hammers_tacs.tsv")
  morph_file <- file.path(anat_dir, "sub-11_ses-test_desc-preproc_seg-hammers_morph.tsv")
  file.create(c(tacs_file, morph_file))

  tacs_list <- create_tacs_list(derivatives_dir)

  expect_equal(nrow(tacs_list), 1L)
  expect_equal(tacs_list$tacs_path, tacs_file)
  expect_equal(tacs_list$morph_path, morph_file)
  expect_equal(tacs_list$description, "seg-hammers_desc-preproc")
})
