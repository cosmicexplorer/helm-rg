/* Copyright 2021 Danny McClanahan */
/* SPDX-License-Identifier: GPL-3.0-or-later */

use bindgen::builder as bbuilder;
use cbindgen::{Builder as CBuilder, Language};
use walkdir::WalkDir;

use std::env;
use std::path::Path;

fn main() {
  let cbindings_config_path = Path::new("cbindgen.toml");
  mark_for_change_detection(&cbindings_config_path);
  mark_for_change_detection(Path::new("src"));

  let bbindgen_output = Path::new("src/emacs_module.rs");

  let emacs_include_path = env::split_paths(&env::var_os("C_INCLUDE_PATH").unwrap())
    .collect::<Vec<_>>()
    .get(0)
    .expect("expected C_INCLUDE_PATH to be set")
    .clone();
  bbuilder()
    .header(format!(
      "{}",
      emacs_include_path.join("emacs-module.h").display()
    ))
    .allowlist_type("emacs_.*")
    .allowlist_var("EMACS_.*")
    .opaque_type(".*_private")
    .raw_line("#![allow(non_camel_case_types)]")
    .raw_line("#![allow(non_upper_case_globals)]")
    .raw_line("#![allow(non_snake_case)]")
    .raw_line("#![allow(dead_code)]")
    .generate()
    .unwrap()
    .write_to_file(bbindgen_output)
    .unwrap();

  let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
  let cbindgen_output = Path::new("src/dynamic-module.h");
  CBuilder::new()
    .with_crate(crate_dir)
    .with_language(Language::C)
    .with_header(&format!(
      "{}\n{}\n\n{}",
      "/* Copyright 2021 Danny McClanahan */",
      "/* SPDX-License-Identifier: GPL-3.0-or-later */",
      "/* See <https://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Modules.html>. */"
    ))
    .with_include_version(true)
    .with_no_includes()
    .with_line_length(100)
    .generate()
    .expect("Unable to generate bindings")
    .write_to_file(cbindgen_output);
}

fn mark_for_change_detection(path: &Path) {
  // Restrict re-compilation check to just our input files.
  // See: http://doc.crates.io/build-script.html#outputs-of-the-build-script
  if !path.exists() {
    panic!(
      "Cannot mark non-existing path for change detection: {}",
      path.display()
    );
  }
  for file in WalkDir::new(path) {
    println!("cargo:rerun-if-changed={}", file.unwrap().path().display());
  }
}
