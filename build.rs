/* Copyright 2021 Danny McClanahan */
/* SPDX-License-Identifier: AGPL-3.0-only */

use bindgen;

use std::{default::Default, env};

fn main() {
  /* println!("cargo:rustc-link-lib=emacs") */
  println!("cargo:rerun-if-changed=wrapper.h");

  let bindings = bindgen::Builder::default()
    .header("wrapper.h")
    .parse_callbacks(Box::new(bindgen::CargoCallbacks))
    .rustfmt_bindings(true)
    .generate()
    .expect("unable to generate bindings for some reason???");

  /* let out_path = PathBuf::from(env::var("OUT_DIR").unwrap()); */
  let out_path = env::current_dir().unwrap();
  bindings
    .write_to_file(out_path.join("src/bindings.rs"))
    .expect("couldn't write bindings!");
}
