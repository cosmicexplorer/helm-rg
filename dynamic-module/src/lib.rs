/* Copyright 2021 Danny McClanahan */
/* SPDX-License-Identifier: GPL-3.0-or-later */

//! *See <https://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Modules.html>.

/* Warn for missing docs in general, and hard require crate-level docs. */
/* #![warn(missing_docs)] */
#![warn(missing_crate_level_docs)]
/* Make all doctests fail if they produce any warnings. */
#![doc(test(attr(deny(warnings))))]
/* Enable all clippy lints except for many of the pedantic ones. It's a shame this needs to be
 * copied and pasted across crates, but there doesn't appear to be a way to include inner attributes
 * from a common source. */
#![deny(
  clippy::all,
  clippy::default_trait_access,
  clippy::expl_impl_clone_on_copy,
  clippy::if_not_else,
  clippy::needless_continue,
  clippy::unseparated_literal_suffix,
  /* TODO: Falsely triggers for async/await:
   *   see https://github.com/rust-lang/rust-clippy/issues/5360 */
  /* clippy::used_underscore_binding */
)]
/* It is often more clear to show that nothing is being moved. */
#![allow(clippy::match_ref_pats)]
/* Subjective style. */
#![allow(
  clippy::len_without_is_empty,
  clippy::redundant_field_names,
  clippy::too_many_arguments
)]
/* Default isn't as big a deal as people seem to think it is. */
#![allow(clippy::new_without_default, clippy::new_ret_no_self)]
/* Arc<Mutex> can be more clear than needing to grok Orderings: */
#![allow(clippy::mutex_atomic)]
/* We only use unsafe pointer dereferences in our no_mangle exposed API, but it is nicer to list
 * just the one minor call as unsafe, than to mark the whole function as unsafe which may hide
 * other unsafeness. */
#![allow(clippy::not_unsafe_ptr_arg_deref)]
#![allow(unsafe_code)]

use emacs_module::{c_types as emacs, expose_c_str, Environment, Runtime};

use std::{
  os::raw::{c_int, c_void},
  ptr,
};

/// *TODO: generate docstring from rust docstring!*
#[no_mangle]
pub unsafe extern "C" fn helm_rg_string_match(
  env: *mut emacs::Env,
  _nargs: isize,
  _args: *mut emacs::Value,
  _data: *mut c_void,
) -> emacs::Value {
  let mut env = Environment::from_ptr(env).unwrap();
  env.make_string(expose_c_str("wow!!!").as_c_str())
}

/// Initialize module. *See [docs].*
///
/// [docs]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Module-Initialization.html
#[no_mangle]
pub unsafe extern "C" fn emacs_module_init(runtime: *mut emacs::Runtime) -> c_int {
  let mut env = match Runtime::from_ptr(runtime).and_then(|mut r| r.get_environment()) {
    Ok(env) => env,
    Err(e) => {
      return e.into();
    }
  };

  env.declare_function(
    Some(helm_rg_string_match),
    expose_c_str("helm-rg-string-match").as_c_str(),
    Some(expose_c_str("See documentation for `string-match'.").as_c_str()),
    2,
    3,
    ptr::null_mut(),
  );

  0
}

#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }
}
