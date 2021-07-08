/* Copyright 2021 Danny McClanahan */
/* SPDX-License-Identifier: GPL-3.0-or-later */

//! *See <https://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Modules.html>.

/* Warn for missing docs in general, and hard require crate-level docs. */
#![warn(missing_docs)]
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

mod emacs_module;
use emacs_module::*;

use displaydoc::Display;
use lazy_static::lazy_static;
use num_enum::{IntoPrimitive, TryFromPrimitive};

use std::{
  ffi::{CStr, CString},
  mem,
  os::raw::{c_int, c_void},
  ptr,
};

/// Necessary for the emacs plugin framework. *See [docs].*
///
/// [docs]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Module-Initialization.html
#[no_mangle]
pub static plugin_is_GPL_compatible: c_int = 0;

/// Types of errors that can be returned by the initialization function.
#[repr(i32)]
#[derive(TryFromPrimitive, IntoPrimitive, Debug, Display)]
pub enum HelmRgInitError {
  /// The size of the emacs runtime structure is incompatible with the compiled-in size.
  InvalidEmacsRuntime = 1,
  /// The emacs module API is incompatible with the compiled-in version.
  InvalidEmacsModuleAPI = 2,
}

fn get_env(runtime: &mut emacs_runtime) -> &mut emacs_env {
  unsafe { &mut *(runtime.get_environment.unwrap())(&mut *runtime) }
}

fn make_function(
  env: &mut emacs_env,
  min_arity: usize,
  max_arity: usize,
  module_func: emacs_function,
  docstring: Option<&CStr>,
  data: *mut c_void,
) -> emacs_value {
  unsafe {
    (env.make_function.unwrap())(
      &mut *env,
      min_arity as isize,
      max_arity as isize,
      module_func,
      docstring.map(|s| s.as_ptr()).unwrap_or_else(ptr::null),
      data,
    )
  }
}

fn intern(env: &mut emacs_env, name: &CStr) -> emacs_value {
  unsafe { (env.intern.unwrap())(&mut *env, name.as_ptr()) }
}

fn funcall<const NARGS: usize>(
  env: &mut emacs_env,
  name: &CStr,
  mut args: [emacs_value; NARGS],
) -> emacs_value {
  unsafe {
    (env.funcall.unwrap())(
      &mut *env,
      intern(env, name),
      NARGS as isize,
      args.as_mut_ptr(),
    )
  }
}

fn expose_c_str(s: &str) -> CString {
  let mut bytes: Vec<u8> = s.as_bytes().to_vec();
  bytes.push(b'\0');
  let boxed: Box<CStr> = Box::from(CStr::from_bytes_with_nul(&bytes).unwrap());
  boxed.into_c_string()
}

lazy_static! {
  static ref DEFALIAS_NAME: CString = expose_c_str("defalias");
}

fn declare_function(
  env: &mut emacs_env,
  f: emacs_function,
  name: &CStr,
  docstring: Option<&CStr>,
  min_arity: usize,
  max_arity: usize,
  data: *mut c_void,
) {
  let fun = make_function(env, min_arity, max_arity, f, docstring, data);
  let sym = intern(env, name);
  funcall(env, &DEFALIAS_NAME, [sym, fun]);
}

/// Recommended check in "Compatibility verification" of
/// <https://www.gnu.org/software/emacs/manual/html_node/elisp/Module-Initialization.html>.
fn verify_compat(runtime: &mut emacs_runtime) -> Result<(), HelmRgInitError> {
  if runtime.size < mem::size_of_val(runtime) as isize {
    return Err(HelmRgInitError::InvalidEmacsRuntime);
  }
  let env = get_env(runtime);
  if env.size < mem::size_of_val(&env) as isize {
    return Err(HelmRgInitError::InvalidEmacsModuleAPI);
  }
  Ok(())
}

fn make_string(env: &mut emacs_env, s: &str) -> emacs_value {
  let s_c = expose_c_str(s);
  unsafe { (env.make_string.unwrap())(&mut *env, s_c.as_ptr(), s_c.as_bytes().len() as isize) }
}

/// *TODO: generate docstring from rust docstring!*
#[no_mangle]
pub unsafe extern "C" fn helm_rg_string_match(
  env: *mut emacs_env,
  _nargs: isize,
  _args: *mut emacs_value,
  _data: *mut c_void,
) -> emacs_value {
  make_string(&mut *env, "wow!!!")
}

/// Initialize module. *See [docs].*
///
/// [docs]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Module-Initialization.html
#[no_mangle]
pub unsafe extern "C" fn emacs_module_init(runtime: *mut emacs_runtime) -> c_int {
  if let Err(e) = verify_compat(&mut *runtime) {
    return e.into();
  }
  let env = get_env(&mut *runtime);

  declare_function(
    env,
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
