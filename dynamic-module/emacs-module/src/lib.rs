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

mod emacs_module;
use emacs_module::*;

use displaydoc::Display;
use lazy_static::lazy_static;
use num_enum::{IntoPrimitive, TryFromPrimitive};

use std::{
  ffi::{CStr, CString},
  mem::{self, ManuallyDrop},
  os::raw::{c_int, c_void},
  ptr,
};

/// Necessary for the emacs plugin framework. *See [docs].*
///
/// [docs]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Module-Initialization.html
#[no_mangle]
pub static mut plugin_is_GPL_compatible: c_int = 0;

/// Types of errors that can be returned by the initialization function.
#[repr(i32)]
#[derive(TryFromPrimitive, IntoPrimitive, Debug, Display)]
pub enum HelmRgInitError {
  /// The size of the emacs runtime structure is incompatible with the compiled-in size.
  InvalidEmacsRuntime = 1,
  /// The emacs module API is incompatible with the compiled-in version.
  InvalidEmacsModuleAPI = 2,
}

pub struct Runtime(ManuallyDrop<Box<emacs_runtime>>);

impl Runtime {
  /// Recommended check in "Compatibility verification" of
  /// <https://www.gnu.org/software/emacs/manual/html_node/elisp/Module-Initialization.html>.
  pub unsafe fn from_ptr(runtime: *mut emacs_runtime) -> Result<Self, HelmRgInitError> {
    let boxed = Box::from_raw(runtime);
    if boxed.size < mem::size_of_val(&*runtime) as isize {
      return Err(HelmRgInitError::InvalidEmacsRuntime);
    }
    Ok(Self(ManuallyDrop::new(boxed)))
  }

  pub fn get_environment(&mut self) -> Result<Environment, HelmRgInitError> {
    let f = self.0.get_environment.unwrap();
    unsafe { Environment::from_ptr(f(&mut **self.0)) }
  }
}

pub fn expose_c_str(s: &str) -> CString {
  let mut bytes: Vec<u8> = s.as_bytes().to_vec();
  bytes.push(b'\0');
  let boxed: Box<CStr> = Box::from(CStr::from_bytes_with_nul(&bytes).unwrap());
  boxed.into_c_string()
}

lazy_static! {
  static ref DEFALIAS_NAME: CString = expose_c_str("defalias");
}

pub struct Environment(ManuallyDrop<Box<emacs_env>>);

impl Environment {
  /// Recommended check in "Compatibility verification" of
  /// <https://www.gnu.org/software/emacs/manual/html_node/elisp/Module-Initialization.html>.
  pub unsafe fn from_ptr(env: *mut emacs_env) -> Result<Self, HelmRgInitError> {
    let boxed = Box::from_raw(env);
    if boxed.size < mem::size_of_val(&*env) as isize {
      return Err(HelmRgInitError::InvalidEmacsModuleAPI);
    }
    Ok(Self(ManuallyDrop::new(boxed)))
  }

  pub fn make_function(
    &mut self,
    min_arity: usize,
    max_arity: usize,
    module_func: emacs_function,
    docstring: Option<&CStr>,
    data: *mut c_void,
  ) -> emacs_value {
    let f = self.0.make_function.unwrap();
    unsafe {
      f(
        &mut **self.0,
        min_arity as isize,
        max_arity as isize,
        module_func,
        docstring.map(|s| s.as_ptr()).unwrap_or_else(ptr::null),
        data,
      )
    }
  }

  pub fn intern(&mut self, name: &CStr) -> emacs_value {
    let f = self.0.intern.unwrap();
    unsafe { f(&mut **self.0, name.as_ptr()) }
  }

  pub fn funcall<const NARGS: usize>(
    &mut self,
    name: &CStr,
    mut args: [emacs_value; NARGS],
  ) -> emacs_value {
    let f = self.0.funcall.unwrap();
    unsafe {
      f(
        &mut **self.0,
        self.intern(name),
        NARGS as isize,
        args.as_mut_ptr(),
      )
    }
  }

  pub fn declare_function(
    &mut self,
    f: emacs_function,
    name: &CStr,
    docstring: Option<&CStr>,
    min_arity: usize,
    max_arity: usize,
    data: *mut c_void,
  ) {
    let fun = self.make_function(min_arity, max_arity, f, docstring, data);
    let sym = self.intern(name);
    self.funcall(&DEFALIAS_NAME, [sym, fun]);
  }

  pub fn make_string(&mut self, s: &CStr) -> emacs_value {
    let f = self.0.make_string.unwrap();
    unsafe { f(&mut **self.0, s.as_ptr(), s.to_bytes().len() as isize) }
  }
}

pub mod c_types {
  pub type Env = super::emacs_env;
  pub type Value = super::emacs_value;
  pub type Runtime = super::emacs_runtime;
}
