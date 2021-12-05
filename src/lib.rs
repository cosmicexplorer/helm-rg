/* Copyright 2021 Danny McClanahan */
/* SPDX-License-Identifier: GPL-3.0-only */

use regex;

use std::{
  ffi::{CStr, CString},
  os::raw::{c_int, c_void},
  ptr,
};

pub mod emacs;

use emacs::{bindings::*, wrappers::*};

#[no_mangle]
pub static plugin_is_GPL_compatible: c_int = 0;

fn get_fset_symbol(mut env: Env) -> Value {
  env.intern(&CString::new(b"fset".to_vec()).expect("\"fset\" should be a valid CStr"))
}

/// Bind NAME to FUN.
#[allow(non_snake_case)]
fn bind_function(mut env: Env, name: &CStr, Sfun: Value) -> Value {
  /* Set the function cell of the symbol named NAME to SFUN using
  the 'fset' function. */

  /* Convert the strings to symbols by interning them */
  let Qfset = get_fset_symbol(env);
  let Qsym = env.intern(name).get_emacs_value();

  /* Prepare the arguments array */
  let mut args: [emacs_value; 2] = [Qsym, Sfun.get_emacs_value()];

  /* Make the call (2 == nb of arguments) */
  env.funcall(Qfset, &mut args)
}

fn get_provide_symbol(mut env: Env) -> Value {
  env.intern(&CString::new(b"provide".to_vec()).expect("\"provide\" should be a valid CStr"))
}

/// Provide FEATURE to Emacs.
///
/// call 'provide' with FEATURE converted to a symbol
#[allow(non_snake_case)]
fn provide(mut env: Env, feature: &CStr) {
  let Qfeat = env.intern(feature);
  let Qprovide = get_provide_symbol(env);
  let mut args: [emacs_value; 1] = [Qfeat.get_emacs_value()];
  env.funcall(Qprovide, &mut args);
}

fn get_doc_cstr() -> CString {
  CString::new(b"doc".to_vec()).expect("doc is a valid string")
}

fn get_mymod_cstr() -> CString {
  CString::new(b"mymod".to_vec()).expect("mymod is a valid string")
}

fn get_mymod_test_cstr() -> CString {
  CString::new(b"mymod-test".to_vec()).expect("mymod-test is a valid string")
}

/// New emacs lisp function. All function exposed to Emacs must have this prototype.
#[no_mangle]
pub unsafe extern "C" fn Fmymod_test(
  env: *mut emacs_env,
  _nargs: isize,
  _args: *mut emacs_value,
  _data: *mut c_void,
) -> emacs_value {
  let mut env = Env::new(env);
  env.make_integer(42).get_emacs_value()
}

#[no_mangle]
pub unsafe extern "C" fn emacs_module_init(ert: *mut emacs_runtime) -> c_int {
  let mut ert = Runtime::new(ert);
  /* A module can verify that the Emacs executable which loads the module is compatible with the
   * module, by comparing the size member of the runtime structure with the value compiled into the
   * module: */
  if ert.dynamic_size() < Runtime::static_size() {
    return 1;
  }
  /* If the size of the runtime object passed to the module is smaller than what it expects, it
   * means the module was compiled for an Emacs version newer (later) than the one which attempts to
   * load it, i.e. the module might be incompatible with the Emacs binary. */

  let mut env = ert.get_environment();
  /* In addition, a module can verify the compatibility of the module API with what the module
   * expects. The following sample code assumes it is part of the emacs_module_init function shown
   * above:  */
  if env.dynamic_size() < Env::static_size() {
    return 2;
  }
  /* This calls the get_environment function using the pointer provided in the runtime structure to
   * retrieve a pointer to the API’s environment, a C struct which also has a size field holding the
   * size of the structure in bytes. */

  /* create a lambda (returns an emacs_value) */
  let fun = env.make_function(
    0,               /* min. number of arguments */
    0,               /* max. number of arguments */
    Fmymod_test,     /* actual function pointer */
    &get_doc_cstr(), /* docstring */
    ptr::null_mut(), /* user pointer of your choice (data param in Fmymod_test) */
  );

  /* bind the function to its name */
  bind_function(env, &get_mymod_test_cstr(), fun);

  /* provide the module */
  provide(env, &get_mymod_cstr());

  /* loaded successfully */
  0
}

#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    let result = 2 + 2;
    assert_eq!(result, 4);
  }
}
