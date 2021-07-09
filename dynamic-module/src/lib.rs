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

use emacs_module::{
  c_types as emacs, expose_c_str, Environment, LispInteger, LispString, LispSymbol, Runtime,
  StandardProperty, Value, ViaValue,
};

use regex::Regex;

use std::{
  os::raw::{c_int, c_void},
  ptr, slice,
};

fn helm_rg_string_match_helper(
  regexp: Regex,
  string: String,
  mut start: usize,
) -> Vec<(usize, usize)> {
  let mut locs = regexp.capture_locations();

  if let Some(m) = regexp.captures_read_at(&mut locs, &string, start) {
    let mut cur: Vec<(usize, usize)> = Vec::new();
    for i in 0..locs.len() {
      cur.push(locs.get(i).unwrap());
    }
    cur
  } else {
    Vec::new()
  }
}

/// *TODO: generate docstring from rust docstring!*
#[no_mangle]
pub unsafe extern "C" fn helm_rg_string_match(
  env: *mut emacs::Env,
  nargs: isize,
  args: *mut emacs::Value,
  data: *mut c_void,
) -> emacs::Value {
  let mut env = Environment::from_ptr(env).unwrap();
  let args: &mut [emacs::Value] = slice::from_raw_parts_mut(args, nargs as usize);
  assert!(args.len() >= 2 && args.len() <= 3);
  assert!(data.is_null());

  let regexp: String = LispString::extract_value(Value::from_ptr(args[0]), &mut env).into();
  let string: String = LispString::extract_value(Value::from_ptr(args[1]), &mut env).into();
  let start: usize = match args
    .get(2)
    .map(|start| LispInteger::extract_value(Value::from_ptr(*start), &mut env))
    .unwrap_or(LispInteger(0))
  {
    LispInteger(x) if x < 0 => {
      let fmt_str =
        LispString::from("invalid negative start offset: %d".to_string()).make_value(&mut env);
      let x = LispInteger(x).make_value(&mut env);
      return env.error([fmt_str.into(), x.into()]);
    }
    LispInteger(x) => x as usize,
  };

  let regexp = match Regex::new(&regexp) {
    Ok(r) => r,
    Err(e) => {
      let signal_sym = LispSymbol(LispString(expose_c_str("helm-rg-native-error")));
      let reason_str =
        LispString::from("failed to compile rust regexp".to_string()).make_value(&mut env);
      let err_str = LispString::from(format!("{:?}", e)).make_value(&mut env);
      return env.signal(signal_sym, [reason_str.into(), err_str.into()]);
    }
  };

  let captured = helm_rg_string_match_helper(regexp, string, start);
  if captured.is_empty() {
    return env.nil();
  }

  let (leftmost_start, _): (usize, usize) = captured[0];
  env.set_match_data(&captured);
  env.make_integer(leftmost_start as emacs::intmax_t)
}

fn helm_rg_string_match_p_helper(regexp: Regex, string: String, start: usize) -> Option<usize> {
  regexp.find_at(&string, start).map(|m| m.start())
}

/// *TODO: generate docstring from rust docstring!*
#[no_mangle]
pub unsafe extern "C" fn helm_rg_string_match_p(
  env: *mut emacs::Env,
  nargs: isize,
  args: *mut emacs::Value,
  data: *mut c_void,
) -> emacs::Value {
  let mut env = Environment::from_ptr(env).unwrap();
  let args: &mut [emacs::Value] = slice::from_raw_parts_mut(args, nargs as usize);
  assert!(args.len() >= 2 && args.len() <= 3);
  assert!(data.is_null());

  let regexp: String = LispString::extract_value(Value::from_ptr(args[0]), &mut env).into();
  let string: String = LispString::extract_value(Value::from_ptr(args[1]), &mut env).into();
  let start: usize = match args
    .get(2)
    .map(|start| LispInteger::extract_value(Value::from_ptr(*start), &mut env))
    .unwrap_or(LispInteger(0))
  {
    LispInteger(x) if x < 0 => {
      let fmt_str =
        LispString::from("invalid negative start offset: %d".to_string()).make_value(&mut env);
      let x = LispInteger(x).make_value(&mut env);
      return env.error([fmt_str.into(), x.into()]);
    }
    LispInteger(x) => x as usize,
  };

  let regexp = match Regex::new(&regexp) {
    Ok(r) => r,
    Err(e) => {
      let signal_sym = LispSymbol(LispString(expose_c_str("helm-rg-native-error")));
      let reason_str =
        LispString::from("failed to compile rust regexp".to_string()).make_value(&mut env);
      let err_str = LispString::from(format!("{:?}", e)).make_value(&mut env);
      return env.signal(signal_sym, [reason_str.into(), err_str.into()]);
    }
  };

  if let Some(found_start) = helm_rg_string_match_p_helper(regexp, string, start) {
    env.make_integer(found_start as emacs::intmax_t)
  } else {
    env.nil()
  }
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

  let error_symbol = LispSymbol(LispString(expose_c_str("helm-rg-native-error")));
  let error_message = LispString(expose_c_str("error occured in native module"));
  env.declare_signal(error_symbol, error_message, None);

  env.declare_function(
    Some(helm_rg_string_match),
    expose_c_str("helm-rg-string-match").as_c_str(),
    Some(
      expose_c_str(&format!(
        "{}\n\n{}",
        "See documentation for `string-match'.", r"\(fn (REGEXP STRING &optional START))"
      ))
      .as_c_str(),
    ),
    2,
    3,
    ptr::null_mut(),
    &[],
  );

  let t = env.t();
  env.declare_function(
    Some(helm_rg_string_match_p),
    expose_c_str("helm-rg-string-match-p").as_c_str(),
    Some(
      expose_c_str(&format!(
        "{}\n\n{}",
        "See documentation for `string-match-p'.", r"\(fn (REGEXP STRING &optional START))",
      ))
      .as_c_str(),
    ),
    2,
    3,
    ptr::null_mut(),
    /* &[
      (StandardProperty::Pure, t),
      (StandardProperty::SideEffectFree, t),
    ], */
    &[],
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
