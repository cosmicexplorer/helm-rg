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
use parking_lot::RwLock;

use std::{
  ffi::{CStr, CString},
  mem::{self, ManuallyDrop},
  ops::{Deref, DerefMut},
  os::raw::{c_char, c_int, c_void},
  ptr,
  sync::Arc,
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

pub struct Runtime(ManuallyDrop<Box<c_types::Runtime>>);

impl Runtime {
  /// Recommended check in "Compatibility verification" of
  /// <https://www.gnu.org/software/emacs/manual/html_node/elisp/Module-Initialization.html>.
  pub unsafe fn from_ptr(runtime: *mut c_types::Runtime) -> Result<Self, HelmRgInitError> {
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
  static ref PUT_NAME: CString = expose_c_str("defalias");
  static ref NIL_NAME: CString = expose_c_str("nil");
  static ref T_NAME: CString = expose_c_str("t");
  static ref FMT_STR: CString = expose_c_str("%S");
  static ref MESSAGE_NAME: CString = expose_c_str("message");
  static ref FORMAT_NAME: CString = expose_c_str("format");
  static ref ERROR_NAME: CString = expose_c_str("error");
  static ref DEFINE_ERROR_NAME: CString = expose_c_str("define-error");
  static ref SIGNAL_NAME: CString = expose_c_str("signal");
  static ref SYMBOL_NAME_NAME: CString = expose_c_str("symbol-name");
  static ref LIST_NAME: CString = expose_c_str("list");
  static ref SET_MATCH_DATA_NAME: CString = expose_c_str("set-match-data");
}

#[derive(Debug, Display)]
pub enum StandardProperty {
  /// Calls with constant arguments can be evaluated at compile time.
  Pure,
  /// The byte compiler may ignore a call whose value is unused.
  /* If the value is ‘error-free’, the byte compiler may even delete such unused calls. */
  SideEffectFree,
}

lazy_static! {
  static ref PURE_STR: CString = expose_c_str("pure");
  static ref SIDE_EFFECT_FREE_STR: CString = expose_c_str("side-effect-free");
}

impl StandardProperty {
  pub fn as_name(&self) -> &CStr {
    match self {
      Self::Pure => PURE_STR.as_c_str(),
      Self::SideEffectFree => SIDE_EFFECT_FREE_STR.as_c_str(),
    }
  }
}

pub struct Environment(ManuallyDrop<Box<c_types::Env>>);

impl Environment {
  /// Recommended check in "Compatibility verification" of
  /// <https://www.gnu.org/software/emacs/manual/html_node/elisp/Module-Initialization.html>.
  pub unsafe fn from_ptr(env: *mut c_types::Env) -> Result<Self, HelmRgInitError> {
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
    module_func: c_types::Function,
    docstring: Option<&CStr>,
    data: *mut c_void,
  ) -> c_types::Value {
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

  pub fn intern(&mut self, name: &CStr) -> c_types::Value {
    let f = self.0.intern.unwrap();
    unsafe { f(&mut **self.0, name.as_ptr()) }
  }

  pub fn funcall<const NARGS: usize>(
    &mut self,
    name: &CStr,
    mut args: [c_types::Value; NARGS],
  ) -> c_types::Value {
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

  pub fn apply(&mut self, name: &CStr, args: &mut [c_types::Value]) -> c_types::Value {
    let f = self.0.funcall.unwrap();
    unsafe {
      f(
        &mut **self.0,
        self.intern(name),
        args.len() as isize,
        args.as_mut_ptr(),
      )
    }
  }

  pub fn declare_function(
    &mut self,
    f: c_types::Function,
    name: &CStr,
    docstring: Option<&CStr>,
    min_arity: usize,
    max_arity: usize,
    data: *mut c_void,
    prop_map: &[(StandardProperty, c_types::Value)],
  ) {
    let fun = self.make_function(min_arity, max_arity, f, docstring, data);
    let sym = self.intern(name);
    self.funcall(&DEFALIAS_NAME, [sym, fun]);
    for (prop, value) in prop_map.iter() {
      let prop_name = self.make_string(prop.as_name());
      self.funcall(&PUT_NAME, [sym, prop_name, *value]);
    }
  }

  pub fn declare_signal(&mut self, name: LispSymbol, message: LispString, parent: Option<Value>) {
    let name = name.make_value(self);
    let message = message.make_value(self);

    if let Some(value) = parent {
      self.funcall(
        &DEFINE_ERROR_NAME,
        [name.into(), message.into(), value.into()],
      );
    } else {
      self.funcall(&DEFINE_ERROR_NAME, [name.into(), message.into()]);
    }
  }

  pub fn list<const NDATA: usize>(&mut self, data: [c_types::Value; NDATA]) -> c_types::Value {
    self.funcall(&LIST_NAME, data)
  }

  pub fn list_many(&mut self, data: &mut [c_types::Value]) -> c_types::Value {
    self.apply(&LIST_NAME, data)
  }

  pub fn signal<const NDATA: usize>(
    &mut self,
    error_symbol: LispSymbol,
    data: [c_types::Value; NDATA],
  ) -> c_types::Value {
    let error_symbol: Value = error_symbol.make_value(self);
    let data: Value = unsafe { Value::from_ptr(self.list(data)) };
    self.funcall(&SIGNAL_NAME, [error_symbol.into(), data.into()])
  }

  pub fn make_string(&mut self, s: &CStr) -> c_types::Value {
    let f = self.0.make_string.unwrap();
    unsafe { f(&mut **self.0, s.as_ptr(), s.to_bytes().len() as isize) }
  }

  pub fn symbol_name(&mut self, arg: c_types::Value) -> c_types::Value {
    self.funcall(&SYMBOL_NAME_NAME, [arg])
  }

  pub fn extract_string(&mut self, arg: c_types::Value) -> String {
    let f = self.0.copy_string_contents.unwrap();
    /* (1) Extract the size of the string to be allocated. */
    let mut len: isize = 0;
    unsafe {
      assert!(f(&mut **self.0, arg, ptr::null_mut(), &mut len));
    }
    /* (2) Copy the string contents into a vector. */
    let owned = CString::new(Vec::<u8>::with_capacity(len as usize)).unwrap();
    let char_ptr = owned.into_raw();
    unsafe {
      assert!(f(&mut **self.0, arg, char_ptr, &mut len));
    }
    let owned = unsafe { CString::from_raw(char_ptr) };
    /* (3) Decode the UTF-8 bytes into an owned string. */
    owned.into_string().unwrap()
  }

  pub fn make_integer(&mut self, n: intmax_t) -> c_types::Value {
    let f = self.0.make_integer.unwrap();
    unsafe { f(&mut **self.0, n) }
  }

  pub fn extract_integer(&mut self, arg: c_types::Value) -> intmax_t {
    let f = self.0.extract_integer.unwrap();
    unsafe { f(&mut **self.0, arg) }
  }

  pub fn nil(&mut self) -> c_types::Value {
    self.intern(&NIL_NAME)
  }

  pub fn t(&mut self) -> c_types::Value {
    self.intern(&T_NAME)
  }

  pub fn eq(&mut self, a: emacs_value, b: emacs_value) -> bool {
    let f = self.0.eq.unwrap();
    unsafe { f(&mut **self.0, a, b) }
  }

  pub fn is_not_nil(&mut self, arg: emacs_value) -> bool {
    let f = self.0.is_not_nil.unwrap();
    unsafe { f(&mut **self.0, arg) }
  }

  pub fn fmt_sexp(&mut self) -> c_types::Value {
    self.make_string(FMT_STR.as_c_str())
  }

  pub fn message_sexp(&mut self, arg: emacs_value) -> String {
    let fmt = self.fmt_sexp();
    let result = self.funcall(MESSAGE_NAME.as_c_str(), [fmt, arg]);
    self.extract_string(result)
  }

  pub fn format_sexp(&mut self, arg: emacs_value) -> String {
    let fmt = self.fmt_sexp();
    let result = self.funcall(FORMAT_NAME.as_c_str(), [fmt, arg]);
    self.extract_string(result)
  }

  pub fn error<const NARGS: usize>(&mut self, mut args: [c_types::Value; NARGS]) -> c_types::Value {
    self.apply(ERROR_NAME.as_c_str(), args.as_mut())
  }

  pub fn set_match_data(&mut self, captures: &[(usize, usize)]) {
    let mut all_matches: Vec<c_types::Value> = Vec::new();
    for (start, end) in captures.iter() {
      all_matches.push(
        LispInteger(*start as c_types::intmax_t)
          .make_value(self)
          .into(),
      );
      all_matches.push(
        LispInteger(*end as c_types::intmax_t)
          .make_value(self)
          .into(),
      );
    }
    let data = self.list_many(all_matches.as_mut());
    self.funcall(&SET_MATCH_DATA_NAME, [data]);
  }
}

pub struct Value(ManuallyDrop<Box<c_types::ValueStruct>>);

impl Value {
  pub unsafe fn from_ptr(value: c_types::Value) -> Self {
    Self(ManuallyDrop::new(Box::from_raw(value)))
  }
}

impl From<Value> for c_types::Value {
  fn from(value: Value) -> c_types::Value {
    Box::into_raw(ManuallyDrop::into_inner(value.0))
  }
}

pub trait ViaValue {
  fn make_value(self, env: &mut Environment) -> Value;
  fn extract_value(value: Value, env: &mut Environment) -> Self;
}

#[derive(Copy, Clone, Debug)]
pub enum LispBoolean {
  Nil,
  T,
}

impl From<bool> for LispBoolean {
  fn from(value: bool) -> Self {
    if value {
      Self::T
    } else {
      Self::Nil
    }
  }
}

impl From<LispBoolean> for bool {
  fn from(value: LispBoolean) -> Self {
    match value {
      LispBoolean::T => true,
      LispBoolean::Nil => false,
    }
  }
}

impl ViaValue for LispBoolean {
  fn make_value(self, env: &mut Environment) -> Value {
    match self {
      Self::Nil => unsafe { Value::from_ptr(env.nil()) },
      Self::T => unsafe { Value::from_ptr(env.t()) },
    }
  }
  fn extract_value(value: Value, env: &mut Environment) -> Self {
    let t = env.t();
    let nil = env.nil();
    let value: c_types::Value = value.into();
    if env.eq(value, t) {
      Self::T
    } else if env.eq(value, nil) {
      Self::Nil
    } else {
      unreachable!("value {} was not bool", env.format_sexp(value))
    }
  }
}

#[derive(Copy, Clone, Debug)]
pub struct LispInteger(pub c_types::intmax_t);

impl ViaValue for LispInteger {
  fn make_value(self, env: &mut Environment) -> Value {
    unsafe { Value::from_ptr(env.make_integer(self.0)) }
  }
  fn extract_value(value: Value, env: &mut Environment) -> Self {
    Self(env.extract_integer(value.into()))
  }
}

#[derive(Clone, Debug)]
pub struct LispString(pub CString);

impl From<LispString> for CString {
  fn from(s: LispString) -> CString {
    s.0
  }
}

impl From<CString> for LispString {
  fn from(s: CString) -> LispString {
    Self(s)
  }
}

impl From<LispString> for String {
  fn from(s: LispString) -> String {
    s.0.into_string().unwrap()
  }
}

impl From<String> for LispString {
  fn from(s: String) -> LispString {
    Self(CString::new(s).unwrap())
  }
}

impl ViaValue for LispString {
  fn make_value(self, env: &mut Environment) -> Value {
    unsafe { Value::from_ptr(env.make_string(self.0.as_c_str())) }
  }
  fn extract_value(value: Value, env: &mut Environment) -> Self {
    Self(CString::new(env.extract_string(value.into())).unwrap())
  }
}

#[derive(Clone, Debug)]
pub struct LispSymbol(pub LispString);

impl ViaValue for LispSymbol {
  fn make_value(self, env: &mut Environment) -> Value {
    unsafe { Value::from_ptr(env.intern(self.0 .0.as_c_str())) }
  }
  fn extract_value(value: Value, env: &mut Environment) -> Self {
    let symbol_name: Value = unsafe { Value::from_ptr(env.symbol_name(value.into())) };
    let lisp_string = LispString::extract_value(symbol_name, env);
    Self(lisp_string)
  }
}

pub mod c_types {
  pub type Env = super::emacs_env;
  pub type Value = super::emacs_value;
  pub type ValueStruct = super::emacs_value_tag;
  pub type Runtime = super::emacs_runtime;
  pub type Function = super::emacs_function;

  #[allow(non_camel_case_types)]
  pub type intmax_t = super::intmax_t;
}
