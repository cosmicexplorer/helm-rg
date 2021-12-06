/* Copyright 2021 Danny McClanahan */
/* SPDX-License-Identifier: GPL-3.0-only */

#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(warnings)]

pub(crate) mod bindings {
  include!("bindings.rs");
}

macro_rules! call_interned_function {
    [$env_handle:expr, $interned_name:expr, $args:expr] => {
      $env_handle.funcall(
        $env_handle.get_ascii_symbol_value($interned_name),
        $args
      )
    }
  }

pub mod wrappers {
  use super::bindings::*;

  use ascii::AsciiString;

  use std::{
    convert::TryInto,
    ffi::{CStr, CString},
    ops::{Deref, DerefMut},
    os::raw::{c_char, c_void},
  };

  use std::sync::Arc;

  use parking_lot::RwLock;

  pub type EnvHandleRefType = Arc<RwLock<Env>>;

  #[derive(Clone)]
  pub struct EnvHandle {
    inner: EnvHandleRefType,
  }

  impl EnvWrapper for EnvHandle {
    fn get_env(&self) -> Self {
      self.clone()
    }
  }

  impl EnvHandle {
    pub fn new(inner: Env) -> Self {
      Self {
        inner: Arc::new(RwLock::new(inner)),
      }
    }

    pub fn get_ref(&self) -> EnvHandleRefType {
      self.inner.clone()
    }

    pub fn copy_string_contents(&self, value: Value) -> String {
      let env = self.get_ref();
      let length_function = env.write().get_ascii_symbol_value("length");

      let length_value = env
        .write()
        .funcall(length_function, &mut [unsafe { value.get_emacs_value() }]);
      let size: usize = env
        .write()
        .extract_integer(length_value)
        .try_into()
        .expect("expected buffer length to be convertible to usize");

      let mut buffer: Vec<c_char> = Vec::with_capacity(size);
      assert!(env
        .write()
        .copy_string_contents_function(value, &mut buffer, size));

      unsafe { CStr::from_ptr(buffer.as_mut_ptr()) }
        .to_string_lossy()
        .to_string()
    }

    /// Return a lisp symbol from the Unicode string `symbol_name`.
    pub fn intern_unicode(&self, symbol_name: &str) -> Value {
      let env = self.get_ref();
      let intern_function = env.write().get_ascii_symbol_value("intern");
      let symbol_name = env.write().make_string(symbol_name);
      let nil = unsafe { env.write().get_ascii_symbol_value("nil").get_emacs_value() };
      env.clone().write().funcall(intern_function, unsafe {
        &mut [symbol_name.get_emacs_value(), nil]
      })
    }

    /// Set the function cell of the symbol named `name` to `Sfun` using the `'fset'` function.
    #[allow(non_snake_case)]
    pub fn bind_function(&mut self, name: &str, Sfun: Value) -> Value {
      let env = self.get_ref();
      /* Convert the strings to symbols by interning them */
      let Qfset = env.write().get_ascii_symbol_value("fset");
      let Qsym = unsafe { self.intern_unicode(name).get_emacs_value() };

      /* Make the call (2 == nb of arguments) */
      let Sfun = unsafe { Sfun.get_emacs_value() };
      env.clone().write().funcall(Qfset, &mut [Qsym, Sfun])
    }

    /// call 'provide' with FEATURE converted to a symbol
    #[allow(non_snake_case)]
    pub fn provide(&mut self, feature: &str) -> Value {
      let env = self.get_ref();
      let Qfeat = self.intern_unicode(feature);
      let Qprovide = env.write().get_ascii_symbol_value("provide");
      let Qfeat = unsafe { Qfeat.get_emacs_value() };
      env.clone().write().funcall(Qprovide, &mut [Qfeat])
    }
  }

  macro_rules! extract_named_function {
    [$handle:expr, $func_name:ident] => {
      unsafe {
        (*$handle).$func_name.expect(&format!(
          "{:?} should be set",
          stringify!($func_name),
        ))
      }
    }
  }

  #[must_use]
  pub enum Compatibility {
    TooOld,
    JustFine,
  }

  pub type UserFunction = unsafe extern "C" fn(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    data: *mut c_void,
  ) -> emacs_value;

  pub trait StaticDynamicSizeCheckable {
    fn dynamic_size(&self) -> usize;
    fn static_size() -> usize;
    fn check_compatibility(&self) -> Compatibility {
      if self.dynamic_size() < Self::static_size() {
        Compatibility::TooOld
      } else {
        Compatibility::JustFine
      }
    }
  }

  pub struct Runtime {
    rt: *mut emacs_runtime,
  }

  impl Runtime {
    pub fn new(rt: *mut emacs_runtime) -> Self {
      Self { rt }
    }

    pub fn get_environment(&mut self) -> Env {
      Env::new(unsafe { extract_named_function![self.rt, get_environment](self.rt) })
    }
  }

  impl StaticDynamicSizeCheckable for Runtime {
    fn dynamic_size(&self) -> usize {
      unsafe {
        (*self.rt)
          .size
          .try_into()
          .expect("isize should convert to usize for Runtime!")
      }
    }

    fn static_size() -> usize {
      std::mem::size_of::<emacs_runtime>()
    }
  }

  pub trait EmacsValue {
    fn as_emacs_value(&self) -> Value;
    fn from_emacs_value(env: EnvHandle, value: Value) -> Self;
  }

  #[must_use]
  #[derive(Clone)]
  pub struct Value {
    val: Arc<RwLock<emacs_value>>,
  }

  impl Value {
    pub fn new(val: emacs_value) -> Self {
      Self {
        val: Arc::new(RwLock::new(val)),
      }
    }
  }

  impl Value {
    pub unsafe fn get_emacs_value(&self) -> emacs_value {
      self.val.write().clone()
    }
  }

  impl EmacsValue for Value {
    fn as_emacs_value(&self) -> Value {
      self.clone()
    }
    fn from_emacs_value(_env: EnvHandle, value: Value) -> Self {
      value
    }
  }

  pub struct Env {
    env: *mut emacs_env,
  }

  pub trait EmacsEnvironment {
    fn get_ascii_symbol_value(&mut self, s: &str) -> Value;

    /// Return a lisp integer.
    fn make_integer(&mut self, value: intmax_t) -> Value;

    fn extract_integer(&mut self, value: Value) -> intmax_t;

    fn make_string(&mut self, s: &str) -> Value;

    /// Generate a lisp function which can be bound to a name with [Self::bind_function].
    fn make_function(
      &mut self,
      min_arity: isize,
      max_arity: isize,
      function: UserFunction,
      documentation: &str,
      data: *mut c_void,
    ) -> Value;

    /// Return a lisp symbol from the [AsciiString] `symbol_name`.
    fn intern_only_ascii(&mut self, symbol_name: &AsciiString) -> Value;

    /// Execute the lisp function `function`.
    fn funcall<const N: usize>(&mut self, function: Value, args: &mut [emacs_value; N]) -> Value;

    fn eq(&mut self, val1: Value, val2: Value) -> bool;

    fn is_not_nil(&mut self, val: Value) -> bool;
  }

  impl Env {
    pub fn new(env: *mut emacs_env) -> Self {
      Self { env }
    }

    fn ascii_string_or_panic(s: &str) -> AsciiString {
      AsciiString::from_ascii(s).expect("string was not ASCII!")
    }

    pub(in crate::emacs) fn copy_string_contents_function(
      &mut self,
      value: Value,
      buffer: &mut Vec<c_char>,
      size_initial: usize,
    ) -> bool {
      let mut size_inout: isize = size_initial
        .try_into()
        .expect("expected usize to be convertible to isize");
      let ret = unsafe {
        extract_named_function![self.env, copy_string_contents](
          self.env,
          value.get_emacs_value(),
          buffer.as_mut_ptr(),
          &mut size_inout,
        )
      };
      assert_eq!(size_initial, size_inout.try_into().unwrap());
      ret
    }
  }

  impl StaticDynamicSizeCheckable for Env {
    fn dynamic_size(&self) -> usize {
      unsafe {
        (*self.env)
          .size
          .try_into()
          .expect("isize should convert to usize for Env!")
      }
    }

    fn static_size() -> usize {
      std::mem::size_of::<emacs_env>()
    }
  }

  impl EmacsEnvironment for Env {
    fn get_ascii_symbol_value(&mut self, s: &str) -> Value {
      self.intern_only_ascii(&Self::ascii_string_or_panic(s))
    }

    fn make_integer(&mut self, value: intmax_t) -> Value {
      Value::new(unsafe { extract_named_function![self.env, make_integer](self.env, value) })
    }

    fn extract_integer(&mut self, value: Value) -> intmax_t {
      unsafe {
        extract_named_function![self.env, extract_integer](self.env, value.get_emacs_value())
      }
    }

    fn make_string(&mut self, s: &str) -> Value {
      let s = CString::new(s.as_bytes()).expect("could not convert into CStr in make_string");
      Value::new(unsafe {
        extract_named_function![self.env, make_string](
          self.env,
          s.as_ptr(),
          s.to_bytes()
            .len()
            .try_into()
            .expect("usize should convert to isize in make_string!"),
        )
      })
    }

    fn make_function(
      &mut self,
      min_arity: isize,
      max_arity: isize,
      function: UserFunction,
      documentation: &str,
      data: *mut c_void,
    ) -> Value {
      Value::new(unsafe {
        extract_named_function![self.env, make_function](
          self.env,
          min_arity,
          max_arity,
          Some(function),
          CString::new(documentation.as_bytes())
            .expect("could not convert into CStr in make_function")
            .as_ptr(),
          data,
        )
      })
    }

    fn intern_only_ascii(&mut self, symbol_name: &AsciiString) -> Value {
      let symbol_name = CString::new(symbol_name.as_bytes().to_vec()).unwrap();
      Value::new(unsafe {
        extract_named_function![self.env, intern](self.env, symbol_name.as_ptr())
      })
    }

    fn funcall<const N: usize>(&mut self, function: Value, args: &mut [emacs_value; N]) -> Value {
      Value::new(unsafe {
        extract_named_function![self.env, funcall](
          self.env,
          function.get_emacs_value(),
          N as isize,
          args.as_mut_ptr(),
        )
      })
    }

    fn eq(&mut self, val1: Value, val2: Value) -> bool {
      unsafe {
        extract_named_function![self.env, eq](
          self.env,
          val1.get_emacs_value(),
          val2.get_emacs_value(),
        )
      }
    }

    fn is_not_nil(&mut self, val: Value) -> bool {
      unsafe {
        extract_named_function![self.env, is_not_nil](self.env, unsafe { val.get_emacs_value() })
      }
    }
  }

  pub trait EnvWrapper {
    fn get_env(&self) -> EnvHandle;
  }
}

pub mod strings {
  use super::{bindings::*, wrappers::*};

  use std::convert::TryInto;

  pub struct StringEnv {
    env: EnvHandle,
  }

  impl StringEnv {
    pub fn new(env: EnvHandle) -> Self {
      Self { env }
    }
  }

  impl EnvWrapper for StringEnv {
    fn get_env(&self) -> EnvHandle {
      self.env.clone()
    }
  }

  pub struct EString {
    string_env: StringEnv,
    inner: Value,
  }

  impl EString {
    pub fn new(string_env: StringEnv, inner: Value) -> Self {
      Self { string_env, inner }
    }
  }

  impl EmacsValue for EString {
    fn as_emacs_value(&self) -> Value {
      self.inner.as_emacs_value()
    }
    fn from_emacs_value(env: EnvHandle, value: Value) -> Self {
      Self::new(StringEnv::new(env), value)
    }
  }

  impl EnvWrapper for EString {
    fn get_env(&self) -> EnvHandle {
      self.string_env.get_env()
    }
  }

  pub trait EmacsString {
    fn to_string(self) -> String;
  }

  impl EmacsString for EString {
    fn to_string(self) -> String {
      self.get_env().copy_string_contents(self.as_emacs_value())
    }
  }
}

pub mod lists {
  use super::{bindings::*, wrappers::*};

  use std::{convert::TryInto, marker::PhantomData};

  pub trait EmacsListEnvironment {
    fn is_not_nil(&self, value: Value) -> bool;
  }

  pub struct ListEnv {
    env: EnvHandle,
  }

  impl ListEnv {
    pub fn new(env: EnvHandle) -> Self {
      Self { env }
    }
  }

  impl EnvWrapper for ListEnv {
    fn get_env(&self) -> EnvHandle {
      self.env.clone()
    }
  }

  impl EmacsListEnvironment for ListEnv {
    fn is_not_nil(&self, value: Value) -> bool {
      self.get_env().get_ref().write().is_not_nil(value)
    }
  }

  pub trait EmacsList<V: EmacsValue> {
    fn is_empty(&self) -> bool;
    fn to_vec(&self) -> Vec<V>;
  }

  #[must_use]
  pub struct List<V: EmacsValue> {
    list_env: ListEnv,
    inner: Value,
    _x: PhantomData<V>,
  }

  impl<V> EnvWrapper for List<V>
  where
    V: EmacsValue,
  {
    fn get_env(&self) -> EnvHandle {
      self.list_env.get_env()
    }
  }

  impl<V> EmacsValue for List<V>
  where
    V: EmacsValue,
  {
    fn as_emacs_value(&self) -> Value {
      self.inner.as_emacs_value()
    }
    fn from_emacs_value(env: EnvHandle, value: Value) -> Self {
      Self::new(ListEnv::new(env), value)
    }
  }

  impl<V> List<V>
  where
    V: EmacsValue,
  {
    pub fn new(list_env: ListEnv, inner: Value) -> Self {
      Self {
        list_env,
        inner: inner,
        _x: PhantomData,
      }
    }
  }

  impl<V> EmacsList<V> for List<V>
  where
    V: EmacsValue,
  {
    fn is_empty(&self) -> bool {
      !self.list_env.is_not_nil(self.as_emacs_value())
    }

    fn to_vec(&self) -> Vec<V> {
      if self.is_empty() {
        return Vec::new();
      }

      let env_handle = self.get_env();
      let env = env_handle.get_ref();
      let mut cur_element = call_interned_function![
        env.write(),
        "car",
        &mut [unsafe { self.as_emacs_value().get_emacs_value() }]
      ];
      let mut rest = call_interned_function![
        env.write(),
        "cdr",
        &mut [unsafe { self.as_emacs_value().get_emacs_value() }]
      ];
      let mut elements: Vec<V> = Vec::new();

      while self.list_env.is_not_nil(rest.clone()) {
        elements.push(V::from_emacs_value(env_handle.clone(), cur_element));
        cur_element = call_interned_function![
          env.write(),
          "car",
          &mut [unsafe { rest.clone().get_emacs_value() }]
        ];
        rest =
          call_interned_function![env.write(), "cdr", &mut [unsafe { rest.get_emacs_value() }]];
      }

      elements.push(V::from_emacs_value(env_handle.clone(), cur_element));
      elements
    }
  }
}

pub mod buffers {
  use super::{bindings::*, lists::*, wrappers::*};

  pub struct BufferEnv {
    env: EnvHandle,
  }

  impl BufferEnv {
    pub fn new(env: EnvHandle) -> Self {
      Self { env: env.clone() }
    }
  }

  impl EnvWrapper for BufferEnv {
    fn get_env(&self) -> EnvHandle {
      self.env.clone()
    }
  }

  pub trait EmacsBufferEnvironment {
    fn buffer_list(&self) -> List<Buffer>;
  }

  impl EmacsBufferEnvironment for BufferEnv {
    fn buffer_list(&self) -> List<Buffer> {
      let env_handle = self.get_env();
      let env = env_handle.get_ref();
      /* This is needed to bypass a borrow error, weirdly. */
      let ret = List::new(
        ListEnv::new(env_handle.clone()),
        call_interned_function![env.write(), "buffer-list", &mut []],
      );
      ret
    }
  }

  pub trait EmacsBuffer {
    fn extract_text(&self) -> String;
  }

  #[must_use]
  pub struct Buffer {
    buffer_env: BufferEnv,
    inner: Value,
  }

  impl Buffer {
    pub fn new(buffer_env: BufferEnv, inner: Value) -> Self {
      Self { buffer_env, inner }
    }
  }

  impl EnvWrapper for Buffer {
    fn get_env(&self) -> EnvHandle {
      self.buffer_env.get_env()
    }
  }

  impl EmacsValue for Buffer {
    fn as_emacs_value(&self) -> Value {
      self.inner.as_emacs_value()
    }
    fn from_emacs_value(env: EnvHandle, value: Value) -> Self {
      Self::new(BufferEnv::new(env), value)
    }
  }

  impl EmacsBuffer for Buffer {
    fn extract_text(&self) -> String {
      let env_handle = self.get_env();
      let env = env_handle.get_ref();
      let text = call_interned_function![
        env.write(),
        "buffer-string",
        &mut [unsafe { self.as_emacs_value().get_emacs_value() }]
      ];
      env_handle.copy_string_contents(text)
    }
  }
}
