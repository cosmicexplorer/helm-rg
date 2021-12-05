/* Copyright 2021 Danny McClanahan */
/* SPDX-License-Identifier: GPL-3.0-only */

#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(warnings)]

pub mod bindings {
  include!("bindings.rs");
}

pub mod wrappers {
  use super::bindings::*;

  use ascii::AsciiString;

  use std::{
    convert::TryInto,
    ffi::{CStr, CString},
    os::raw::c_void,
  };

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

  #[must_use]
  pub struct Value {
    val: emacs_value,
  }

  impl Value {
    pub fn new(val: emacs_value) -> Self {
      Self { val }
    }

    pub fn get_emacs_value(self) -> emacs_value {
      self.val
    }
  }

  pub struct Env {
    env: *mut emacs_env,
  }

  pub trait EmacsEnvironment {
    /// Return a lisp integer.
    fn make_integer(&mut self, value: intmax_t) -> Value;

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

    /// Return a lisp symbol from the Unicode string `symbol_name`.
    fn intern_unicode(&mut self, symbol_name: &str) -> Value;

    /// Execute the lisp function `function`.
    fn funcall<const N: usize>(&mut self, function: Value, args: &mut [emacs_value; N]) -> Value;

    /// Bind `Sfun` to `name` in the current environment.
    #[allow(non_snake_case)]
    fn bind_function(&mut self, name: &str, Sfun: Value) -> Value;

    /// Provide `feature` to Emacs.
    fn provide(&mut self, feature: &str) -> Value;
  }

  impl Env {
    pub fn new(env: *mut emacs_env) -> Self {
      Self { env }
    }

    fn ascii_string_or_panic(s: &str) -> AsciiString {
      AsciiString::from_ascii(s).expect("string was not ASCII!")
    }

    fn get_ascii_symbol_value(&mut self, s: &str) -> Value {
      self.intern_only_ascii(&Self::ascii_string_or_panic(s))
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
    fn make_integer(&mut self, value: intmax_t) -> Value {
      Value::new(unsafe { extract_named_function![self.env, make_integer](self.env, value) })
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

    fn intern_unicode(&mut self, symbol_name: &str) -> Value {
      let intern_function = self.get_ascii_symbol_value("intern");
      let symbol_name = self.make_string(symbol_name);
      let mut intern_args: [emacs_value; 2] = [
        symbol_name.get_emacs_value(),
        self.get_ascii_symbol_value("nil").get_emacs_value(),
      ];
      self.funcall(intern_function, &mut intern_args)
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

    #[allow(non_snake_case)]
    fn bind_function(&mut self, name: &str, Sfun: Value) -> Value {
      /* Set the function cell of the symbol named NAME to SFUN using
      the 'fset' function. */

      /* Convert the strings to symbols by interning them */
      let Qfset = self.get_ascii_symbol_value("fset");
      let Qsym = self.intern_unicode(name).get_emacs_value();

      /* Prepare the arguments array */
      let mut args: [emacs_value; 2] = [Qsym, Sfun.get_emacs_value()];

      /* Make the call (2 == nb of arguments) */
      self.funcall(Qfset, &mut args)
    }

    /// call 'provide' with FEATURE converted to a symbol
    #[allow(non_snake_case)]
    fn provide(&mut self, feature: &str) -> Value {
      let Qfeat = self.intern_unicode(feature);
      let Qprovide = self.get_ascii_symbol_value("provide");
      let mut args: [emacs_value; 1] = [Qfeat.get_emacs_value()];
      self.funcall(Qprovide, &mut args)
    }
  }
}
