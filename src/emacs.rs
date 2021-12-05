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
    fn make_integer(&mut self, value: intmax_t) -> Value;
    fn make_function(
      &mut self,
      min_arity: isize,
      max_arity: isize,
      function: UserFunction,
      documentation: &CStr,
      data: *mut c_void,
    ) -> Value;
    fn intern(&mut self, symbol_name: &CStr) -> Value;
    fn funcall<const N: usize>(&mut self, function: Value, args: &mut [emacs_value; N]) -> Value;

    /// Bind NAME to FUN.
    #[allow(non_snake_case)]
    fn bind_function(&mut self, name: &CStr, Sfun: Value) -> Value;

    /// Provide FEATURE to Emacs.
    fn provide(&mut self, feature: &CStr) -> Value;
  }

  impl Env {
    pub fn new(env: *mut emacs_env) -> Self {
      Self { env }
    }

    fn get_fset_symbol(&mut self) -> Value {
      self.intern(&CString::new(b"fset".to_vec()).expect("\"fset\" should be a valid CStr"))
    }

    fn get_provide_symbol(&mut self) -> Value {
      self.intern(&CString::new(b"provide".to_vec()).expect("\"provide\" should be a valid CStr"))
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

    fn make_function(
      &mut self,
      min_arity: isize,
      max_arity: isize,
      function: UserFunction,
      documentation: &CStr,
      data: *mut c_void,
    ) -> Value {
      Value::new(unsafe {
        extract_named_function![self.env, make_function](
          self.env,
          min_arity,
          max_arity,
          Some(function),
          documentation.as_ptr(),
          data,
        )
      })
    }

    fn intern(&mut self, symbol_name: &CStr) -> Value {
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

    #[allow(non_snake_case)]
    fn bind_function(&mut self, name: &CStr, Sfun: Value) -> Value {
      /* Set the function cell of the symbol named NAME to SFUN using
      the 'fset' function. */

      /* Convert the strings to symbols by interning them */
      let Qfset = self.get_fset_symbol();
      let Qsym = self.intern(name).get_emacs_value();

      /* Prepare the arguments array */
      let mut args: [emacs_value; 2] = [Qsym, Sfun.get_emacs_value()];

      /* Make the call (2 == nb of arguments) */
      self.funcall(Qfset, &mut args)
    }

    /// call 'provide' with FEATURE converted to a symbol
    #[allow(non_snake_case)]
    fn provide(&mut self, feature: &CStr) -> Value {
      let Qfeat = self.intern(feature);
      let Qprovide = self.get_provide_symbol();
      let mut args: [emacs_value; 1] = [Qfeat.get_emacs_value()];
      self.funcall(Qprovide, &mut args)
    }
  }
}
