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

  use std::{convert::TryInto, ffi::CStr, os::raw::c_void};

  pub type UserFunction = unsafe extern "C" fn(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    data: *mut c_void,
  ) -> emacs_value;

  pub trait StaticDynamicSizeCheckable {
    fn dynamic_size(&self) -> usize;
    fn static_size() -> usize;
  }

  #[derive(Copy, Clone, Debug)]
  pub struct Runtime {
    rt: *mut emacs_runtime,
  }

  impl Runtime {
    pub fn new(rt: *mut emacs_runtime) -> Self {
      Self { rt }
    }

    pub fn get_environment(&mut self) -> Env {
      let get_environment = unsafe {
        (*self.rt)
          .get_environment
          .expect("get_environment pointer should be set")
      };

      Env::new(unsafe { get_environment(self.rt) })
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

  #[derive(Copy, Clone, Debug)]
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

  #[derive(Copy, Clone, Debug)]
  pub struct Env {
    env: *mut emacs_env,
  }

  impl Env {
    pub fn new(env: *mut emacs_env) -> Self {
      Self { env }
    }

    pub fn make_integer(&mut self, value: intmax_t) -> Value {
      let make_integer = unsafe {
        (*self.env)
          .make_integer
          .expect("make_integer should be set")
      };
      Value::new(unsafe { make_integer(self.env, value) })
    }

    pub fn make_function(
      &mut self,
      min_arity: isize,
      max_arity: isize,
      function: UserFunction,
      documentation: &CStr,
      data: *mut c_void,
    ) -> Value {
      let make_function = unsafe {
        (*self.env)
          .make_function
          .expect("make_function pointer should be set")
      };

      Value::new(unsafe {
        make_function(
          self.env,
          min_arity,
          max_arity,
          Some(function),
          documentation.as_ptr(),
          data,
        )
      })
    }

    pub fn intern(&mut self, symbol_name: &CStr) -> Value {
      let intern = unsafe {
        (*self.env)
          .intern
          .expect("intern function pointer should be set")
      };

      Value::new(unsafe { intern(self.env, symbol_name.as_ptr()) })
    }

    pub fn funcall<const N: usize>(
      &mut self,
      function: Value,
      args: &mut [emacs_value; N],
    ) -> Value {
      let funcall = unsafe {
        (*self.env)
          .funcall
          .expect("funcall function pointer should be set")
      };

      Value::new(unsafe {
        funcall(
          self.env,
          function.get_emacs_value(),
          N as isize,
          args.as_mut_ptr(),
        )
      })
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
}
