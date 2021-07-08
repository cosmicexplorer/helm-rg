/* Copyright 2021 Danny McClanahan */
/* SPDX-License-Identifier: GPL-3.0-or-later */

/* See <https://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Modules.html>. */

/* Generated with cbindgen:0.19.0 */

#define EMACS_MAJOR_VERSION 28

#define EMACS_LIMB_MAX -1

typedef struct emacs_value_tag {
  uint8_t _unused[0];
} emacs_value_tag;

typedef struct emacs_value_tag *emacs_value;

typedef struct emacs_env_private {
  uint8_t _address;
} emacs_env_private;

typedef unsigned int emacs_funcall_exit;

typedef long __intmax_t;

typedef __intmax_t intmax_t;

typedef unsigned int emacs_process_input_result;

typedef long __time_t;

typedef long __syscall_slong_t;

typedef struct timespec {
  __time_t tv_sec;
  __syscall_slong_t tv_nsec;
} timespec;

typedef size_t emacs_limb_t;

typedef struct emacs_env_28 {
  intptr_t size;
  struct emacs_env_private *private_members;
  emacs_value (*make_global_ref)(emacs_env *env, emacs_value value);
  void (*free_global_ref)(emacs_env *env, emacs_value global_value);
  emacs_funcall_exit (*non_local_exit_check)(emacs_env *env);
  void (*non_local_exit_clear)(emacs_env *env);
  emacs_funcall_exit (*non_local_exit_get)(emacs_env *env, emacs_value *symbol, emacs_value *data);
  void (*non_local_exit_signal)(emacs_env *env, emacs_value symbol, emacs_value data);
  void (*non_local_exit_throw)(emacs_env *env, emacs_value tag, emacs_value value);
  emacs_value (*make_function)(emacs_env *env, intptr_t min_arity, intptr_t max_arity, emacs_value (*func)(emacs_env *env, intptr_t nargs, emacs_value *args, void *data), const char *docstring, void *data);
  emacs_value (*funcall)(emacs_env *env, emacs_value func, intptr_t nargs, emacs_value *args);
  emacs_value (*intern)(emacs_env *env, const char *name);
  emacs_value (*type_of)(emacs_env *env, emacs_value arg);
  bool (*is_not_nil)(emacs_env *env, emacs_value arg);
  bool (*eq)(emacs_env *env, emacs_value a, emacs_value b);
  intmax_t (*extract_integer)(emacs_env *env, emacs_value arg);
  emacs_value (*make_integer)(emacs_env *env, intmax_t n);
  double (*extract_float)(emacs_env *env, emacs_value arg);
  emacs_value (*make_float)(emacs_env *env, double d);
  bool (*copy_string_contents)(emacs_env *env, emacs_value value, char *buf, intptr_t *len);
  emacs_value (*make_string)(emacs_env *env, const char *str_, intptr_t len);
  emacs_value (*make_user_ptr)(emacs_env *env, void (*fin)(void *arg1), void *ptr);
  void *(*get_user_ptr)(emacs_env *env, emacs_value arg);
  void (*set_user_ptr)(emacs_env *env, emacs_value arg, void *ptr);
  void (*(*get_user_finalizer)(void *arg1, emacs_env *env, emacs_value uptr))(void *arg1, emacs_env *env, emacs_value uptr);
  void (*set_user_finalizer)(emacs_env *env, emacs_value arg, void (*fin)(void *arg1));
  emacs_value (*vec_get)(emacs_env *env, emacs_value vector, intptr_t index);
  void (*vec_set)(emacs_env *env, emacs_value vector, intptr_t index, emacs_value value);
  intptr_t (*vec_size)(emacs_env *env, emacs_value vector);
  bool (*should_quit)(emacs_env *env);
  emacs_process_input_result (*process_input)(emacs_env *env);
  struct timespec (*extract_time)(emacs_env *env, emacs_value arg);
  emacs_value (*make_time)(emacs_env *env, struct timespec time);
  bool (*extract_big_integer)(emacs_env *env, emacs_value arg, int *sign, intptr_t *count, emacs_limb_t *magnitude);
  emacs_value (*make_big_integer)(emacs_env *env, int sign, intptr_t count, const emacs_limb_t *magnitude);
  void (*(*get_function_finalizer)(void *arg1, emacs_env *env, emacs_value arg))(void *arg1, emacs_env *env, emacs_value arg);
  void (*set_function_finalizer)(emacs_env *env, emacs_value arg, void (*fin)(void *arg1));
  int (*open_channel)(emacs_env *env, emacs_value pipe_process);
  void (*make_interactive)(emacs_env *env, emacs_value function, emacs_value spec);
  emacs_value (*make_unibyte_string)(emacs_env *env, const char *str_, intptr_t len);
} emacs_env_28;

typedef struct emacs_env_28 emacs_env;

typedef struct emacs_runtime_private {
  uint8_t _address;
} emacs_runtime_private;

typedef struct emacs_runtime {
  intptr_t size;
  struct emacs_runtime_private *private_members;
  emacs_env *(*get_environment)(struct emacs_runtime *runtime);
} emacs_runtime;

#define emacs_funcall_exit_emacs_funcall_exit_return 0

#define emacs_funcall_exit_emacs_funcall_exit_signal 1

#define emacs_funcall_exit_emacs_funcall_exit_throw 2

#define emacs_process_input_result_emacs_process_input_continue 0

#define emacs_process_input_result_emacs_process_input_quit 1

extern const int plugin_is_GPL_compatible;

/**
 * *TODO: generate docstring from rust docstring!*
 */
emacs_value helm_rg_string_match(emacs_env *env, intptr_t _nargs, emacs_value *_args, void *_data);

/**
 * Initialize module. *See [docs].*
 *
 * [docs]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Module-Initialization.html
 */
int emacs_module_init(struct emacs_runtime *runtime);
