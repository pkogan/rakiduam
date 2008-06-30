#include "/usr/cvs/ciaosystems/CiaoDE/ciao//include/LINUXi86/ciao_gluecode.h"

void socket_c(long, long *);
BOOL gluecode_socket_c(struct worker *w) {
  ciao_term t0;
  ciao_term t1;
  long c0;
  long c1;
  ciao_term u1;
  DECL_STATE;
  INIT_STATE;
  ciao_frame_begin_s(state);
  t0 = ciao_ref(state, X(0));
  t1 = ciao_ref(state, X(1));
  if (!ciao_is_integer_s(state, t0)) ERROR_IN_ARG(X(0), 0 + 1, INTEGER);
  c0 = ciao_to_integer_s(state, t0);
  IMPLICIT_STATE;
  GLUECODE_TRY(socket_c(c0, &c1));
  u1 = ciao_integer_s(state, c1);
  if (!ciao_unify_s(state, u1, t1)) return FALSE;
  ciao_frame_end_s(state);
  return TRUE;
}

char *recv_c(long, long);
BOOL gluecode_recv_c(struct worker *w) {
  ciao_term t0;
  ciao_term t1;
  ciao_term t2;
  long c0;
  long c1;
  char *c2;
  ciao_term u2;
  DECL_STATE;
  INIT_STATE;
  ciao_frame_begin_s(state);
  t0 = ciao_ref(state, X(0));
  t1 = ciao_ref(state, X(1));
  t2 = ciao_ref(state, X(2));
  if (!ciao_is_integer_s(state, t0)) ERROR_IN_ARG(X(0), 0 + 1, INTEGER);
  if (!ciao_is_integer_s(state, t1)) ERROR_IN_ARG(X(1), 1 + 1, INTEGER);
  c0 = ciao_to_integer_s(state, t0);
  c1 = ciao_to_integer_s(state, t1);
  IMPLICIT_STATE;
  GLUECODE_TRY(c2 = recv_c(c0, c1));
  u2 = ciao_str_to_list(state, c2);
  if (!ciao_unify_s(state, u2, t2)) return FALSE;
  ciao_frame_end_s(state);
  return TRUE;
}

void close_c(long);
BOOL gluecode_close_c(struct worker *w) {
  ciao_term t0;
  long c0;
  DECL_STATE;
  INIT_STATE;
  ciao_frame_begin_s(state);
  t0 = ciao_ref(state, X(0));
  if (!ciao_is_integer_s(state, t0)) ERROR_IN_ARG(X(0), 0 + 1, INTEGER);
  c0 = ciao_to_integer_s(state, t0);
  IMPLICIT_STATE;
  GLUECODE_TRY(close_c(c0));
  ciao_frame_end_s(state);
  return TRUE;
}

void socket_udp_init(char *module) {
  define_c_mod_predicate(module, "socket_c", 2, gluecode_socket_c);
  define_c_mod_predicate(module, "recv_c", 3, gluecode_recv_c);
  define_c_mod_predicate(module, "close_c", 1, gluecode_close_c);
}

void socket_udp_end(char *module) {
  undefine_c_mod_predicate(module, "socket_c", 2);
  undefine_c_mod_predicate(module, "recv_c", 3);
  undefine_c_mod_predicate(module, "close_c", 1);
}

