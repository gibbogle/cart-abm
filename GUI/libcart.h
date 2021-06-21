#ifndef LIBCART_H
#define LIBCART_H

#ifdef __cplusplus
extern "C" {
#endif
//
//
void execute(int *,char *, int *,char *, int *);
void simulate_step(int *, int *, int *, int *);
void terminate_run(int *);
void get_initial_values(int *);

//
//
#ifdef __cplusplus
}
#endif

#endif // LIBCART_H
