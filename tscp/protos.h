/*
 *	PROTOS.H
 *	Tom Kerrigan's Simple Chess Program (TSCP)
 *
 *	Copyright 1997 Tom Kerrigan
 */


/* prototypes */

/* board.c */
void init_board();
void init_hash();
int hash_rand();
void set_hash();
BOOL in_check(int s);
BOOL attack(int sq, int s);
void gen();
void gen_caps();
void gen_push(int from, int to, int bits);
void gen_promote(int from, int to, int bits);
BOOL makemove(move_bytes m);
void takeback();
void syncBoard();
int checkBoard();
void init_attack_table();
void print_attacktables(char table[64]);

/* book.c */
void open_book();
void close_book();
int book_move();
BOOL book_match(char *s1, char *s2);

/* search.c */
void think(int output);
int search(int alpha, int beta, int depth);
int quiesce(int alpha, int beta);
int reps();
void sort_pv();
void sort(int from);
void checkup();

/* eval.c */
int eval();
int eval_light_pawn(int sq);
int eval_dark_pawn(int sq);
int eval_light_king(int sq);
int eval_lkp(int f);
int eval_dark_king(int sq);
int eval_dkp(int f);

/* main.c */
int get_ms();
int main();
int parse_move(char *s);
char *move_str(move_bytes m);
void print_board();
void xboard();
void print_result();
void bench();