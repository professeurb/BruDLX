#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <stdbool.h>
#include <stdint.h>

#define left 0
#define right 1
#define up 2
#define down 3
#define datum 4

typedef struct Cell {
  struct Cell *lft;
  struct Cell *rgt;
  struct Cell *upp;
  struct Cell *dwn;
  struct Cell *dat;
} cell;

static inline void insert_horiz(cell *l, cell *i) {
  cell *r = l->rgt;
  l->rgt = i;
  i->lft = l;
  r->lft = i;
  i->rgt = r;
}

static inline void insert_vert(cell *u, cell *i) {
  cell *d = u->dwn;
  u->dwn = i;
  i->upp = u;
  d->upp = i;
  i->dwn = d;
}

static inline void hide_vert(cell *i) {
  cell *above = i->upp;
  cell *below = i->dwn;
  above->dwn = below;
  below->upp = above;
}

static inline void restore_vert(cell *i) {
  cell *above = i->upp;
  cell *below = i->dwn;
  above->dwn = i;
  below->upp = i;
}

static inline void cover_col_aux2(cell *row) {
  cell *pos = row->rgt;
  while (pos != row) {
    hide_vert(pos);
    pos->dat->dat -= 1;
    pos = pos->rgt;
  }
}

static inline void cover_col_aux1(cell *col) {
  cell *row = col->dwn;
  while (row != col) {
    cover_col_aux2(row);
    row = row->dwn;
  }
}

static inline void cover_column(cell *col) {
  cell *l = col->lft;
  cell *r = col->rgt;
  l->rgt = r;
  r->lft = l;
  cover_col_aux1(col);
}

static inline void cover_column_register(cell *head, cell *col) {
  cell *l = col->lft;
  cell *r = col->rgt;
  l->rgt = r;
  r->lft = l;
  col->rgt = head->dwn;
  head->dwn = col;
  cover_col_aux1(col);
}

static inline void uncover_col_aux2(cell *row) {
  cell *pos = row->lft;
  while (pos != row) {
    restore_vert(pos);
    pos->dat->dat += 1;
    pos = pos->lft;
  }
}

static inline void uncover_col_aux1(cell *col, cell *row) {
  while (row != col) {
    uncover_col_aux2(row);
    row = row->upp;
  }
}

static inline void uncover_col_aux1_unregister(cell *col) {
  cell *row = col->upp;
  while (true) {
    uncover_col_aux2(row);
    cell *next = row->upp;
    if (next == col) {
      col->dwn = row;
      break;
    }
    row = next;
  }
}

static inline void uncover_column(cell *col) {
  uncover_col_aux1(col, col->upp);
  cell *l = col->lft;
  cell *r = col->rgt;
  l->rgt = col;
  r->lft = col;
}

static inline void uncover_column_unregister(cell *head) {
  cell *col = head->dwn;
  // cell *row = col -> upp;
  head->dwn = col->rgt;
  uncover_col_aux1_unregister(col);
  cell *l = col->lft;
  cell *r = l->rgt;
  l->rgt = col;
  r->lft = col;
  col->rgt = r;
}

static inline void cover_right(cell *row) {
  cell *pos = row->rgt;
  while (pos != row) {
    cover_column(pos->dat);
    pos = pos->rgt;
  }
}

static inline void uncover_left(cell *row) {
  cell *pos = row->lft;
  while (pos != row) {
    uncover_column(pos->dat);
    pos = pos->lft;
  }
}

void mix(cell *head, bool forward) {
  cell *col, *row;

  if (forward)
    goto forward;

backward:
  col = head->dwn;

  if (col == head)
    return;
  row = col->dwn;
  uncover_left(row);
  row = row->dwn; // next row

  if (row == col) {
    uncover_column_unregister(head);
    goto backward;
  }
  col->dwn = row;
  cover_right(row);

forward:
  col = head->rgt;

  if (col == head)
    return;
  cell *card = col->dat;
  cell *cand = col->rgt;
  while (cand != head) {
    cell *new_card = cand->dat;
    if (new_card < card) {
      col = cand;
      card = new_card;
    }
    cand = cand->rgt;
  }
  row = col->dwn;
  if (row == col)
    goto backward;
  cover_column_register(head, col);
  cover_right(row);
  goto forward;
}

void _forward(cell *head);

void _backward(cell *head) {
  while (true) {
    cell *col = head->dwn;

    if (col == head)
      return;
    cell *row = col->dwn;
    uncover_left(row);
    row = row->dwn; // next row

    if (row != col) {
      col->dwn = row;
      cover_right(row);
      return _forward(head);
    }
    uncover_column_unregister(head);
  }
}

void _forward(cell *head) {
  while (true) {
    cell *col = head->rgt;

    if (col == head) {
      // We are a a leaf
      // All the columns are covered
      return;
    }
    // Otherwise, we select the next branch
    // i.e. the next col to cover
    cell *card = col->dat;
    cell *cand = col->rgt;
    while (cand != head) {
      cell *new_card = cand->dat;
      if (new_card < card) {
        col = cand;
        card = new_card;
      }
      cand = cand->rgt;
    }
    // The selected branch is col
    cell *row = col->dwn;
    if (row == col)
      return _backward(head);
    // Enter the branch and continue
    cover_column_register(head, col);
    cover_right(row);
  }
}

/*
void down(cell *head);
void up(cell *head);
void step(cell *head);

void down(cell *head) {}
void up(cell *head) {}
void step(cell *head) {}
*/

CAMLprim value forward(value bigarray) {
  cell *head = Caml_ba_data_val(bigarray);
  // mix(head, true);
  _forward(head);
  return Val_unit;
}

CAMLprim value backward(value bigarray) {
  cell *head = Caml_ba_data_val(bigarray);
  // mix(head, false);
  _backward(head);
  return Val_unit;
}

CAMLprim value prepare(value bigarray) {
  int64_t *arr = Caml_ba_data_val(bigarray);
  // Update pointers
  int dim = Caml_ba_array_val(bigarray)->dim[0];
  for (int i = 0; i < dim; i++) {
    arr[i] = (int64_t)arr + arr[i] * sizeof(struct Cell *);
  }
  return Val_unit;
}
