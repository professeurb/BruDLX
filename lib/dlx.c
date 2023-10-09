#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <stdbool.h>
#include <stdint.h>

typedef struct Cell {
  struct Cell *lft;
  struct Cell *rgt;
  struct Cell *upp;
  struct Cell *dwn;
  struct Cell *dat;
} cell;

static inline void hide(cell *row) {
  cell *pos = row->rgt;
  while (pos != row) {
    cell *above = pos->upp;
    cell *below = pos->dwn;
    above->dwn = below;
    below->upp = above;
    pos->dat->dat -= 1;
    pos = pos->rgt;
  }
}

static inline void unhide(cell *row) {
  cell *pos = row->lft;
  while (pos != row) {
    cell *above = pos->upp;
    cell *below = pos->dwn;
    above->dwn = pos;
    below->upp = pos;
    pos->dat->dat += 1;
    pos = pos->lft;
  }
}

static inline void cover(cell *col) {
  cell *l = col->lft;
  cell *r = col->rgt;
  l->rgt = r;
  r->lft = l;
  cell *row = col->dwn;
  while (row != col) {
    hide(row);
    row = row->dwn;
  }
}

static inline void uncover(cell *col) {
  cell *row = col->upp;
  while (row != col) {
    unhide(row);
    row = row->upp;
  }
  cell *l = col->lft;
  cell *r = col->rgt;
  l->rgt = col;
  r->lft = col;
}

static inline void record(cell *head, cell *col) {
  cell *l = col->lft;
  cell *r = col->rgt;
  l->rgt = r;
  r->lft = l;
  col->rgt = head->dwn;
  head->dwn = col;
  cell *row = col->dwn;
  while (row != col) {
    hide(row);
    row = row->dwn;
  }
}

static inline void unrecord(cell *head) {
  cell *col = head->dwn;
  head->dwn = col->rgt;
  cell *row = col->upp;
  while (true) {
    unhide(row);
    cell *next = row->upp;
    if (next == col) {
      col->dwn = row;
      break;
    }
    row = next;
  }
  cell *l = col->lft;
  cell *r = l->rgt;
  l->rgt = col;
  r->lft = col;
  col->rgt = r;
}

void down(cell *head);

void up(cell *head) {
  while (true) {
    cell *col = head->dwn;

    if (col == head)
      return;
    cell *row = col->dwn;
    cell *pos = row->lft;
    while (pos != row) {
      uncover(pos->dat);
      pos = pos->lft;
    }

    row = row->dwn;

    if (row != col) {
      col->dwn = row;
      cell *pos = row->rgt;
      while (pos != row) {
        cover(pos->dat);
        pos = pos->rgt;
      }
      return down(head);
    }
    unrecord(head);
  }
}

void down(cell *head) {
  while (true) {
    cell *col = head->rgt;

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
    cell *row = col->dwn;
    if (row == col)
      return up(head);
    record(head, col);
    cell *pos = row->rgt;
    while (pos != row) {
      cover(pos->dat);
      pos = pos->rgt;
    }
  }
}

CAMLprim value forward(value bigarray) {
  cell *head = Caml_ba_data_val(bigarray);
  // mix(head, true);
  down(head);
  return Val_unit;
}

CAMLprim value backward(value bigarray) {
  cell *head = Caml_ba_data_val(bigarray);
  // mix(head, false);
  up(head);
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
