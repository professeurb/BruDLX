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

static inline void _insert_horiz(cell *l, cell *i) {
  cell *r = l->rgt;
  l->rgt = i;
  i->lft = l;
  r->lft = i;
  i->rgt = r;
}

// void insert_horiz_old(int64_t *arr, int64_t l, int64_t i) {
//   int64_t r = arr[l + right];
//   arr[l + right] = i;
//   arr[i + left] = l;
//   arr[r + left] = i;
//   arr[i + right] = r;
// }

static inline void _insert_vert(cell *u, cell *i) {
  cell *d = u->dwn;
  u->dwn = i;
  i->upp = u;
  d->upp = i;
  i->dwn = d;
}

// void insert_vert_old(int64_t *arr, int64_t u, int64_t i) {
//   int64_t d = arr[u + down];
//   arr[u + down] = i;
//   arr[i + up] = u;
//   arr[d + up] = i;
//   arr[i + down] = d;
// }

static inline void _hide_vert(cell *i) {
  cell *above = i->upp;
  cell *below = i->dwn;
  above->dwn = below;
  below->upp = above;
}

// void hide_vert_old(int64_t *arr, int64_t i) {
//   int64_t above = arr[i + up];
//   int64_t below = arr[i + down];
//   arr[above + down] = below;
//   arr[below + up] = above;
// }

static inline void _restore_vert(cell *i) {
  cell *above = i->upp;
  cell *below = i->dwn;
  above->dwn = i;
  below->upp = i;
}

// void restore_vert_old(int64_t *arr, int64_t i) {
//   int64_t above = arr[i + up];
//   int64_t below = arr[i + down];
//   arr[above + down] = i;
//   arr[below + up] = i;
// }

static inline void _cover_col_aux2(cell *row) {
  cell *pos = row->rgt;
  while (pos != row) {
    _hide_vert(pos);
    pos->dat->dat -= 1;
    pos = pos->rgt;
  }
}

// void cover_col_aux2_old(int64_t *arr, int64_t row) {
//   int64_t pos = arr[row + right];
//   while (pos != row) {
//     hide_vert_old(arr, pos);
//     arr[arr[pos + datum] + datum] -= 1;
//     pos = arr[pos + right];
//   }
// }

static inline void _cover_col_aux1(cell *col) {
  cell *row = col->dwn;
  while (row != col) {
    _cover_col_aux2(row);
    row = row->dwn;
  }
}

// void cover_col_aux1_old(int64_t *arr, int64_t col) {
//   int64_t row = arr[col + down];
//   while (row != col) {
//     // int64_t pos = arr[row + right];
//     cover_col_aux2_old(arr, row);
//     row = arr[row + down];
//   }
// }

static inline void _cover_column(cell *col) {
  cell *l = col->lft;
  cell *r = col->rgt;
  l->rgt = r;
  r->lft = l;
  _cover_col_aux1(col);
}

// void cover_column_old(int64_t *arr, int64_t col) {
//   int64_t l = arr[col + left];
//   int64_t r = arr[col + right];
//   arr[l + right] = r;
//   arr[r + left] = l;
//   cover_col_aux1_old(arr, col);
// }

static inline void _cover_column_weird(cell *head, cell *col) {
  cell *l = col->lft;
  cell *r = col->rgt;
  l->rgt = r;
  r->lft = l;
  col->rgt = head->dwn;
  head->dwn = col;
  _cover_col_aux1(col);
}

// void cover_column_weird_old(int64_t *arr, int64_t col) {
//   int64_t l = arr[col + left];
//   int64_t r = arr[col + right];
//   arr[l + right] = r;
//   arr[r + left] = l;
//   arr[col + right] = arr[0 + down];
//   arr[0 + down] = col;
//   cover_col_aux1_old(arr, col);
// }

static inline void _uncover_col_aux2(cell *row) {
  cell *pos = row->lft;
  while (pos != row) {
    _restore_vert(pos);
    pos->dat->dat += 1;
    pos = pos->lft;
  }
}

// void uncover_col_aux2_old(int64_t *arr, int64_t row) {
//   int64_t pos = arr[row + left];
//   while (pos != row) {
//     restore_vert_old(arr, pos);
//     arr[arr[pos + datum] + datum] += 1;
//     pos = arr[pos + left];
//   }
// }

static inline void _uncover_col_aux1(cell *col, cell *row) {
  while (row != col) {
    _uncover_col_aux2(row);
    row = row->upp;
  }
}

// void uncover_col_aux1_old(int64_t *arr, int64_t col, int64_t row) {
//   while (row != col) {
//     uncover_col_aux2_old(arr, row);
//     row = arr[row + up];
//   }
// }

static inline void _uncover_col_aux1_weird(cell *col) {
  cell *row = col->upp;
  while (true) {
    _uncover_col_aux2(row);
    cell *next = row->upp;
    if (next == col) {
      col->dwn = row;
      break;
    }
    row = next;
  }
}

// void uncover_col_aux1_weird_old(int64_t *arr, int64_t col) {
//   int64_t row = arr[col + up];
//   while (true) {
//     uncover_col_aux2_old(arr, row);
//     int64_t next = arr[row + up];
//     if (next == col) {
//       arr[col + down] = row;
//       break;
//     }
//     row = next;
//   }
// }

static inline void _uncover_column(cell *col) {
  _uncover_col_aux1(col, col->upp);
  cell *l = col->lft;
  cell *r = col->rgt;
  l->rgt = col;
  r->lft = col;
}

// void uncover_column_old(int64_t *arr, int64_t col) {
//   uncover_col_aux1_old(arr, col, arr[col + up]);
//   int64_t l = arr[col + left];
//   int64_t r = arr[col + right];
//   arr[l + right] = col;
//   arr[r + left] = col;
// }

static inline void _uncover_column_weird(cell *head) {
  cell *col = head->dwn;
  // cell *row = col -> upp;
  head->dwn = col->rgt;
  _uncover_col_aux1_weird(col);
  cell *l = col->lft;
  cell *r = l->rgt;
  l->rgt = col;
  r->lft = col;
  col->rgt = r;
}

// void uncover_column_weird_old(int64_t *arr) {
//   int64_t col = arr[0 + down];
//   // int64_t row = arr[col + up];
//   arr[0 + down] = arr[col + right];
//   uncover_col_aux1_weird_old(arr, col);
//   int64_t l = arr[col + left];
//   int64_t r = arr[l + right];
//   arr[l + right] = col;
//   arr[r + left] = col;
//   arr[col + right] = r;
// }

static inline void _cover_right(cell *row) {
  cell *pos = row->rgt;
  while (pos != row) {
    _cover_column(pos->dat);
    pos = pos->rgt;
  }
}

// void cover_right_old(int64_t *arr, int64_t row) {
//   int64_t pos = arr[row + right];
//   while (pos != row) {
//     cover_column_old(arr, arr[pos + datum]);
//     pos = arr[pos + right];
//   }
// }

static inline void _uncover_left(cell *row) {
  cell *pos = row->lft;
  while (pos != row) {
    _uncover_column(pos->dat);
    pos = pos->lft;
  }
}

// void uncover_left_old(int64_t *arr, int64_t row) {
//   int64_t pos = arr[row + left];
//   while (pos != row) {
//     uncover_column_old(arr, arr[pos + datum]);
//     pos = arr[pos + left];
//   }
// }

void _mix(cell *head, bool forward) {
  cell *col, *row;

  if (forward)
    goto forward;

backward:
  col = head->dwn;

  if (col == head)
    return;
  row = col->dwn;
  _uncover_left(row);
  row = row->dwn; // next row

  if (row == col) {
    _uncover_column_weird(head);
    goto backward;
  }
  col->dwn = row;
  _cover_right(row);

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
  _cover_column_weird(head, col);
  _cover_right(row);
  goto forward;
}

// void mix(int64_t *arr, bool forward) {
//   int64_t col, row;
//
//   if (forward)
//     goto forward;
//
// backward:
//   col = arr[0 + down];
//
//   if (col == 0)
//     return;
//   row = arr[col + down];
//   uncover_left_old(arr, row);
//   row = arr[row + down]; // next row
//
//   if (row == col) {
//     uncover_column_weird_old(arr);
//     goto backward;
//   }
//   arr[col + down] = row;
//   cover_right_old(arr, row);
//
// forward:
//   col = arr[0 + right];
//
//   if (col == 0)
//     return;
//   int64_t card = arr[col + datum];
//   int64_t cand = arr[col + right];
//   while (cand != 0) {
//     int64_t new_card = arr[cand + datum];
//     if (new_card < card) {
//       col = cand;
//       card = new_card;
//     }
//     cand = arr[cand + right];
//   }
//   if (card == 0)
//     goto backward;
//   cover_column_weird_old(arr, col);
//   row = arr[col + down];
//   cover_right_old(arr, row);
//   goto forward;
// }

CAMLprim value forward(value bigarray) {
  cell *head = Caml_ba_data_val(bigarray);
  _mix(head, true);
  return Val_unit;
}

// CAMLprim value forward(value bigarray) {
//   int64_t *arr = Caml_ba_data_val(bigarray);
//   // _forward(arr);
//   mix(arr, true);
//   return Val_unit;
// }

CAMLprim value backward(value bigarray) {
  cell *head = Caml_ba_data_val(bigarray);
  _mix(head, false);
  return Val_unit;
}

// CAMLprim value backward(value bigarray) {
//   int64_t *arr = Caml_ba_data_val(bigarray);
//   // _backward(arr);
//   mix(arr, false);
//   return Val_unit;
// }

CAMLprim value prepare(value bigarray) {
  int64_t *arr = Caml_ba_data_val(bigarray);
  // Update pointers
  int dim = Caml_ba_array_val(bigarray)->dim[0];
  for (int i = 0; i < dim; i++) {
    arr[i] = (int64_t)arr + arr[i] * sizeof(struct Cell *);
    // arr[i] = (int64_t) &arr[i];
  }
  // cell *head = (cell *)arr;
  // // Setup counts correctly
  // cell *curr = head -> rgt;
  // while (curr != head) {
  //   int64_t cnt = (int64_t) (curr -> dat);
  //   cnt = (int64_t) arr + 5 * (cnt - (int64_t) arr);
  //   curr -> dat = (struct Cell *) cnt;
  //   curr = curr -> rgt;
  // }
  return Val_unit;
}
