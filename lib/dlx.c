#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <stdbool.h>
#include <stdint.h>

#define left 0
#define right 1
#define up 2
#define down 3
#define datum 4

void insert_horiz(int32_t *arr, int32_t l, int32_t i) {
  int32_t r = arr[l + right];
  arr[l + right] = i;
  arr[i + left] = l;
  arr[r + left] = i;
  arr[i + right] = r;
}

void insert_vert(int32_t *arr, int32_t u, int32_t i) {
  int32_t d = arr[u + down];
  arr[u + down] = i;
  arr[i + up] = u;
  arr[d + up] = i;
  arr[i + down] = d;
}

void hide_vert(int32_t *arr, int32_t i) {
  int32_t above = arr[i + up];
  int32_t below = arr[i + down];
  arr[above + down] = below;
  arr[below + up] = above;
}

void restore_vert(int32_t *arr, int32_t i) {
  int32_t above = arr[i + up];
  int32_t below = arr[i + down];
  arr[above + down] = i;
  arr[below + up] = i;
}

void cover_col_aux2(int32_t *arr, int32_t row) {
  int32_t pos = arr[row + right];
  while (pos != row) {
    hide_vert(arr, pos);
    arr[arr[pos + datum] + datum] -= 1;
    pos = arr[pos + right];
  }
}

void cover_col_aux1(int32_t *arr, int32_t col) {
  int32_t row = arr[col + down];
  while (row != col) {
    int32_t pos = arr[row + right];
    cover_col_aux2(arr, row);
    row = arr[row + down];
  }
}

void cover_column(int32_t *arr, int32_t col) {
  int32_t l = arr[col + left];
  int32_t r = arr[col + right];
  arr[l + right] = r;
  arr[r + left] = l;
  cover_col_aux1(arr, col);
}

void cover_column_weird(int32_t *arr, int32_t col) {
  int32_t l = arr[col + left];
  int32_t r = arr[col + right];
  arr[l + right] = r;
  arr[r + left] = l;
  arr[col + right] = arr[0 + down];
  arr[0 + down] = col;
  cover_col_aux1(arr, col);
}

void uncover_col_aux2(int32_t *arr, int32_t row) {
  int32_t pos = arr[row + left];
  while (pos != row) {
    restore_vert(arr, pos);
    arr[arr[pos + datum] + datum] += 1;
    pos = arr[pos + left];
  }
}

void uncover_col_aux1(int32_t *arr, int32_t col, int32_t row) {
  while (row != col) {
    int32_t pos = arr[row + left];
    uncover_col_aux2(arr, row);
    row = arr[row + up];
  }
}

void uncover_col_aux1_weird(int32_t *arr, int32_t col) {
  int32_t row = arr[col + up];
  while (true) {
    uncover_col_aux2(arr, row);
    int32_t next = arr[row + up];
    if (next == col) {
      arr[col + down] = row;
      break;
    }
    row = next;
  }
}

void uncover_column(int32_t *arr, int32_t col) {
  uncover_col_aux1(arr, col, arr[col + up]);
  int32_t l = arr[col + left];
  int32_t r = arr[col + right];
  arr[l + right] = col;
  arr[r + left] = col;
}

void uncover_column_weird(int32_t *arr) {
  int32_t col = arr[0 + down];
  int32_t row = arr[col + up];
  arr[0 + down] = arr[col + right];
  uncover_col_aux1_weird(arr, col);
  int32_t l = arr[col + left];
  int32_t r = arr[l + right];
  arr[l + right] = col;
  arr[r + left] = col;
  arr[col + right] = r;
}

void cover_right(int32_t *arr, int32_t row) {
  int32_t pos = arr[row + right];
  while (pos != row) {
    cover_column(arr, arr[pos + datum]);
    pos = arr[pos + right];
  }
}

void uncover_left(int32_t *arr, int32_t row) {
  int32_t pos = arr[row + left];
  while (pos != row) {
    uncover_column(arr, arr[pos + datum]);
    pos = arr[pos + left];
  }
}

void _backward(int32_t *arr);

void _forward(int32_t *arr) {
  int32_t col = arr[0 + right];

  if (col != 0) {
    int32_t card = arr[col + datum];
    int32_t cand = arr[col + right];
    while (cand != 0) {
      int32_t new_card = arr[cand + datum];
      if (new_card < card) {
        col = cand;
        card = new_card;
      }
      cand = arr[cand + right];
    }
    if (card == 0)
      _backward(arr);
    else {
      int32_t row = arr[col + down];
      cover_column_weird(arr, col);
      cover_right(arr, row);
      _forward(arr);
    }
  }
}

void _backward(int32_t *arr) {
  int32_t col = arr[0 + down];
  if (col != 0) {
    int32_t row = arr[col + down];
    uncover_left(arr, row);
    row = arr[row + down];
    if (row != col) {
      arr[col + down] = row;
      cover_right(arr, row);
      _forward(arr);
    } else {
      uncover_column_weird(arr);
      _backward(arr);
    }
  }
}

void _mix(int32_t *arr, bool forward) {
  int32_t col, row;

  if (forward)
    goto forward;

backward:
  col = arr[0 + down];

  if (col == 0)
    return;
  row = arr[col + down];
  uncover_left(arr, row);
  row = arr[row + down]; // next row

  // if (row != col) {
  //   arr[col + down] = row;
  //   cover_right(arr, row);
  //   goto forward;
  // } else {
  //   uncover_column_weird(arr);
  //   goto backward;
  // }

  if (row == col) {
    uncover_column_weird(arr);
    goto backward;
  }
  arr[col + down] = row;
  cover_right(arr, row);
  // goto forward;

forward:
  col = arr[0 + right];

  if (col == 0)
    return;
  int32_t card = arr[col + datum];
  int32_t cand = arr[col + right];
  while (cand != 0) {
    int32_t new_card = arr[cand + datum];
    if (new_card < card) {
      col = cand;
      card = new_card;
    }
    cand = arr[cand + right];
  }
  if (card == 0)
    goto backward;
  cover_column_weird(arr, col);
  row = arr[col + down];
  cover_right(arr, row);
  goto forward;
}

CAMLprim value forward(value bigarray) {
  int32_t *arr = Caml_ba_data_val(bigarray);
  // _forward(arr);
  _mix(arr, true);
  return Val_unit;
}

CAMLprim value backward(value bigarray) {
  int32_t *arr = Caml_ba_data_val(bigarray);
  // _backward(arr);
  _mix(arr, false);
  return Val_unit;
}
