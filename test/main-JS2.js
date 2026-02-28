import {
  JS2_get_left, JS2_get_right, JS2_swap_at, JS2_merge
} from './JS2.js';

let pass = 0, fail = 0;
function assert(cond, msg) {
  if (cond) { pass++; }
  else { fail++; console.error("FAIL:", msg); }
}

// Struct field access
{
  const p = { left: new Uint32Array(4), right: new Uint32Array(4) };
  p.left[0] = 10; p.left[1] = 20;
  p.right[0] = 30; p.right[1] = 40;

  assert(JS2_get_left(p, 0) === 10, "get_left [0]");
  assert(JS2_get_right(p, 1) === 40, "get_right [1]");
}

// Swap
{
  const p = { left: new Uint32Array(4), right: new Uint32Array(4) };
  p.left[2] = 100; p.right[2] = 200;
  JS2_swap_at(p, 2);
  assert(JS2_get_left(p, 2) === 200, "swap left got right's value");
  assert(JS2_get_right(p, 2) === 100, "swap right got left's value");
}

// Merge (max of left and right into left)
{
  const p = {
    left:  new Uint32Array([1, 5, 3, 0]),
    right: new Uint32Array([4, 2, 6, 1])
  };
  JS2_merge(p);
  assert(JS2_get_left(p, 0) === 4, "merge [0] = max(1,4)");
  assert(JS2_get_left(p, 1) === 5, "merge [1] = max(5,2)");
  assert(JS2_get_left(p, 2) === 6, "merge [2] = max(3,6)");
  assert(JS2_get_left(p, 3) === 1, "merge [3] = max(0,1)");
  // right unchanged
  assert(JS2_get_right(p, 0) === 4, "right unchanged [0]");
}

console.log(`JS2: ${pass} passed, ${fail} failed`);
if (fail > 0) process.exit(1);
