import {
  JS3_capacity, JS3_test_bit, JS3_set_bit, JS3_clear_bit,
  JS3_union, JS3_popcount
} from './JS3.js';

let pass = 0, fail = 0;
function assert(cond, msg) {
  if (cond) { pass++; }
  else { fail++; console.error("FAIL:", msg); }
}

// Basic bit operations
{
  const s = new Uint8Array(64);
  assert(!JS3_test_bit(s, 0), "initially unset");
  JS3_set_bit(s, 10);
  JS3_set_bit(s, 20);
  assert(JS3_test_bit(s, 10), "10 set");
  assert(JS3_test_bit(s, 20), "20 set");
  assert(!JS3_test_bit(s, 15), "15 still unset");

  JS3_clear_bit(s, 10);
  assert(!JS3_test_bit(s, 10), "10 cleared");
  assert(JS3_test_bit(s, 20), "20 still set");
}

// Popcount
{
  const s = new Uint8Array(64);
  assert(JS3_popcount(s) === 0, "popcount empty = 0");
  JS3_set_bit(s, 0);
  JS3_set_bit(s, 31);
  JS3_set_bit(s, 63);
  assert(JS3_popcount(s) === 3, "popcount = 3");
}

// Union
{
  const a = new Uint8Array(64);
  const b = new Uint8Array(64);
  JS3_set_bit(a, 5);
  JS3_set_bit(a, 10);
  JS3_set_bit(b, 10);
  JS3_set_bit(b, 50);

  JS3_union(a, b);
  assert(JS3_test_bit(a, 5), "union has 5");
  assert(JS3_test_bit(a, 10), "union has 10");
  assert(JS3_test_bit(a, 50), "union has 50");
  assert(!JS3_test_bit(a, 0), "union missing 0");
  assert(JS3_popcount(a) === 3, "union popcount = 3");
}

// Capacity constant
{
  assert(JS3_capacity === 64, "capacity = 64");
}

console.log(`JS3: ${pass} passed, ${fail} failed`);
if (fail > 0) process.exit(1);
