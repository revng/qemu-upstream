- variables:
  - name: "shamt"
    in: 0
  - name: "rs1"
    in: 0
  - name: "rs2"
    in: 255
  - name: "rs3"
    in: 0
  overflow: 0
  underflow: 0
  has_valid_test_memop: 0
  has_store:
- variables:
  - name: "shamt"
    in: 3
  - name: "rs1"
    in: 5368
  - name: "rs2"
    in: 255
  - name: "rs3"
    in: 4278190080
  overflow: 0
  underflow: 0
  has_valid_test_memop: 1
  has_store:
  - address: 7408
    value: 4278190080
- variables:
  - name: "shamt"
    in: 3
  - name: "rs1"
    in: 4294961272
  - name: "rs2"
    in: 536872180
  - name: "rs3"
    in: 4278190080
  overflow: 1
  underflow: 0
  has_valid_test_memop: 1
  has_store:
  - address: 4120
    value: 4278190080
- variables:
  - name: "shamt"
    in: 5
  - name: "rs1"
    in: 804596192
  - name: "rs2"
    in: 109074304
  - name: "rs3"
    in: 4278190080
  overflow: 1
  underflow: 0
  has_valid_test_memop: 1
  has_store:
  - address: 6624
    value: 4278190080
- variables:
  - name: "shamt"
    in: 1
  - name: "rs1"
    in: 3902
  - name: "rs2"
    in: 2147485696
  - name: "rs3"
    in: 4278190080
  overflow: 1
  underflow: 0
  has_valid_test_memop: 1
  has_store:
  - address: 7998
    value: 4278190080
