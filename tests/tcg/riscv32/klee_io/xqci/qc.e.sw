- variables:
  - name: "imm"
    in: 0
  - name: "rs2"
    in: 0
  - name: "rs1"
    in: 0
  overflow: 0
  underflow: 0
  has_valid_test_memop: 0
  has_store:
- variables:
  - name: "imm"
    in: 8191
  - name: "rs2"
    in: 0
  - name: "rs1"
    in: 4100
  overflow: 0
  underflow: 0
  has_valid_test_memop: 0
  has_store:
- variables:
  - name: "imm"
    in: 33554688
  - name: "rs2"
    in: 0
  - name: "rs1"
    in: 33563136
  overflow: 0
  underflow: 0
  has_valid_test_memop: 0
  has_store:
- variables:
  - name: "imm"
    in: 33554688
  - name: "rs2"
    in: 16711680
  - name: "rs1"
    in: 33563136
  overflow: 0
  underflow: 0
  has_valid_test_memop: 1
  has_store:
  - address: 8960
    value: 16711680
    size: 32
- variables:
  - name: "imm"
    in: 16797984
  - name: "rs2"
    in: 16711680
  - name: "rs1"
    in: 4278177568
  overflow: 1
  underflow: 0
  has_valid_test_memop: 1
  has_store:
  - address: 8256
    value: 16711680
    size: 32
