- variables:
  - name: "imm"
    in: 67108863
  - name: "rs2"
    in: 0
  - name: "rs1"
    in: 4227858448
  overflow: 1
  underflow: 0
  has_valid_test_memop: 0
  has_store:
- variables:
  - name: "imm"
    in: 12313
  - name: "rs2"
    in: 16711680
  - name: "rs1"
    in: 4294959079
  overflow: 1
  underflow: 0
  has_valid_test_memop: 1
  has_store:
  - address: 4096
    value: 16711680
- variables:
  - name: "imm"
    in: 5215
  - name: "rs2"
    in: 16711680
  - name: "rs1"
    in: 0
  overflow: 0
  underflow: 0
  has_valid_test_memop: 1
  has_store:
  - address: 5215
    value: 16711680
