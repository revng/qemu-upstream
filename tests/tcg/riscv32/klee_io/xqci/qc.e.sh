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
    in: 67108863
  - name: "rs2"
    in: 0
  - name: "rs1"
    in: 4228120576
  overflow: 1
  underflow: 0
  has_valid_test_memop: 0
  has_store:
- variables:
  - name: "imm"
    in: 3015
  - name: "rs2"
    in: 4294902015
  - name: "rs1"
    in: 4096
  overflow: 0
  underflow: 0
  has_valid_test_memop: 1
  has_store:
  - address: 7111
    value: 255
- variables:
  - name: "imm"
    in: 50344951
  - name: "rs2"
    in: 4294902015
  - name: "rs1"
    in: 4244626444
  overflow: 1
  underflow: 0
  has_valid_test_memop: 1
  has_store:
  - address: 4099
    value: 255
