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
    in: 4261412864
  overflow: 1
  underflow: 0
  has_valid_test_memop: 0
  has_store:
- variables:
  - name: "imm"
    in: 1007
  - name: "rs2"
    in: 65280
  - name: "rs1"
    in: 4096
  overflow: 0
  underflow: 0
  has_valid_test_memop: 1
  has_store:
  - address: 5103
    value: 65280
- variables:
  - name: "imm"
    in: 52440924
  - name: "rs2"
    in: 65280
  - name: "rs1"
    in: 4242530480
  overflow: 1
  underflow: 0
  has_valid_test_memop: 1
  has_store:
  - address: 4108
    value: 65280
