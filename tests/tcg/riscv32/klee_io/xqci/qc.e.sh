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
    in: 4095
  - name: "rs2"
    in: 0
  - name: "rs1"
    in: 4294963202
  overflow: 1
  underflow: 0
  has_valid_test_memop: 0
  has_store:
- variables:
  - name: "imm"
    in: 24158199
  - name: "rs2"
    in: 0
  - name: "rs1"
    in: 4270817280
  overflow: 1
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
    in: 24158199
  - name: "rs2"
    in: 4294902015
  - name: "rs1"
    in: 4270817280
  overflow: 1
  underflow: 0
  has_valid_test_memop: 1
  has_store:
  - address: 8183
    value: 255
    size: 16
- variables:
  - name: "imm"
    in: 4059
  - name: "rs2"
    in: 4294902015
  - name: "rs1"
    in: 4096
  overflow: 0
  underflow: 0
  has_valid_test_memop: 1
  has_store:
  - address: 8155
    value: 255
    size: 16
