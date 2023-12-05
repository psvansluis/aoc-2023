import { assert } from "chai";

describe("addition", () => {
  it("1+1=2", () => {
    const result = 1 + 1;
    const expected = 2;
    assert.equal(result, expected);
  });

  it("fails", () => {
    assert.isTrue(false);
  });
});
