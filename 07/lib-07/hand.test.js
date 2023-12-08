const JokerHand = require("./jokerHand");

const comboValue5OfKind = 2500000000;
test("5 of kind", () => {
  expect(new JokerHand("JJJJJ").comboValue).toBe(comboValue5OfKind);
});
