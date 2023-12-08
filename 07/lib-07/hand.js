//@ts-check
class Hand {
  cards;

  constructor(hand, mapping) {
    this.cards = [...hand].map(mapping);
  }

  get value() {
    return this.comboValue + this.cardValues;
  }

  get comboValue() {
    throw new Error("let children define this");
    return 0;
  }

  get cardValues() {
    return this.cards.reduce((acc, card) => acc * 15 + card, 1);
  }
}

module.exports = Hand;
