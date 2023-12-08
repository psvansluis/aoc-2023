//@ts-check

const Hand = require("./hand");

class JackHand extends Hand {
  constructor(hand) {
    super(hand, cardValueWithJack);
  }

  get occurrences() {
    return Object.values(
      this.cards.reduce(
        (acc, el) => ({
          ...acc,
          [el]: (acc[el] ?? 0) + 1,
        }),
        {}
      )
    );
  }

  get comboValue() {
    return this.occurrences.reduce((acc, el) => acc + el ** 2, 0) * 1e8;
  }
}

const cardValueWithJack = (card) =>
  ({ A: 14, K: 13, Q: 12, J: 11, T: 10 }[card] ?? Number(card));

module.exports = JackHand;
