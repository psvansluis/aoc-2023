//@ts-check
const Hand = require("./hand");

class JokerHand extends Hand {
  constructor(hand) {
    super(hand, cardValueWithJoker);
  }

  get occurrences() {
    return Object.values(
      this.cards.reduce(
        (acc, el) =>
          el > 1
            ? {
                ...acc,
                [el]: (acc[el] ?? 0) + 1,
              }
            : acc,
        { 0: 0 }
      )
    );
  }

  get comboValue() {
    let sorted = this.occurrences;
    sorted.sort((a, b) => b - a);
    sorted[0] += this.nJokers;
    return sorted.reduce((acc, el) => acc + el ** 2, 0) * 1e8;
  }

  get nJokers() {
    return this.cards.filter((card) => card === 1).length;
  }
}

const cardValueWithJoker = (card) =>
  ({ A: 14, K: 13, Q: 12, J: 1, T: 10 }[card] ?? Number(card));

module.exports = JokerHand;
