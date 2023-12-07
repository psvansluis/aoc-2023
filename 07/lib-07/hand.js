//@ts-check
class Hand {
  cards;

  constructor(hand) {
    this.cards = [...hand].map(cardValue);
  }

  get value() {
    return this.comboValue + this.cardValues;
  }

  get comboValue() {
    if (this.isFiveOfKind) {
      return 6e8;
    } else if (this.isFourOfKind) {
      return 5e8;
    } else if (this.isFullHouse) {
      return 4e8;
    } else if (this.isThreeOfKind) {
      return 3e8;
    } else if (this.isTwoPair) {
      return 2e8;
    } else if (this.isOnePair) {
      return 1e8;
    }
    return 0;
  }

  get cardValues() {
    return this.cards.reduce((acc, card) => {
      return acc * 15 + card;
    }, 1);
  }

  get isFiveOfKind() {
    return this.uniqueValues === 1;
  }

  get uniqueValues() {
    return Object.values(this.occurrences).length;
  }

  get occurrences() {
    return this.cards.reduce(
      (acc, el) => ({
        ...acc,
        [el]: (acc[el] ?? 0) + 1,
      }),
      {}
    );
  }

  get isFourOfKind() {
    return Object.values(this.occurrences).includes(4);
  }

  get isThreeOfKind() {
    return Object.values(this.occurrences).includes(3);
  }

  get isFullHouse() {
    const occurrences = Object.values(this.occurrences);
    return occurrences.includes(3) && occurrences.includes(2);
  }

  get isTwoPair() {
    const occurrences = Object.values(this.occurrences);
    return occurrences.indexOf(2) !== occurrences.lastIndexOf(2);
  }

  get isOnePair() {
    const occurrences = Object.values(this.occurrences);
    return (
      occurrences.includes(2) &&
      occurrences.indexOf(2) === occurrences.lastIndexOf(2)
    );
  }
}

module.exports = Hand;

const cardValue = (card) =>
  ({ A: 14, K: 13, Q: 12, J: 11, T: 10 }[card] ?? Number(card));
