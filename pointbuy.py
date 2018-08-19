#!/usr/bin/env python3
"""D&D 5E point buy script by Iguanotron, August 2018.

Generates random ability scores with constant point-buy cost.

By default, the user is prompted to select from several stat arrays.
Their final ability scores are then generated in a random order from
that array.
"""

import argparse
import functools
import random


DEFAULT_POINTS = 27
DEFAULT_NUM_CHOICES = 3

STATS_BY_COST = {
    0: 8,
    1: 9,
    2: 10,
    3: 11,
    4: 12,
    5: 13,
    7: 14,
    9: 15,
}

COSTS = sorted(STATS_BY_COST.keys(), reverse=True)

ABILITIES = (
    'STR',
    'DEX',
    'CON',
    'INT',
    'WIS',
    'CHA',
)


@functools.lru_cache(maxsize=128)  # Cache should grow to <=84 in all cases
def cost_spreads_of(n, total, max_cost):
    """Return a list of all spreads of n costs summing to exactly total."""
    if n == 0:
        return [()]
    if total == 0:
        return [(0,) * n]

    return [(cost, *spread)
            for cost in COSTS
            if cost <= total and cost <= max_cost and total <= n * cost
            for spread in cost_spreads_of(n - 1, total - cost, cost)]


def best_cost_spread_of(n, total):
    """Return the best spread on n costs summing to at most total."""
    spread = []
    while n > 0 and total > 0:
        for cost in COSTS:
            if cost <= total:
                spread.append(cost)
                n -= 1
                total -= cost
                break
    return spread


def list_arrays(all_cost_spreads):
    """List all stat arrays."""
    for spread in all_cost_spreads:
        print('%2d %2d %2d %2d %2d %2d' % tuple(STATS_BY_COST[cost] for cost in spread))


def generate_scores(all_cost_spreads, n=1):
    """Generate n random stat arrays."""
    if n > len(all_cost_spreads):
        all_cost_spreads *= -(-n // len(all_cost_spreads))
    arrays = [[STATS_BY_COST[cost] for cost in spread]
              for spread in random.sample(all_cost_spreads, n)]
    print(' %3s %3s %3s %3s %3s %3s' % ABILITIES)
    for array in arrays:
        random.shuffle(array)
        print(' %3d %3d %3d %3d %3d %3d' % tuple(array))


def choose_scores(all_cost_spreads, num_choices=DEFAULT_NUM_CHOICES):
    """Guided process of choosing a stat array.

    User picks from several random arrays, then their choice
    is scrambled and randomly assigned to actual abilities.
    """
    num_choices = min(num_choices, len(all_cost_spreads))
    
    arrays = [[STATS_BY_COST[cost] for cost in spread]
              for spread in sorted(random.sample(all_cost_spreads, num_choices), reverse=True)]

    print('\nChoose an array:\n')
    for i, arr in enumerate(arrays):
        print('  %d)  %2d %2d %2d %2d %2d %2d' % (i + 1, *arr))
    print()

    while True:
        answer = input('> ')
        try:
            selection = int(answer) - 1
        except ValueError:
            print('Error: expected a number between 1 and %d' % num_choices)
            continue
        if selection < 0 or selection >= num_choices:
            print('Error: expected a number between 1 and %d' % num_choices)
            continue
        break

    array = arrays[selection]
    random.shuffle(array)
    
    print('\nAbility Scores:\n')
    for i, stat in enumerate(ABILITIES):
        print('  %3s  %2d' % (stat, array[i]))
    print()


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-p', '--points',
                        type=int,
                        metavar='n',
                        default=DEFAULT_POINTS,
                        help='total points available (default %d)' % DEFAULT_POINTS)
    parser.add_argument('-c', '--choices',
                        type=int,
                        metavar='n',
                        help='choose from n arrays (default %d)' % DEFAULT_NUM_CHOICES)
    parser.add_argument('-g', '--generate',
                        type=int,
                        metavar='n',
                        help='generate n random ability score arrays')
    parser.add_argument('-l', '--list',
                        action='store_true',
                        help='list all valid point-buy arrays')
    args = parser.parse_args()


    if sum([args.choices is not None, args.generate is not None, args.list]) > 1:
        parser.error('incompatible arguments: only one of -c, -g, and -l accepted at a time')
    if args.points < 0:
        parser.error('invalid point pool size (must be nonnegative)')

    all_cost_spreads = cost_spreads_of(6, args.points, max(COSTS))

    if not all_cost_spreads:
        all_cost_spreads = [best_cost_spread_of(6, args.points)]

    if args.list:
        list_arrays(all_cost_spreads)
    elif args.generate is not None:
        generate_scores(all_cost_spreads, args.generate)
    elif args.choices is not None:
        if args.choices <= 0:
            parser.error('invalid number of choices (must be positive)')
        choose_scores(all_cost_spreads, args.choices)
    else:
        choose_scores(all_cost_spreads)
