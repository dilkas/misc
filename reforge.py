import copy

def reforge(stats, target, changes = []):
    if target == 0: return changes
    if target < 0 or len(stats) == 0: return []
    for i, stat in enumerate(stats):
        result = reforge(stats[:i] + stats[i + 1:],
                         target - stat,
                         changes + [stat])
        if result != []: return result
    return []


def at_least(stats, target):

    def _at_least(stats, target, limit):
        if target > limit: return []
        result = reforge(stats, target)
        if result != []: return result
        return _at_least(stats, target + 1, limit)

    return _at_least(stats, target, sum(stats))


def up_to(stats, target):
    if target < 0: return []
    result = reforge(stats, target)
    if result != []: return result
    return up_to(stats, target - 1)


def maximize_two(stats, target1, target2):

    def value(changes):
        chamges1, changes2 = changes
        return target1 - sum(changes1) + target2 - sum(changes2)

    def minus(a, b):
        c = copy.deepcopy(a)
        d = copy.deepcopy(b)
        for e in d:
            c.remove(e)
        return c

    limit = sum(stats)
    possible_changes = []
    for t1 in range(limit):
        for t2 in range(limit):
            changes1 = reforge(stats, target1)
            if changes1 == []: continue
            changes2 = reforge(minus(stats, changes1), target2)
            if changes2 != []: possible_changes.append((changes1, changes2))
            if possible_changes: print possible_changes
    return min(possible_changes, key = value)

print at_least([], 410 - 343)
