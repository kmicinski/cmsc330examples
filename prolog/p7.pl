/*
 * Backtracking: If a goal fails, prolog goes back to its last choice
 * and looks for an alternative. In example below, we backtrack after
 * has_money(mary) fails.
 */

travel(X) :- on_vacation(X), has_money(X).

on_vacation(mary).
on_vacation(peter).
has_money(peter).

    

