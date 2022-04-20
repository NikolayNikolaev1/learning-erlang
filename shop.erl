% Module for getting total price from list of product with quantities.
-module(shop).
-compile(export_all).
%-export([cost/1], [total/1]).

cost(oranges) -> 5;
cost(newspaper) -> 8;
cost(apples) -> 2;
cost(pears) -> 9;
cost(milk) -> 7.

total([{Product, Quantity}|T]) -> cost(Product) * Quantity + total(T);

total([]) -> 0.


