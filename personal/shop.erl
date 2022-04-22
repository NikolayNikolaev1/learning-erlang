% Module for getting total price from list of product with quantities.
-module(shop).
-import(mylists, [map/2, sum/1]).
-export([cost/1, total/1]).
%-compile(export_all).

cost(apples) -> 2;
cost(milk) -> 7;
cost(newspaper) -> 8;
cost(oranges) -> 5;
cost(pears) -> 9.

%total([{Product, Quantity}|T]) -> cost(Product) * Quantity + total(T);
%total([]) -> 0.

% Maps the product cost  times the quantity and returns their sum.
%total(Products)
%	-> sum(map(fun({Product, Quantity}) -> cost(Product) * Quantity end, Products)).

% Sum product total prices with list comprehension.
total(Products)
	-> sum([cost(Product) * Quantity || {Product, Quantity} <- Products]).

