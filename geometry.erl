% Module for getting the area of rectangle, square and circle.
-module(geometry).
-export([area/1]).

area({rectangle, Width, Height}) -> Width * Height;

area({square, Width}) -> Width * Width;

area({circle, Radius}) -> 3.14159 * Radius * Radius.