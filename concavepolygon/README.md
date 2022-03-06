# Concave Polygon

https://www.hackerrank.com/challenges/lambda-march-concave-polygon/problem

## Notes

Very similar to convex hull algorithm.
Basically if something is not convex, it is concave. So I reused the convex hull algorithm to find the convex hull then compare the number of points with the input

If input is the same, then it is convex, if not the same, then it is concave.

However, the trick here is, any collinear points on the edge, I need to include as well.


