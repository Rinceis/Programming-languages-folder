module A1 where
-- | The purpose of this first function is to return the value that is given when given one has 4 num parameters, the first parameter
-- | is added to the second parameter and fourth parameter multiplied together, which in turn is added to the third
-- |parameter times the fourth parameter squared. It seeks to emulate a quadratic equation, and returns the result of a basic quadratic
-- |equation given those four parameters as variables.

quadratic a b c x = a + (b*x) + (c * x^2)
-- | returns a num value

-- |the purpose of the second function, which takes in a value and a 2-tuple, is to scale a certain point on a vector by the given value,
-- | presumably to accurately find the the new point reached after the distance is multiplied in multiple directions. it returns a new 2-tuple.

scaleVector n1 (n2, n3) = ((n1*n2), (n1*n3))
-- | returns a 2-tuple

-- |the third function takes in two 3-tuples as the x, y, and z coordinates for two different points, and it's purpose is to find the cartesian distance
-- |between these two points. after these calculations, we are given a floating point value, the distance between the two points.

tripleDistance (x1, y1, z1) (x2, y2, z2) = sqrt((x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2)
-- |returns a floating point
