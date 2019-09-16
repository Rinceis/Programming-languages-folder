cylinder radius height =
  let circleArea = 2 * pi * radius * radius
  sideArea = 2 * pi * radius * radius
  in
      circleArea + sideArea


cy1 radius height = circleArea + sideArea
where
  circleArea = 2* pi * radius * radius
  sideArea = 2 * pi * radius * height
