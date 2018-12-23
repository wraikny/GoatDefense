module GoatDefense.Core.Helper.Math

module Angle =
  let pi = float32 System.Math.PI

  let radToDeg r =
    r / pi * 180.0f

  let degToRad d =
    d / 180.0f * pi


/// 二次元ベクトルを表す型。
type ^a Vec2 =
  { x : ^a; y : ^a }

  static member inline (~-) a =
    { x = -a.x; y = -a.y }

  static member inline (+) (a, b) =
    { x = a.x + b.x; y = a.y + b.y }

  static member inline (-) (a, b) =
    { x = a.x - b.x; y = a.y - b.y }

  static member inline (*) (a, b) =
    { x = a.x * b; y = a.y * b }

  static member inline (*) (a, b) =
    { x = a * b.x; y = a * b.y }

  static member inline (/) (a, b) =
    { x = a.x / b; y = a.y / b }


module Vec2 =
  let inline init(x, y) : ^a Vec2 =
    { x = x; y = y }

  let inline zeros() : ^a Vec2 =
    let zero = LanguagePrimitives.GenericZero
    init(zero, zero)

  let inline squaredLength(v : ^a Vec2) : ^a =
    v.x ** 2.0f + v.y ** 2.0f

  let inline length (v : ^a Vec2) : ^a =
    (squaredLength v) ** 0.5f

  let inline normalize(v : ^a Vec2) : ^a Vec2 option =
    let q = (length v)
    try
      v * (q ** -1.0f)
      |> Some
    with
    | _ ->
      None

  let inline angle (v : float32 Vec2) : float32 option =
    if v.x <> 0.0f then
      let angle =
        atan (v.y / v.x)
        +
        if (float32 v.x > 0.0f) then 0.0f
        else Angle.pi

      Some angle
    elif v.y > 0.0f then
      Some (Angle.pi * 0.5f)
    elif v.y < 0.0f then
      Some (Angle.pi * 1.5f)
    else
      None