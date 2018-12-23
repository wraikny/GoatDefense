[<AutoOpen>]
module GoatDefense.Core.Helper.Alias

open GoatDefense.Core.Helper.Math

/// Viewと紐つけるための型。
type ID = int
module ID =
  let zero : ID = LanguagePrimitives.GenericZero
  let one : ID = LanguagePrimitives.GenericOne

/// Actorの種類を管理するための型。
type ActorType = string

/// ゲーム内で使用する距離を表す型。
type Distance = float32
module Distance =
  let zero : Distance = LanguagePrimitives.GenericZero
  let one : Distance = LanguagePrimitives.GenericOne

/// ゲーム内で使用する座標を表す型。
type Coordinate = Distance Vec2
module Coordinate =
  let zeros : Coordinate = Vec2.zeros()

/// フィールドなどの大きさを表す型。
type Size = int Vec2
module Size =
  let zeros : Size = Vec2.zeros()

/// 時間を測るための型。
type Count = int
module Count =
  let zero : Count = LanguagePrimitives.GenericZero
  let one : Count = LanguagePrimitives.GenericOne


///// プレイヤーをチーム分けするための型s
//type Team = int


///// Actor等の作成コストを表す型。
//type Cost = uint32

///// Statusを表すための型。
//type StatusValue = uint32
//module StatusValue =
//  let zero : StatusValue = LanguagePrimitives.GenericZero
//  let one : StatusValue = LanguagePrimitives.GenericOne