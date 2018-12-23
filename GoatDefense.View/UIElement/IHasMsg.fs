[<AutoOpen>]
module GoatDefense.View.UIElement.IHasMsg


[<Interface>]
type IHasMsg<'Msg> =
  abstract Msg : 'Msg